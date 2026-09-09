#!/usr/bin/env python
# code/09_policyengine_te.py
# TE Stage 3 (PolicyEngine engine): run TY(REF_YEAR) tax units through PolicyEngine-US under the same
# three arc scenarios as the reference TAXSIM implementation and write a schema-identical
# taxsim_results.parquet, so the
# downstream CBO present-value wrapper (10) and everything after it are engine-agnostic.
# Invoked by:  code/09_tax_engine.R  (dispatcher; TAX_ENGINE=="policyengine") via system2(PYTHON_BIN,...)
# Input :  data/processed/tax_units.parquet   (engine-neutral frame built by 08_tax_units.R)
# Output:  data/processed/taxsim_results.parquet   (unit-level scenario taxes + rates)
#
# Method = faithful port of the TAXSIM WAGE-SUBSTITUTION arc (SIPP earnings ASSUMED gross of
# employee elective deferrals; verified in params.R). PolicyEngine is the tax function; the contribution
# algebra stays in the wage vectors exactly as TAXSIM saw them, so the two engines are directly
# comparable and the employer leg (adds employer dollars to wages) works identically:
#   base   : pw = max(0, pwages - c_ee_prim), sw = max(0, swages - c_ee_sp)   -> deferrals excluded
#   cf_ee  : pw = pwages,                     sw = swages                     -> employee exclusion repealed
#   cf_all : pw = pwages + c_er_prim,         sw = swages + c_er_sp           -> + employer exclusion repealed
#   te_ee_arc = fiitax(cf_ee)  - fiitax(base);  te_er_arc = fiitax(cf_all) - fiitax(cf_ee)
# Federal-only headline (matches 09's state=0): siitax_*/srate_*/te_*_arc_st are written as 0.
# MTR (frate_*) via a +$DELTA finite difference on the primary's employment_income in each scenario.

import argparse, sys, time
import importlib.metadata
import numpy as np
import pandas as pd

try:
    from policyengine_us import Simulation
except Exception as e:  # pragma: no cover - install/compat failure is a hard stop
    sys.stderr.write("09_policyengine_te.py: cannot import policyengine_us: %r\n" % (e,))
    sys.exit(2)


def assert_version(expected):
    """The 'pinned' version in params.R is a claim, not a mechanism, unless enforced here.
    A silent `pip install -U` would otherwise change every tax number while the constant, the log
    line, and the draft footnote keep asserting the old version."""
    if not expected:
        return
    installed = importlib.metadata.version("policyengine_us")
    if installed != expected:
        sys.stderr.write(
            "09_policyengine_te.py: policyengine-us version mismatch: installed %s, pinned %s "
            "(params.R POLICYENGINE_VERSION). Reinstall the pinned version or update the pin "
            "(and the draft footnote) deliberately.\n" % (installed, expected))
        sys.exit(3)

# MTR direction: a FORWARD finite difference -- the rate on the NEXT $1,000, not on the last $1,000
# already earned. Deliberate (external RA review, 2026-08-27): frate_base is applied downstream (10)
# to inside buildup and to future withdrawals, i.e. to income ADDED on top of what the unit already
# has, so the next-dollar rate is the correct rate; and a backward difference is UNDEFINED for the
# 42.6% of units whose base wages are under $1,000. Measured sensitivity of the CBO-comparable
# income-tax TE: backward -$1,000 = +0.06%, central +/-$500 = +1.18% -- against a 3.5%-vs-6%
# discount-rate spread an order of magnitude larger. Contribution-weighted mean MTR: forward
# 20.94%, backward 21.51%, central 20.81%.
MTR_DELTA = 1000.0  # dollars added to primary employment_income for the marginal-rate finite difference

# income components mapped once (TAXSIM field -> PolicyEngine person variable). Wages are set per scenario.
PRIMARY_INCOME = {                # non-wage income assigned to the PRIMARY person
    "psemp":     "self_employment_income",
    "intrec":    "taxable_interest_income",
    "dividends": "qualified_dividend_income",
    "otherprop": "rental_income",
    "pensions":  "taxable_pension_income",
    "gssi":      "social_security_retirement",
    "pui":       "unemployment_compensation",
}
SPOUSE_INCOME = {                 # income assigned to the SPOUSE person (married units only)
    "ssemp":     "self_employment_income",
    "sui":       "unemployment_compensation",
}


def build_situation(chunk, year, pw, sw):
    """One PolicyEngine situation dict for a chunk of tax units.

    chunk : DataFrame slice of tax_units (one row per filing unit)
    pw,sw : arrays (len == len(chunk)) of PRIMARY / SPOUSE employment income for the scenario
    Returns (situation, order) where order is the taxsimid list in tax_unit insertion order.
    """
    y = int(year)
    people, order = {}, []
    fam, mar, tax, spm, hh = {}, {}, {}, {}, {}
    for i, (_, row) in enumerate(chunk.iterrows()):
        tid = int(row["taxsimid"])
        order.append(tid)
        married = str(row["mstat"]).strip() == "married, jointly"
        pkey = f"u{tid}_p"
        members = [pkey]
        prec = {"age": {y: int(row["page"]) if row["page"] and int(row["page"]) > 0 else 40},
                "employment_income": {y: float(pw[i])}}
        for src, dst in PRIMARY_INCOME.items():
            v = row.get(src, 0)
            if v and float(v) != 0.0:
                prec[dst] = {y: float(v)}
        people[pkey] = prec
        mar_members = [pkey]
        if married:
            skey = f"u{tid}_s"
            members.append(skey)
            mar_members.append(skey)
            srec = {"age": {y: int(row["sage"]) if row["sage"] and int(row["sage"]) > 0 else 40},
                    "employment_income": {y: float(sw[i])}}
            for src, dst in SPOUSE_INCOME.items():
                v = row.get(src, 0)
                if v and float(v) != 0.0:
                    srec[dst] = {y: float(v)}
            people[skey] = srec
        # ALL dependents, from 08's dep_ages (";"-joined observed ages; age 0 = a real infant).
        # The former age1-3 loop capped units at three children and its `int(a) > 0` guard
        # dropped genuine age-0 dependents; age1-3 remain TAXSIM-only fields.
        da = row.get("dep_ages", "")
        if da is None or (isinstance(da, float) and np.isnan(da)):
            da = ""
        ages = [int(float(t)) for t in str(da).split(";") if t.strip() != ""]
        if len(ages) != int(row["depx"]):
            raise RuntimeError(f"unit {tid}: dep_ages lists {len(ages)} dependents, depx={int(row['depx'])}")
        for j, a in enumerate(ages):
            ckey = f"u{tid}_k{j+1}"
            people[ckey] = {"age": {y: a}}
            members.append(ckey)
        # federal-only: state is irrelevant to income_tax. 08 leaves a few units' state = NA
        # (empty TST_INTV / Type-2 noninterview); map any missing/invalid to a valid placeholder.
        st = row.get("state", None)
        if st is None or (isinstance(st, float) and np.isnan(st)) or str(st).strip() in ("", "nan", "NA", "None"):
            st = "TX"
        else:
            st = str(st).strip()
        fam[f"f{tid}"] = {"members": list(members)}
        mar[f"m{tid}"] = {"members": mar_members}
        tax[f"t{tid}"] = {"members": list(members)}
        spm[f"s{tid}"] = {"members": list(members)}
        hh[f"h{tid}"] = {"members": list(members), "state_name": {y: st}}
    situation = {"people": people, "families": fam, "marital_units": mar,
                 "tax_units": tax, "spm_units": spm, "households": hh}
    return situation, order


def fed_tax(chunk, year, pw, sw):
    """Vectorized federal income tax per tax unit for one scenario over a chunk. Returns array aligned
    to chunk row order."""
    situation, order = build_situation(chunk, year, pw, sw)
    sim = Simulation(situation=situation)
    vals = np.asarray(sim.calculate("income_tax", int(year)), dtype=float)
    if len(vals) != len(order):
        raise RuntimeError(f"income_tax length {len(vals)} != n_units {len(order)} in chunk")
    # calculate() returns tax-unit values in insertion order == chunk row order
    return vals


def run(in_path, out_path, chunk_size, limit, oasdi_cap):
    t_all = time.time()
    u = pd.read_parquet(in_path)
    if limit:
        u = u.head(int(limit)).copy()
    n = len(u)
    year = int(u["year"].iloc[0])
    if u["year"].nunique() != 1:
        sys.stderr.write("09_policyengine_te.py: WARNING multiple 'year' values in tax_units; using per-chunk.\n")
    print(f"09_pe: {n} units, tax year {year}, chunk_size {chunk_size}", flush=True)

    # scenario wage vectors (mirror the reference TAXSIM implementation exactly)
    pw_base = np.maximum(0.0, u["pwages"].to_numpy() - u["c_ee_prim"].to_numpy())
    sw_base = np.maximum(0.0, u["swages"].to_numpy() - u["c_ee_sp"].to_numpy())
    pw_ee, sw_ee = u["pwages"].to_numpy(float), u["swages"].to_numpy(float)
    pw_all = u["pwages"].to_numpy(float) + u["c_er_prim"].to_numpy(float)
    sw_all = u["swages"].to_numpy(float) + u["c_er_sp"].to_numpy(float)
    trunc_ee = float(np.sum((u["c_ee_prim"] - u["pwages"])[u["c_ee_prim"] > u["pwages"]]) +
                     np.sum((u["c_ee_sp"] - u["swages"])[u["c_ee_sp"] > u["swages"]]))

    scen = {
        "base":   (pw_base, sw_base),
        "cf_ee":  (pw_ee,   sw_ee),
        "cf_all": (pw_all,  sw_all),
    }
    out = {k: np.full(n, np.nan) for k in
           ("fiitax_base", "fiitax_cf_ee", "fiitax_cf_all",
            "frate_base", "frate_cf_ee", "frate_cf_all")}

    n_chunks = int(np.ceil(n / chunk_size))
    for ci in range(n_chunks):
        lo, hi = ci * chunk_size, min((ci + 1) * chunk_size, n)
        chunk = u.iloc[lo:hi]
        for nm, (pw, sw) in scen.items():
            base = fed_tax(chunk, year, pw[lo:hi], sw[lo:hi])
            out[f"fiitax_{nm}"][lo:hi] = base
            bumped = fed_tax(chunk, year, pw[lo:hi] + MTR_DELTA, sw[lo:hi])
            out[f"frate_{nm}"][lo:hi] = (bumped - base) / MTR_DELTA * 100.0
        if (ci + 1) % 5 == 0 or ci == n_chunks - 1:
            print(f"09_pe: chunk {ci+1}/{n_chunks} done ({hi}/{n} units, "
                  f"{time.time()-t_all:.0f}s)", flush=True)

    res = pd.DataFrame({
        "filing_unit_id": u["filing_unit_id"].to_numpy(),
        "taxsimid": u["taxsimid"].to_numpy(),
        "mstat": u["mstat"].to_numpy(),
        "depx": u["depx"].to_numpy(),
        "wgt_unit": u["wgt_unit"].to_numpy(),
        "c_ee_401": u["c_ee_401"].to_numpy(), "c_ee_ira": u["c_ee_ira"].to_numpy(),
        "c_ee_pen": u["c_ee_pen"].to_numpy(), "c_er_401": u["c_er_401"].to_numpy(),
        "c_er_ira": u["c_er_ira"].to_numpy(),
        "c_ee_prim": u["c_ee_prim"].to_numpy(), "c_ee_sp": u["c_ee_sp"].to_numpy(),
        "c_er_prim": u["c_er_prim"].to_numpy(), "c_er_sp": u["c_er_sp"].to_numpy(),
        "bal_ret": u["bal_ret"].to_numpy(), "wd_ret": u["wd_ret"].to_numpy(),
        "pensions": u["pensions"].to_numpy(), "page": u["page"].to_numpy(),
        "fiitax_base": out["fiitax_base"], "siitax_base": 0.0,
        "frate_base": out["frate_base"], "srate_base": 0.0,
        # ficar carried for schema parity ONLY; downstream 10 computes its own FICA wedge from
        # params. The cap flows from params.R via --oasdi-cap (no duplicated vintage
        # literal here); the 15.3/2.9 combined statutory rates are year-invariant.
        "ficar_base": np.where(pw_base < float(oasdi_cap), 15.3, 2.9),
        "fiitax_cf_ee": out["fiitax_cf_ee"], "siitax_cf_ee": 0.0,
        "frate_cf_ee": out["frate_cf_ee"], "srate_cf_ee": 0.0, "ficar_cf_ee": 15.3,
        "fiitax_cf_all": out["fiitax_cf_all"], "siitax_cf_all": 0.0,
        "frate_cf_all": out["frate_cf_all"], "srate_cf_all": 0.0, "ficar_cf_all": 15.3,
    })
    res["te_ee_arc"] = res["fiitax_cf_ee"] - res["fiitax_base"]
    res["te_er_arc"] = res["fiitax_cf_all"] - res["fiitax_cf_ee"]
    res["te_ee_arc_st"] = 0.0
    res["te_er_arc_st"] = 0.0

    res.to_parquet(out_path, index=False)

    has_c = (res["c_ee_401"] + res["c_ee_ira"] + res["c_ee_pen"] +
             res["c_er_401"] + res["c_er_ira"]) > 0
    w = res["wgt_unit"].to_numpy()
    print("\n================ 09 POLICYENGINE: REVIEW ================", flush=True)
    print(f"Engine: PolicyEngine-US (pinned) | units: {n} | with contributions: {int(has_c.sum())}")
    print(f"Truncated employee-contribution dollars (contrib > measured wages): ${round(trunc_ee):,}")
    q = np.nanpercentile(res["frate_base"], [5, 25, 50, 75, 95])
    print("frate_base distribution (5/25/50/75/95): " + " ".join(f"{v:.1f}" for v in q))
    arc = (res["te_ee_arc"] + res["te_er_arc"]).to_numpy()
    print(f"Arc TE (income tax), units w/ contributions: mean ${round(np.average(arc[has_c], weights=w[has_c]))}"
          f" | negative-arc units: {int((arc[has_c] < 0).sum())}")
    print(f"Weighted aggregate arc TE ($B): employee "
          f"{np.sum(res['te_ee_arc'].to_numpy() * w)/1e9:.1f} + employer "
          f"{np.sum(res['te_er_arc'].to_numpy() * w)/1e9:.1f}")
    print(f"\nWrote {out_path}  ({time.time()-t_all:.0f}s total)", flush=True)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="in_path", required=True)
    ap.add_argument("--out", dest="out_path", required=True)
    ap.add_argument("--chunk", dest="chunk_size", type=int, default=1000)
    ap.add_argument("--limit", dest="limit", type=int, default=0)
    ap.add_argument("--expect-version", dest="expect_version", default="",
                    help="fail loud unless installed policyengine-us equals this (params.R pin)")
    ap.add_argument("--oasdi-cap", dest="oasdi_cap", type=float, default=168600.0,
                    help="OASDI taxable maximum from params.R (schema-parity ficar column only)")
    a = ap.parse_args()
    assert_version(a.expect_version)
    run(a.in_path, a.out_path, a.chunk_size, a.limit, a.oasdi_cap)
