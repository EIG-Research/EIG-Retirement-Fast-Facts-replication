# Derived metric definitions & justification (`code/_shared`)

This documents exactly how the SIPP Fast Facts pillar metrics are constructed in `01_build_dataset.R`,
using the **Census-verbatim** variable definitions, universes, and value codes from the
[2025 SIPP Data Dictionary](https://www.census.gov/programs-surveys/sipp/tech-documentation/complete-documents.html).
All items code `1=Yes / 2=No / -9=NA`; all measured for `MONTHCODE = 12` (December reference month,
reference year 2024). Estimates weighted by `WPFINWGT`. Working population: ages 18–64 with an active
December job (`EJB1_JBORSE ∈ {1,2,3}`); N = 12,330 ≈ **147.3M** weighted. **Civilian labor force**
(2026-07-16): households containing any active-duty military member are dropped (`EXCLUDE_MILITARY` in
`params.R`); the headline numbers below were refreshed for this frame (was 12,423 ≈ 148.6M all-worker,
and 12,341 ≈ 147.5M under the pre-2026-08-27 coalesce-based military test).

---

## 1. Access — the headline metric

### Building blocks (verbatim)
- **`EMJOB_401 / EMJOB_IRA / EMJOB_PEN`** — "Any [401k/403b/503b/TSP | IRA/Keogh | DB/cash-balance]
  account(s) **provided through main employer or business** during the reference period." Universe:
  the corresponding `EOWN_* = 1` (owns that account type) **and** held a December job.
- **`EPENSNYN`** — "Did … main employer or business **have any kind of pension or retirement plans**
  for anyone in the company or organization?" Universe (verbatim): age 15+ with a December job **and**
  `EMJOB_IRA in (2,.) and EMJOB_401 in (2,.) and EMJOB_PEN in (2,.)` — i.e., people who did **not**
  already report a plan through their main employer.
- **`EINCPENS`** — "Was … **included in the pension or retirement plan(s) offered** by … main employer
  or business?" Universe (verbatim): `EPENSNYN = 1`.

### The skip pattern (why the metric is a union)
SIPP routes people down two mutually exclusive branches:
- **Branch A — already in an employer plan:** `EMJOB_* = 1`. These respondents are **skipped out** of
  `EPENSNYN`/`EINCPENS`. Offer+inclusion is true by construction but recorded only in `EMJOB_*`.
- **Branch B — not currently in an employer plan:** asked `EPENSNYN`; if `= 1`, asked `EINCPENS`.
  `EINCPENS = 1` = offered **and** included/eligible.

A person who owns a DC/IRA account that is **not** through their current employer (e.g., an old-job
401(k)) answers `EMJOB_* = 2` and is therefore still asked `EPENSNYN`/`EINCPENS` — so their access is
correctly judged on their **current** employer's offer + inclusion, not the legacy account.

### Constructions (`01_build_dataset.R`)
```r
emp_acct       <- is_yes(EMJOB_401) | is_yes(EMJOB_IRA) | is_yes(EMJOB_PEN)   # Branch A
incl           <- is_yes(EINCPENS)                                            # Branch B: offered & INCLUDED
offered        <- is_yes(EPENSNYN)                                            # employer offers (any inclusion)
access_emp_raw <- emp_acct | incl                                             # "offered AND eligible"
access_emp     <- ifelse(is_self_employed, FALSE, access_emp_raw)             # SE forced to no employer access
lacks_access   <- !access_emp                                                 # HEADLINE (H1)
```
- **H1 "offered AND eligible"** (`access_emp`) is the headline. **Lacks access** = holds no employer plan
  **and** (`EPENSNYN = 2` **or** `EINCPENS = 2`).
- **H3 "offer-only"** (`access_offer = emp_acct | offered`) counts a plan that *exists* at the firm
  regardless of the individual's inclusion. H1 ⊂ H3-covered, so H1 shows a **larger** gap; the wedge
  between them is workers excluded from a plan their employer sponsors (part-time/tenure/hours rules).
- **H4 "ownership-based"** (`access_ownership = is_yes(EOWN_THR401) | is_yes(EOWN_IRAKEO)`) is a distinct
  lens: holds any DC/IRA **account** (employer or personal), DC/IRA only, not employer-dependent.

## 2. Participation (P) and Matching (M)
- **Presentation default (option 2, 2026-07-16):** participation and matching are **employer-benefit**
  concepts, so they are presented on an **employee basis** (Private + Government) — because SIPP's
  "employer *or business*" wording otherwise lets self-employed business-plan saving register as
  participation (~12%) and employer "match" (~9%) even though the access pillar forces the self-employed
  to lacking. `02_analysis.R` writes employee tables (`participation_employees.csv`,
  `matching_employees.csv`, `pillars_employee_summary.csv`); Figures 2–3 and the draft use them. The
  all-worker tables (`participation_all_workers.csv`, `matching_all_workers.csv`) are retained for
  reference and for the tax-expenditure cross-cuts (which stay on the full worker frame). Employee
  headline: participation **46.4%** (**91.7%** of workers who hold an employer plan — the contribution-
  question universe; conservative floor counting eligible-but-unenrolled as non-participants: 86.3%),
  match **39.9%** (81.5% of participants; 88.6% of DC/IRA-universe participants),
  median employer match **$3,000** / mean **~$5,800**; self-employed reported separately (12.5% / 9.2%).
- **Participation** = `is_yes(ESCNTYN_401|ESCNTYN_IRA|ESCNTYN_PEN)` ("respondent contributed … through
  main employer"; universe `EMJOB_* = 1`). P1's PUBLISHED denominator is the plan-type-observed access
  group (`access_emp_obs` = the ESCNTYN question universe; decision of 2026-08-27) — workers
  whose access comes only via the EINCPENS offer-and-inclusion branch were never asked the contribution
  question. The all-access denominator ships as a disclosed conservative floor
  (`participation_given_access_floor.csv`). P2 = among all workers.
- **Matching** = `is_yes(EECNTYN_401|EECNTYN_IRA)` ("main employer **contributed to** respondent's
  account"; universe `EMJOB_* = 1`) — a binary **receipt** flag, used for v1. **Correction (2026-07-10
  completeness pass):** SIPP *does* carry the employer contribution **dollar amount** — `TECNTAMT`,
  `TECNTAMT_401`, `TECNTAMT_IRA` (and `TECNTAMT1`/`TECNTAMT2` by plan). So match *generosity* is
  measurable; a future revision can report matched dollars, not just receipt. (Respondent contribution
  amounts: `TSCNTAMT`, and reference-year `TSCNTAMT1`/`TSCNTAMT2`.)

---

## 3. Interpretation, justification & journalist framing

**Question posed:** *"How many workers lack access to a retirement account provided by an employer at
this moment?"*

**Direct answer (weighted, workers 18–64 employed, latest SIPP = Dec 2024; employment classified from any December job line):**
- **76.2M workers (51.7%) lack access to an employer-provided retirement plan.**
- Decomposed: **61.1M are employees** whose employer offers nothing or excludes them (46.2% of the 132.3M
  who work *for* an employer), and **15.0M are self-employed**, who by definition have no employer to
  provide one (19.7% of the "lacking" total).
- By class: Private **49.1%** lack (55.1M); Government **30.2%** (6.0M); Self-employed **100%** (15.0M).
- (Employment is classified from *any* December job line, not a job-1-only record — the latter
  undercounts employment; the frame integrates to 147.3M civilian employed workers 18–64.)

**Is this framing justified? Yes — with four disclosures that must travel with the number:**

1. **Self-employed vs. employees (the key clarification).** Counting the self-employed as "lacking
   employer-provided access" is *literally* correct (they have no employer) but mechanically inflates an
   all-worker gap. The honest, journalist-ready move is to **decompose**: lead with the employee gap
   (46.2% / 61.1M) as the true "employer access gap," and report the all-worker figure (51.7% / 76.2M)
   only alongside the self-employed share. Never state "half of workers can't get an employer plan"
   without noting ~1 in 5 of those (19.7%) are self-employed by choice/status. (Self-employed retirement options
   — SEP/SIMPLE/solo-401(k)/IRA — are a separate story; see H4 ownership.)

2. **Self-reported, not employer-reported.** These are household self-reports of plan offer/inclusion.
   Administrative/establishment measures (BLS National Compensation Survey) put the private-sector
   *lack-access* rate near ~27%, well below our ~49% (private). SIPP self-report systematically shows larger gaps
   (workers under-report eligibility/awareness). This is a known feature and the reason EIG's original
   analysis chose SIPP; the number should be labeled a **self-reported** access gap, not conflated with
   administrative counts.

3. **"Plan" includes defined-benefit pensions, not only "accounts."** `EPENSNYN`/`EINCPENS` ask about a
   "pension or retirement plan" generally, and `EMJOB_PEN` is DB — so eligibility for a **DB pension**
   counts as "has access." 15.8% of workers participate in a DB plan (Government **50.3%**, Private
   10.7%), which is why the government gap looks small. If the journalist means an *account* (DC/401(k)/
   IRA) strictly, the DC/IRA-ownership lens (H4: 55.0% own one → 45.0% do not) is the account-specific
   complement. Among workers our metric counts as "having access," 10.4% do **not** own a DC/IRA account
   (they are eligible-but-not-enrolled, or covered only by DB).

4. **Access ≠ participation, and it's a point-in-time snapshot.** "Access" = *eligible to participate*,
   not enrolled (that's the participation pillar: 91.7% of employer-plan holders contribute; 86.3%
   under the conservative all-access floor). "At this
   moment" = the December 2024 reference month of the most recent SIPP release, not literally today.

**Bottom line.** The metric is defensible and validates against the predecessor EIG SIPP analysis
(our full-time rate 43.5% ≈ EIG's ~42%). It answers *"share/number of workers who lack access to an
employer-sponsored retirement plan, self-reported, as of late 2024."* For the exact phrase "retirement
**account** provided by an employer," the most precise answer pairs the **employee** figure
(46.2% / 61.2M, self-employed reported separately) with the DC-account lens (H4) so "plan" vs "account"
is not blurred.

---

## Provenance
Variable semantics: the 2025 SIPP Data Dictionary (Census Bureau). Figures:
`01_build_dataset.R` → `data/processed/sipp_fastfacts.parquet`, rebuilt 2026-07-15 on
SIPP 2025 (reference year 2024).
