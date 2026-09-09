# ==============================================================================
# eig_style_public.R -- self-contained chart styling for the public replication
# package.
#
# Why this file exists
# --------------------
# In EIG's internal repository the figure scripts source a shared brand style
# library (theme functions, a design-token file, and licensed brand OTFs). That
# library is not part of this replication package: the OTFs are commercially
# licensed and cannot be redistributed, and the token file is internal tooling.
#
# This module reproduces the pieces the analysis scripts actually consume, so the
# figure code in this package runs end to end with no external style dependency:
#
#   eig_load_tokens()                  -> palette + font-family constants
#   eig_theme_ggplot(tokens, base_size)-> the ggplot2 theme the figures build on
#
# The hex values are the EIG 2022 primary palette as used in the published
# figures, so regenerated charts carry the same colors. TYPOGRAPHY WILL DIFFER:
# without the brand OTFs the figure scripts fall back to a system serif/sans
# stack (they detect this themselves and report it at run time). Regenerated
# figures are therefore color-faithful but not typographically identical to the
# published PNGs in output/figures/.
# ==============================================================================

# ---- Design tokens -----------------------------------------------------------
# EIG 2022 primary palette. Only the ids the analysis code references are
# defined; this is not the full brand palette.
eig_load_tokens <- function(path = NULL) {
  # `path` is accepted and ignored so the call signature matches the internal
  # library (which reads a token file from disk). Nothing is read here.
  env <- new.env(parent = baseenv())

  env$EIG_COLORS <- c(
    "eig_teal_900"  = "#024140",  # deck / accent
    "eig_green_700" = "#19644D",  # primary single series
    "eig_gold_600"  = "#E1AD28",  # highlight / second series
    "eig_black"     = "#000000"   # text
  )

  # Documented neutral gray for de-emphasis only; carries no series encoding.
  # The 2022 palette has no gray token.
  env$EIG_NEUTRAL_GREY <- "#6E6E6E"

  # Token font primaries. The figure scripts check whether these are installed
  # and fall back to Georgia/Arial and then generic serif/sans if not.
  env$EIG_FONT_HEADLINE_PRIMARY <- "Source Serif Pro"
  env$EIG_FONT_BODY_PRIMARY     <- "Open Sans"

  # Version string, reported alongside generated chart payloads.
  env$EIG_TOKEN_VERSION <- "1.0.0"

  env
}

# ---- ggplot2 theme -----------------------------------------------------------
# Matches the internal eig_theme_ggplot(): horizontal gridlines only, no panel
# border, left-aligned title/subtitle, white plot background.
eig_theme_ggplot <- function(tokens = NULL, base_size = 10) {
  if (is.null(tokens)) tokens <- eig_load_tokens()
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  ggplot2::theme_minimal(base_size = base_size,
                         base_family = tokens$EIG_FONT_BODY_PRIMARY) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = tokens$EIG_FONT_HEADLINE_PRIMARY,
        face   = "bold",
        size   = base_size * 1.5,
        color  = tokens$EIG_COLORS[["eig_black"]],
        hjust  = 0
      ),
      plot.subtitle = ggplot2::element_text(
        family = tokens$EIG_FONT_BODY_PRIMARY,
        face   = "bold",
        size   = base_size * 1.1,
        color  = tokens$EIG_COLORS[["eig_teal_900"]],
        hjust  = 0
      ),
      axis.title       = ggplot2::element_text(face = "bold",
                            color = tokens$EIG_COLORS[["eig_black"]]),
      axis.text        = ggplot2::element_text(
                            color = tokens$EIG_COLORS[["eig_black"]]),
      legend.title     = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#D9D9D9",
                                                 linewidth = 0.3),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.caption     = ggplot2::element_text(size = base_size * 0.8,
                            color = "#444444", hjust = 0)
    )
}
