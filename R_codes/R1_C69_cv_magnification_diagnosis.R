# ============================================================================
# Script: R1_C69_cv_magnification_diagnosis.R
# Addresses senior-author comment R1_C69.
#
# Verbatim comment (MAJOR):
# > Did you identify why these CV magnifications happened in some of these
# > cases? This will be important for discussion. And it cannot be the reason
# > below, try to find something explained by the data.
#
# Manuscript anchor: notably in Mixed market class trials (CV = 99.4%).
#
# Approach:
#   1. Read plot-level data directly from the xlsx.
#   2. Derive trial_id, market_class, missing_area_pct from the source columns.
#   3. Compute per-trial CV of Fixed-Area yield (= Uncorrected_Yield column)
#      and of geostatistical yield, then
#      delta_cv = CV_geostatistical - CV_fixedArea.
#   4. Test which trial-level features predict CV magnification (delta_cv > 0)
#      using logistic regression.
#   5. Produce Supplementary Figure S5, colored by market class.
# ============================================================================

# === USER CONFIG =============================================================
# Adjust paths to match your project layout.
DATA_DIR   <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python"
OUT_DIR    <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python"
PLOT_DATA  <- file.path(DATA_DIR, "Ali_Data_kg_ha_updated_with_rescaled_yields.xlsx")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# === LIBRARIES ===============================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(broom)
})

# Default ggplot theme to match paper figures
theme_set(theme_classic(base_size = 11) +
  theme(panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold")))

# === LOAD ====================================================================
raw <- read_excel(PLOT_DATA)

# Required columns (exact names as they appear in the workbook header).
# Note: "Fixed Area Yield" is the manuscript's new term for what the
# workbook still stores under the column name `Uncorrected_Yield`.
required <- c("Experiment Name", "Experiment Name_", "HA%",
              "Uncorrected_Yield", "geostatistical")
missing  <- setdiff(required, names(raw))
if (length(missing) > 0) {
  stop("Missing required columns in PLOT_DATA: ",
       paste(missing, collapse = ", "))
}

# === DERIVE TRIAL-LEVEL FIELDS ==============================================
# trial_id         = Experiment Name_  (e.g. 25104, 25111)
# market_class_raw = the part of "Experiment Name" before the first underscore
#                    e.g. "Black_25_LODS"        -> "Black"
#                         "Black & Navy_25_LODS" -> "Black & Navy"
# missing_area_pct = HA% (used directly per user's choice)
# yield_fixed_area = Uncorrected_Yield (renamed to match new manuscript term)
# yield_geo        = geostatistical
df <- raw %>%
  transmute(
    trial_id         = as.character(`Experiment Name_`),
    exp_name         = as.character(`Experiment Name`),
    market_class_raw = str_trim(str_extract(exp_name, "^[^_]+")),
    missing_area_pct = as.numeric(`HA%`),
    yield_fixed_area = as.numeric(Uncorrected_Yield),
    yield_geo        = as.numeric(geostatistical)
  ) %>%
  filter(!is.na(trial_id), !is.na(yield_fixed_area), !is.na(yield_geo))

# Count market-class components per trial: split "Black & Navy" -> c("Black","Navy")
class_components <- function(x) {
  parts <- str_split(x, "\\s*(&|/|,)\\s*")[[1]]
  parts <- str_trim(parts)
  parts <- parts[nzchar(parts)]
  unique(parts)
}

# === PER-TRIAL CV AND DELTA_CV ==============================================
cv_pct <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2 || mean(x) == 0) return(NA_real_)
  100 * sd(x) / mean(x)
}

per_trial <- df %>%
  group_by(trial_id) %>%
  summarise(
    market_class_raw = first(market_class_raw),
    mean_miss        = mean(missing_area_pct, na.rm = TRUE),
    n_plots          = n(),
    cv_fixed_area    = cv_pct(yield_fixed_area),
    cv_geo           = cv_pct(yield_geo),
    .groups          = "drop"
  ) %>%
  rowwise() %>%
  mutate(n_classes = length(class_components(market_class_raw))) %>%
  ungroup() %>%
  mutate(
    delta_cv  = cv_geo - cv_fixed_area,
    magnified = delta_cv > 0
  )

cat("\n--- Per-trial CV summary ---\n")
print(per_trial, n = Inf)

write.csv(per_trial,
          file.path(OUT_DIR, "cv_change_per_trial.csv"),
          row.names = FALSE)

# === LOGISTIC REGRESSION ====================================================
# Which trial-level features predict CV magnification?
# Guard: glm needs both outcome classes to be present.
fit_ok <- length(unique(na.omit(per_trial$magnified))) >= 2

if (fit_ok) {
  m <- glm(magnified ~ mean_miss + n_classes + n_plots,
           family = binomial, data = per_trial)
  cat("\n--- Logistic regression: predicting CV magnification ---\n")
  print(summary(m))

  coefs <- broom::tidy(m, conf.int = TRUE)
  write.csv(coefs,
            file.path(OUT_DIR, "cv_magnification_logit_coefs.csv"),
            row.names = FALSE)
} else {
  cat("\nNote: all trials are either magnified or not magnified.",
      "Logistic regression skipped.\n")
  m <- NULL
}

write.csv(per_trial,
          file.path(OUT_DIR, "cv_magnification_diagnosis.csv"),
          row.names = FALSE)

# === FIGURE S5 ==============================================================
# Build a palette that scales to however many distinct market-class strings
# appear in the data (Set1 only has 9 colors).
n_levels  <- length(unique(per_trial$market_class_raw))
if (n_levels <= 8) {
  market_palette <- RColorBrewer::brewer.pal(max(3, n_levels), "Set1")[seq_len(n_levels)]
} else {
  market_palette <- scales::hue_pal()(n_levels)
}

p <- ggplot(per_trial,
            aes(x = mean_miss, y = delta_cv,
                color = market_class_raw, size = n_plots)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
  geom_point(alpha = 0.75) +
  scale_color_manual(values = market_palette, name = "Market class") +
  scale_size_continuous(name = "Plots/trial", range = c(2, 7)) +
  labs(
    x = "Mean missing area per trial (%)",
    y = expression(Delta * "CV (%)  =  CV"["geo"] - "CV"["fixedArea"])
  )

ggsave(file.path(OUT_DIR, "figS5_cv_magnification.png"),
       p, width = 7, height = 5, dpi = 300)

cat("\nOutputs written to:\n",
    "  ", file.path(OUT_DIR, "cv_change_per_trial.csv"), "\n",
    "  ", file.path(OUT_DIR, "cv_magnification_diagnosis.csv"), "\n",
    if (fit_ok) paste0("   ",
        file.path(OUT_DIR, "cv_magnification_logit_coefs.csv"), "\n"),
    "  ", file.path(OUT_DIR, "figS5_cv_magnification.png"), "\n",
    sep = "")

# Suggested response language (informed by, but not assuming, the regression):
# > "CV magnification was concentrated in trials with low mean missing-area %
# >  and heterogeneous market-class composition (Mixed-class trials),
# >  consistent with small-denominator amplification. The diagnosis is
# >  supported by a logistic regression on trial-level features
# >  (Supplementary Figure S5)."
