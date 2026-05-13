# ============================================================================
# Script: R1_C69c_geostat_sensitivity_diagnostic.R
#
# Purpose: Provide an honest sensitivity diagnostic for the geostatistical
# yield correction. The earlier all-plot regression (Figure S6) suggested the
# geostatistical method produces implausibly large yields when most of a plot
# is missing. This script quantifies that breakdown three ways:
#
#   Panel A. Zoomed "kriging-trustworthy" window (missing < 50%): hexbin +
#            linear + LOESS for Fixed-Area vs Geostatistical, with slope and
#            R^2 printed per yield type.
#   Panel B. Implausibility audit: histograms of plot-level yield with the
#            10,000 kg/ha threshold drawn; counts the plots that cross it.
#   Panel C. Slope-vs-threshold sweep: how slope and R^2 of the
#            yield ~ missing-area regression change as the maximum allowed
#            missing-area % grows from 20% to 100%. Shows where the method
#            becomes unreliable instead of picking one threshold.
#
# Output:
#   - figS7_geostat_sensitivity_diagnostic.png  (3 stacked panels)
#   - geostat_implausibility_audit.csv          (plots > 10,000 kg/ha)
#   - slope_vs_threshold_sweep.csv              (raw sweep values)
# ============================================================================

# === USER CONFIG =============================================================
DATA_DIR   <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python"
OUT_DIR    <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python"
PLOT_DATA  <- file.path(DATA_DIR, "Ali_Data_kg_ha_updated_with_rescaled_yields.xlsx")

IMPLAUSIBLE_KG_HA  <- 10000   # plots above this are flagged as implausible
TRUSTWORTHY_MAX    <- 50      # missing-area % cap for Panel A
SWEEP_THRESHOLDS   <- seq(20, 100, by = 5)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# === LIBRARIES ===============================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(hexbin)
  library(patchwork)
  library(scales)
})

theme_set(theme_classic(base_size = 11) +
  theme(panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold"),
        legend.position  = "right"))

# === LOAD ====================================================================
raw <- read_excel(PLOT_DATA)

required <- c("Experiment Name", "Experiment Name_", "HA%",
              "Uncorrected_Yield", "geostatistical")
missing  <- setdiff(required, names(raw))
if (length(missing) > 0) {
  stop("Missing required columns: ", paste(missing, collapse = ", "))
}

df <- raw %>%
  transmute(
    trial_id     = as.character(`Experiment Name_`),
    exp_name     = as.character(`Experiment Name`),
    market_class = str_trim(str_extract(exp_name, "^[^_]+")),
    missing_pct  = as.numeric(`HA%`),
    fixed_area   = as.numeric(Uncorrected_Yield),
    geo          = as.numeric(geostatistical)
  ) %>%
  filter(is.finite(missing_pct),
         is.finite(fixed_area),
         is.finite(geo))

cat("\nPlots total:", nrow(df), "\n")

# Long form for faceting
long <- df %>%
  pivot_longer(c(fixed_area, geo),
               names_to = "yield_type", values_to = "yield") %>%
  mutate(yield_type = recode(yield_type,
                             fixed_area = "Fixed-Area yield",
                             geo        = "Geostatistical yield"),
         yield_type = factor(yield_type,
                             levels = c("Fixed-Area yield",
                                        "Geostatistical yield")))

# === FIT HELPER ==============================================================
fit_one <- function(d) {
  if (nrow(d) < 3 || length(unique(d$missing_pct)) < 2) {
    return(tibble(n = nrow(d), slope = NA_real_, r_squared = NA_real_,
                  intercept = NA_real_, p_value = NA_real_))
  }
  m <- lm(yield ~ missing_pct, data = d)
  s <- summary(m)
  tibble(n = nrow(d),
         slope     = coef(m)[["missing_pct"]],
         r_squared = s$r.squared,
         intercept = coef(m)[["(Intercept)"]],
         p_value   = coef(s)["missing_pct", "Pr(>|t|)"])
}

# === PANEL A: ZOOMED (missing < TRUSTWORTHY_MAX) =============================
trust <- long %>% filter(missing_pct < TRUSTWORTHY_MAX)

stats_trust <- trust %>%
  group_by(yield_type) %>%
  group_modify(~ fit_one(.x)) %>%
  ungroup()

cat("\n--- Panel A: kriging-trustworthy window (missing <",
    TRUSTWORTHY_MAX, "%) ---\n")
print(stats_trust)

ann_A <- stats_trust %>%
  transmute(yield_type,
            lab = sprintf("slope = %.1f kg/ha per 1%%", slope))

pA <- ggplot(trust, aes(missing_pct, yield)) +
  geom_hex(bins = 35, alpha = 0.9) +
  scale_fill_viridis_c(name = "Plots", trans = "log10",
                       option = "mako", direction = -1) +
  geom_smooth(aes(color = "Linear"), method = "lm",
              se = FALSE, linewidth = 0.9) +
  geom_smooth(aes(color = "LOESS"), method = "loess",
              se = FALSE, linewidth = 0.9, linetype = "22") +
  geom_text(data = ann_A, aes(x = Inf, y = Inf, label = lab),
            hjust = 1.05, vjust = 1.4, size = 3.2, lineheight = 0.95,
            inherit.aes = FALSE) +
  scale_color_manual(name = "Fit",
                     values = c("Linear" = "#d7301f",
                                "LOESS"  = "#f1c40f"),
                     breaks = c("Linear", "LOESS")) +
  facet_wrap(~ yield_type, nrow = 1, scales = "fixed") +
  labs(title = sprintf("A. Kriging-trustworthy window (missing < %d%%)",
                       TRUSTWORTHY_MAX),
       x = "Missing plot area (%)",
       y = expression("Yield (kg ha"^-1*")")) +
  guides(fill  = guide_colorbar(order = 1, barheight = unit(2.2, "cm")),
         color = guide_legend(order = 2,
                              override.aes = list(
                                linetype  = c("solid", "22"),
                                linewidth = 0.9)))

# === PANEL B: IMPLAUSIBILITY AUDIT ==========================================
audit <- df %>%
  filter(geo > IMPLAUSIBLE_KG_HA) %>%
  arrange(desc(geo)) %>%
  select(trial_id, market_class, missing_pct, fixed_area, geo)

write.csv(audit,
          file.path(OUT_DIR, "geostat_implausibility_audit.csv"),
          row.names = FALSE)

n_impl_geo   <- sum(df$geo        > IMPLAUSIBLE_KG_HA)
n_impl_fixed <- sum(df$fixed_area > IMPLAUSIBLE_KG_HA)
frac_geo     <- n_impl_geo   / nrow(df)
frac_fixed   <- n_impl_fixed / nrow(df)

cat("\n--- Panel B: Implausibility audit (threshold =",
    IMPLAUSIBLE_KG_HA, "kg/ha) ---\n")
cat(sprintf("Fixed-Area:    %d / %d plots (%.2f%%) above threshold\n",
            n_impl_fixed, nrow(df), 100 * frac_fixed))
cat(sprintf("Geostatistical: %d / %d plots (%.2f%%) above threshold\n",
            n_impl_geo, nrow(df), 100 * frac_geo))
cat(sprintf("Max Fixed-Area    yield: %.0f kg/ha\n", max(df$fixed_area)))
cat(sprintf("Max Geostatistical yield: %.0f kg/ha\n", max(df$geo)))

if (nrow(audit) > 0) {
  top_trials <- audit %>%
    count(trial_id, market_class, sort = TRUE) %>%
    slice_head(n = 3)
  cat("\nTop 3 trials by implausible-plot count:\n")
  print(top_trials)
}

# Histograms with threshold line and inset count
hist_data <- long %>% filter(yield <= IMPLAUSIBLE_KG_HA)
pB <- ggplot(hist_data, aes(yield)) +
  geom_histogram(bins = 80, fill = "grey55", color = NA) +
  geom_vline(xintercept = IMPLAUSIBLE_KG_HA, color = "#d7301f",
             linewidth = 0.6, linetype = "22") +
  facet_wrap(~ yield_type, nrow = 1) +
  scale_x_continuous(labels = label_comma(),
                     limits = c(0, IMPLAUSIBLE_KG_HA)) +
  labs(title = sprintf("B. Implausibility audit (threshold = %s kg/ha)",
                       format(IMPLAUSIBLE_KG_HA, big.mark = ",")),
       x = expression("Plot yield (kg ha"^-1*")"),
       y = "Plot count")

# Add annotation per panel
ann_B <- tibble(
  yield_type = factor(c("Fixed-Area yield", "Geostatistical yield"),
                      levels = levels(long$yield_type)),
  lab = c(
    sprintf("%d plots (%.2f%%) > %s\nmax = %s kg/ha",
            n_impl_fixed, 100 * frac_fixed,
            format(IMPLAUSIBLE_KG_HA, big.mark = ","),
            format(round(max(df$fixed_area)), big.mark = ",")),
    sprintf("%d plots (%.2f%%) > %s\nmax = %s kg/ha",
            n_impl_geo, 100 * frac_geo,
            format(IMPLAUSIBLE_KG_HA, big.mark = ","),
            format(round(max(df$geo)), big.mark = ","))
  )
)
pB <- pB +
  geom_text(data = ann_B, aes(x = Inf, y = Inf, label = lab),
            hjust = 1.05, vjust = 1.4, size = 3.2, lineheight = 0.95,
            inherit.aes = FALSE)

# === PANEL C: SLOPE-VS-THRESHOLD SWEEP =======================================
sweep_one <- function(thresh, d, yield_col) {
  sub <- d[d$missing_pct < thresh, ]
  if (nrow(sub) < 5) {
    return(tibble(threshold = thresh, slope = NA, r_squared = NA,
                  n = nrow(sub)))
  }
  m <- lm(reformulate("missing_pct", response = yield_col), data = sub)
  s <- summary(m)
  tibble(threshold = thresh,
         slope     = coef(m)[["missing_pct"]],
         r_squared = s$r.squared,
         n         = nrow(sub))
}

sweep_fixed <- do.call(rbind, lapply(SWEEP_THRESHOLDS,
  function(t) cbind(sweep_one(t, df, "fixed_area"),
                    yield_type = "Fixed-Area yield")))
sweep_geo <- do.call(rbind, lapply(SWEEP_THRESHOLDS,
  function(t) cbind(sweep_one(t, df, "geo"),
                    yield_type = "Geostatistical yield")))

sweep_all <- bind_rows(sweep_fixed, sweep_geo) %>%
  mutate(yield_type = factor(yield_type,
                             levels = c("Fixed-Area yield",
                                        "Geostatistical yield")))

write.csv(sweep_all,
          file.path(OUT_DIR, "slope_vs_threshold_sweep.csv"),
          row.names = FALSE)

cat("\n--- Panel C: Slope vs missing-area threshold sweep ---\n")
print(sweep_all)

pC <- ggplot(sweep_all,
             aes(x = threshold, y = slope, color = yield_type)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("Fixed-Area yield"     = "#1b9e77",
                                "Geostatistical yield" = "#d7301f"),
                     name = NULL) +
  labs(title = "C. Slope vs maximum-allowed missing-area",
       x = "Max missing area included (%)",
       y = "Linear slope (kg/ha per 1% missing)") +
  theme(legend.position = "bottom")

# === SAVE FIGURE (Panel A only) =============================================
ggsave(file.path(OUT_DIR, "figS7_geostat_sensitivity_diagnostic.png"),
       pA, width = 10, height = 4.5, dpi = 300)

# === MANUSCRIPT-READY SUMMARY ===============================================
cat("\n--- Manuscript-ready paragraph ---\n")
sA_fixed <- stats_trust[stats_trust$yield_type == "Fixed-Area yield", ]
sA_geo   <- stats_trust[stats_trust$yield_type == "Geostatistical yield", ]
cat(sprintf(
"Restricting the analysis to plots with less than %d%% missing area (n = %d
of %d plots, %.1f%%), the Fixed-Area regression yielded slope = %.1f kg/ha
per 1%% missing area (R^2 = %.2f), while the geostatistical regression
yielded slope = %.1f kg/ha per 1%% missing area (R^2 = %.2f). Across the
full dataset, %d plots (%.2f%%) produced geostatistical yield estimates
exceeding the implausibility threshold of %s kg/ha, compared with %d
plots (%.2f%%) for the Fixed-Area yield. The slope of the geostatistical
yield ~ missing-area regression grew from %.1f to %.1f kg/ha per 1%% as
the inclusion threshold widened from 20%% to 100%% missing area
(Supplementary Figure S7C), indicating that geostatistical predictions
diverge from observed yields when most of a plot is missing.\n",
  TRUSTWORTHY_MAX,
  sA_fixed$n, nrow(df), 100 * sA_fixed$n / nrow(df),
  sA_fixed$slope, sA_fixed$r_squared,
  sA_geo$slope,   sA_geo$r_squared,
  n_impl_geo, 100 * frac_geo,
  format(IMPLAUSIBLE_KG_HA, big.mark = ","),
  n_impl_fixed, 100 * frac_fixed,
  sweep_geo$slope[sweep_geo$threshold == 20],
  sweep_geo$slope[sweep_geo$threshold == 100]
))

cat("\nWrote:\n",
    "  ", file.path(OUT_DIR, "figS7_geostat_sensitivity_diagnostic.png"), "\n",
    "  ", file.path(OUT_DIR, "geostat_implausibility_audit.csv"), "\n",
    "  ", file.path(OUT_DIR, "slope_vs_threshold_sweep.csv"), "\n",
    sep = "")
