# ============================================================
# CV Comparison Heatmap — six correction methods
#   Computes per-trial CV% for the Uncorrected yield and each of the six
#   correction methods using the same mixed-model logic as the Python script,
#   then renders a three-panel heatmap:
#     - Fixed Area Yield (kg ha^-1)             [Uncorrected, fixed across methods]
#     - Spatially Rescaled Yield (kg ha^-1)     [Corrected, per method a-f]
#     - Delta (kg ha^-1)                         [Uncorrected - Corrected]
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lme4)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# ---- 1. Load data ---------------------------------------------------------
file_path <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python/Ali_Data_kg_ha_updated_with_rescaled_yields.xlsx"
df <- read_excel(file_path)
names(df) <- str_trim(names(df))

# Six correction-method columns + (a)-(f) panel labels
correction_methods <- c(
  "Yield_ProductiveArea", "Yield_StandCount", "EmpiricalAlpha",
  "PowerLaw",             "non_linear",       "geostatistical"
)
titles_map <- c(
  Yield_ProductiveArea = "(a)",
  Yield_StandCount     = "(b)",
  EmpiricalAlpha       = "(c)",
  PowerLaw             = "(d)",
  non_linear           = "(e)",
  geostatistical       = "(f)"
)

for (col in c("ENTRY", "REP", "IBLK", "Experiment Name")) {
  if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
}

# ---- 2. Mixed-model CV calculator ----------------------------------------
# Mirrors Python `calculate_metrics`:
#   * Tries: trait ~ ENTRY (+ REP) + (0 + IBLK | REP) via lme4::lmer
#   * Falls back to OLS if mixed model fails
#   * CV% = 100 * sqrt(residual variance) / mean(trait)
calculate_cv <- function(data, trait) {
  analysis_data <- data %>% filter(!is.na(.data[[trait]]))
  if (nrow(analysis_data) < 5) return(NA_real_)

  has_multi_rep <- length(unique(analysis_data$REP)) > 1
  trait_mean    <- mean(analysis_data[[trait]], na.rm = TRUE)
  fixed_rhs     <- if (has_multi_rep) "ENTRY + REP" else "ENTRY"

  mse <- tryCatch({
    fit <- suppressMessages(suppressWarnings(
      lmer(as.formula(paste0(trait, " ~ ", fixed_rhs, " + (0 + IBLK | REP)")),
           data = analysis_data,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 5000)))
    ))
    sigma(fit)^2
  }, error = function(e) {
    summary(lm(as.formula(paste0(trait, " ~ ", fixed_rhs)),
               data = analysis_data))$sigma^2
  })

  if (is.na(mse) || trait_mean == 0) return(NA_real_)
  100 * sqrt(mse) / trait_mean
}

# ---- 3. Per-trial CV for Uncorrected + each method -----------------------
results_df <- df %>%
  group_by(Trial = `Experiment Name`) %>%
  group_modify(~ {
    out <- tibble(Uncorrected_CV = calculate_cv(.x, "Uncorrected_Yield"))
    for (m in correction_methods) {
      out[[paste0(m, "_CV")]] <- if (m %in% names(.x))
        calculate_cv(.x, m) else NA_real_
    }
    out
  }) %>%
  ungroup() %>%
  filter(!is.na(Uncorrected_CV))

print(results_df)

# ---- 4. Reshape to long format for heatmap -------------------------------
# Display order requested by user (top -> bottom in the figure).
# These names are matched against the `Trial` column from the Excel file. If
# the names in the data differ slightly (spaces, underscores, capitalisation,
# "MSU" vs "MRF" suffixes, etc.) you can edit this vector to match exactly.
display_order <- c(
  "Yellow_21_MRF",
  "Red & Pink_25_SVREC",
  "Red & Pink_24_SVREC",
  "Pinto_25_LODS",
  "Navy_25_SVREC",
  "Navy_24_SVREC",
  "Navy_22_SVREC",
  "Navy_22_MRF",
  "Mixed_25_SVREC",
  "Mixed_24_SVREC",
  "Kidney_25_LODS",
  "Kidney_22_MRF",
  "Kidney_21_MRF",
  "GN & Pinto_25_SVREC",
  "GN & Pinto_24_SVREC",
  "Cran_25_LODS",
  "Black_25_SVREC",
  "Black_25_LODS",
  "Black_24_SVREC",
  "Black_24_LODS",
  "Black_22_SVREC",
  "Black_22_MRF",
  "Black & Navy_25_LODS"
)

# Warn about any mismatches between the requested order and the data, then
# build the final factor levels: requested-order entries first (in order),
# followed by any extra trials present in the data but not in the list.
data_trials   <- unique(results_df$Trial)
missing_in_data  <- setdiff(display_order, data_trials)
extra_in_data    <- setdiff(data_trials,  display_order)
if (length(missing_in_data))
  message("Note: requested trials not found in data: ",
          paste(missing_in_data, collapse = ", "))
if (length(extra_in_data))
  message("Note: extra trials in data, appended to bottom: ",
          paste(extra_in_data, collapse = ", "))

trial_levels <- c(intersect(display_order, data_trials), extra_in_data)

heatmap_long <- results_df %>%
  pivot_longer(
    cols      = ends_with("_CV") & !matches("Uncorrected_CV"),
    names_to  = "Method",
    values_to = "Corrected"
  ) %>%
  mutate(
    Method      = str_remove(Method, "_CV$"),
    Method_Lbl  = titles_map[Method],
    Uncorrected = Uncorrected_CV,
    Delta       = Uncorrected - Corrected
  ) %>%
  select(Trial, Method, Method_Lbl, Uncorrected, Corrected, Delta)

heatmap_df <- heatmap_long %>%
  pivot_longer(cols = c(Uncorrected, Corrected, Delta),
               names_to = "Metric", values_to = "Value") %>%
  mutate(
    Trial      = factor(Trial, levels = rev(trial_levels)),
    Method_Lbl = factor(Method_Lbl, levels = unname(titles_map[correction_methods])),
    Metric     = factor(Metric, levels = c("Uncorrected", "Corrected", "Delta"))
  )

# ---- 5. Heatmap builder ---------------------------------------------------
build_heatmap <- function(d, diverging = FALSE, title_expr, legend_title) {
  p <- ggplot(d, aes(x = Method_Lbl, y = Trial, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(is.na(Value), "", sprintf("%.1f", Value))),
              size = 4.4, color = "grey15", fontface = "bold") +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = title_expr, x = NULL, y = NULL, fill = legend_title) +
    theme_minimal(base_size = 16, base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", size = 18, hjust = 0.5,
                                      margin = margin(b = 10)),
      axis.text.x.top  = element_text(face = "bold", size = 16),
      axis.text.y      = element_text(size = 14),
      panel.grid       = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(size = 14, face = "bold"),
      legend.text      = element_text(size = 13),
      legend.key.width = unit(1.6, "cm"),
      legend.key.height= unit(0.5, "cm"),
      plot.margin      = margin(8, 12, 8, 8)
    )

  if (diverging) {
    p + scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                             midpoint = 0, na.value = "grey92",
                             breaks = scales::pretty_breaks(n = 5))
  } else {
    p + scale_fill_viridis_c(option = "viridis", direction = -1,
                             na.value = "grey92")
  }
}

p_unc <- build_heatmap(
  heatmap_df %>% filter(Metric == "Uncorrected"),
  diverging   = FALSE,
  title_expr  = expression(bold("Fixed Area Yield (kg ha"^-1*")")),
  legend_title = "CV (%)"
)

p_cor <- build_heatmap(
  heatmap_df %>% filter(Metric == "Corrected"),
  diverging   = FALSE,
  title_expr  = expression(bold("Spatially Rescaled Yield (kg ha"^-1*")")),
  legend_title = "CV (%)"
) + theme(axis.text.y = element_blank())

p_del <- build_heatmap(
  heatmap_df %>% filter(Metric == "Delta"),
  diverging   = TRUE,
  title_expr  = expression(bold(Delta * " (kg ha"^-1*")")),
  legend_title = expression(Delta)
) + theme(axis.text.y = element_blank())

# ---- 6. Compose & save ----------------------------------------------------
combined <- p_unc + p_cor + p_del +
  plot_layout(widths = c(1.25, 1, 1)) +
  plot_annotation(
    title = "Trial CV by Correction Method (a-f): Fixed vs Spatially Rescaled Yield",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5,
                                margin = margin(b = 10))
    )
  )

print(combined)

output_path <- file.path(dirname(file_path), "Trial_CV_Heatmap.png")
ggsave(filename = output_path, plot = combined,
       width = 18, height = 12, dpi = 350, bg = "white")
cat("Saved to:", normalizePath(output_path), "\n")
