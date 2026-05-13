# ============================================================
# Trial CV Tornado Plot from extracted figure values
#   Left:   Uncorrected vs Corrected CV (back-to-back)
#   Right:  Delta CV (Uncorrected - Corrected)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# ---- 1. Hard-coded values extracted from the figure -----------------------
# Order matches the figure (top -> bottom)
results_df <- tibble::tribble(
  ~Trial,                ~Uncorrected, ~Corrected,
  "Yellow_21_MRF",         9.5,  13.6,
  "Red & Pink_25_SVREC",  13.4,  17.8,
  "Red & Pink_24_SVREC",   7.9,   8.0,
  "Pinto_25_LODS",        22.5,  31.3,
  "Navy_25_SVREC",         7.4,  15.1,
  "Navy_24_SVREC",         9.5,   9.4,
  "Navy_22_SVREC",         9.8,  12.3,
  "Navy_22_MRF",          13.4,  14.0,
  "Mixed_25_SVREC",       15.3,  17.1,
  "Mixed_24_SVREC",       20.0,  19.8,
  "Kidney_25_LODS",       39.7,  50.5,
  "Kidney_22_MRF",        12.1,  12.2,
  "Kidney_21_MRF",        10.7,  12.1,
  "GN & Pinto_25_SVREC",  11.1,  14.5,
  "GN & Pinto_24_SVREC",  13.1,  12.2,
  "Cran_25_LODS",         37.5,  37.4,
  "Black_25_SVREC",       13.7,  12.8,
  "Black_25_LODS",        34.8,  33.9,
  "Black_24_SVREC",       13.3,  13.4,
  "Black_24_LODS",        23.2,  26.2,
  "Black_22_SVREC",       10.4,  11.5,
  "Black_22_MRF",         16.6,  16.9,
  "Black & Navy_25_LODS", 26.2,  28.8
) %>%
  mutate(Delta = Uncorrected - Corrected)

print(results_df)

# Lock trial order (top-to-bottom in figure -> reverse for ggplot y-axis)
trial_levels <- rev(results_df$Trial)

# ---- 2a. Tornado panel (left) ---------------------------------------------
plot_df <- results_df %>%
  mutate(Trial = factor(Trial, levels = trial_levels)) %>%
  pivot_longer(cols = c(Uncorrected, Corrected),
               names_to = "Type", values_to = "CV") %>%
  mutate(
    CV_signed = if_else(Type == "Uncorrected", -CV, CV),
    Type      = factor(Type, levels = c("Uncorrected", "Corrected"))
  )

max_val <- max(abs(plot_df$CV_signed), na.rm = TRUE) * 1.12

p_tornado <- ggplot(plot_df, aes(x = CV_signed, y = Trial, fill = Type)) +
  geom_col(width = 0.78, color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = sprintf("%.1f%%", CV),
        x = CV_signed / 2),
    color = "white", fontface = "bold", size = 4.4
  ) +
  geom_vline(xintercept = 0, color = "grey20", linewidth = 0.6) +
  scale_x_continuous(
    name   = "Coefficient of Variation (%)",
    limits = c(-max_val, max_val),
    breaks = pretty(c(-max_val, max_val), n = 8),
    labels = function(x) paste0(abs(round(x, 1)), "%")
  ) +
  scale_fill_manual(
    values = c(Uncorrected = "#1f77b4", Corrected = "#ff7f0e"),
    labels = c("Uncorrected CV %", "Corrected CV %")
  ) +
  labs(y = NULL, fill = NULL) +
  theme_minimal(base_size = 15, base_family = "sans") +
  theme(
    axis.title.x       = element_text(face = "bold", size = 16, margin = margin(t = 10)),
    axis.text.x        = element_text(size = 13),
    axis.text.y        = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dashed"),
    legend.position    = "top",
    legend.text        = element_text(size = 14),
    plot.margin        = margin(4, 8, 4, 4)
  )

# ---- 2b. Delta panel (right) ----------------------------------------------
# Positive delta (green) = correction REDUCED CV (improvement)
# Negative delta (red)   = correction INCREASED CV (worse)
delta_df <- results_df %>%
  mutate(Trial = factor(Trial, levels = trial_levels))

delta_max <- max(abs(delta_df$Delta), na.rm = TRUE) * 1.45

p_delta <- ggplot(delta_df, aes(x = Delta, y = Trial, fill = Delta > 0)) +
  geom_col(width = 0.78, color = "white", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey20", linewidth = 0.6) +
  geom_text(
    aes(label = sprintf("%+.1f", Delta),
        hjust = ifelse(Delta >= 0, -0.18, 1.18)),
    color = "grey20", fontface = "bold", size = 4.4
  ) +
  scale_x_continuous(
    name   = expression(bold(Delta * " CV (p.p.)")),
    limits = c(-delta_max, delta_max),
    breaks = pretty(c(-delta_max, delta_max), n = 5),
    labels = function(x) sprintf("%+.0f", x)
  ) +
  scale_fill_manual(
    values = c(`TRUE` = "#2ca02c", `FALSE` = "#d62728"),
    guide  = "none"
  ) +
  labs(y = NULL) +
  theme_minimal(base_size = 15, base_family = "sans") +
  theme(
    axis.title.x       = element_text(size = 16, margin = margin(t = 10)),
    axis.text.x        = element_text(size = 13),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dashed"),
    plot.margin        = margin(4, 4, 4, 8)
  )

# ---- 2c. Combine panels ---------------------------------------------------
combined <- (p_tornado + p_delta) +
  plot_layout(widths = c(2.4, 1)) +
  plot_annotation(
    title    = "Trial Comparison: Uncorrected vs. Corrected CV",
    subtitle = "Lower CV indicates higher experimental precision; right panel shows percentage-point change (Uncorrected - Corrected)",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 19, hjust = 0.5,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey40", size = 13, hjust = 0.5,
                                   margin = margin(b = 12))
    )
  )

# ---- 3. Display & save ----------------------------------------------------
print(combined)

output_path <- "Trial_CV_Tornado_FromFigure.png"   # change directory if desired
ggsave(
  filename = output_path,
  plot     = combined,
  width    = 15, height = 13, dpi = 350, bg = "white"
)

cat("Saved to:", normalizePath(output_path), "\n")
