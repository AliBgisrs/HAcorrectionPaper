# ============================================================
# Combined two-panel figure
#   (a)  Tornado: Fixed Area Yield vs Spatially Rescaled Yield (CV %)
#        + Delta CV side panel
#   (b)  Heritability scatter: H^2 of Fixed Area Yield vs H^2 of Spatially
#        Rescaled Yield, with green improvement zone above the 1:1 line.
# Panel titles are just "(a)" and "(b)"; no other titles or subtitles.
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# =========================================================
# PANEL (a) — Tornado plot from extracted figure values
# =========================================================
results_df <- tibble::tribble(
  ~Trial,                ~Fixed, ~Rescaled,
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
  mutate(Delta = Fixed - Rescaled)

trial_levels <- rev(results_df$Trial)

# Display labels matched to the heatmap figure
fixed_lbl    <- expression(bold("Fixed Area Yield (kg ha"^-1*")"))
rescaled_lbl <- expression(bold("Spatially Rescaled Yield (kg ha"^-1*")"))

# --- Tornado sub-panel (left side of panel a) -----------------------------
plot_df <- results_df %>%
  mutate(Trial = factor(Trial, levels = trial_levels)) %>%
  pivot_longer(cols = c(Fixed, Rescaled),
               names_to = "Type", values_to = "CV") %>%
  mutate(
    CV_signed = if_else(Type == "Fixed", -CV, CV),
    Type      = factor(Type, levels = c("Fixed", "Rescaled"))
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
    values = c(Fixed = "#1f77b4", Rescaled = "#ff7f0e"),
    labels = c(fixed_lbl, rescaled_lbl)
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
    legend.text        = element_text(size = 12),
    legend.spacing.x   = unit(0.4, "cm"),
    plot.margin        = margin(4, 8, 4, 4)
  )

# --- Delta sub-panel (right side of panel a) ------------------------------
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

# Combine tornado + delta into the full panel (a). Add the (a) tag via
# plot_annotation so it sits at the top-left of the composite panel only.
panel_a <- (p_tornado + p_delta) +
  plot_layout(widths = c(2.4, 1)) +
  plot_annotation(
    title = "(a)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0,
                                margin = margin(b = 6))
    )
  )

# Wrap into a single grob so it can be combined cleanly with panel (b)
panel_a_grob <- patchwork::wrap_elements(full = panel_a)

# =========================================================
# PANEL (b) — Heritability (H^2) scatter
# =========================================================
file_path <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python/h2_summary.xlsx"
df_h2 <- read_excel(file_path)
names(df_h2) <- str_trim(names(df_h2))

baseline_col  <- "H2_CWT_A"
corrected_col <- "H2_Corrected"

df_h2 <- df_h2 %>%
  mutate(
    Group     = paste(Program, Env),
    Baseline  = .data[[baseline_col]],
    Corrected = .data[[corrected_col]]
  ) %>%
  filter(!is.na(Baseline), !is.na(Corrected))

n_improved <- sum(df_h2$Corrected > df_h2$Baseline)
n_worsened <- sum(df_h2$Corrected < df_h2$Baseline)

min_val <- 0
max_val_h2 <- max(c(df_h2$Baseline, df_h2$Corrected), na.rm = TRUE) + 0.05

shade_df <- data.frame(
  x = c(min_val,  min_val,    max_val_h2),
  y = c(min_val,  max_val_h2, max_val_h2)
)

n_groups   <- length(unique(df_h2$Group))
shape_pool <- c(21, 22, 23, 24, 25)
shape_vals <- rep_len(shape_pool, n_groups)

p_h2 <- ggplot(df_h2, aes(x = Baseline, y = Corrected,
                          shape = Group, fill = Group)) +
  geom_polygon(data = shade_df, aes(x = x, y = y),
               inherit.aes = FALSE,
               fill = "#d9f0d3", alpha = 0.55) +
  geom_abline(intercept = 0, slope = 1,
              color = "grey45", linetype = "dashed", linewidth = 0.9) +
  geom_point(size = 6.5, alpha = 0.92, stroke = 1.0, color = "black") +
  annotate("label",
           x = min_val + (max_val_h2 - min_val) * 0.04,
           y = min_val + (max_val_h2 - min_val) * 0.96,
           label = paste0("Improved: ", n_improved),
           hjust = 0, vjust = 1, size = 7.0, fontface = "bold",
           color = "#1b7837", fill = "white", label.size = 0,
           label.padding = unit(0.45, "lines")) +
  annotate("label",
           x = min_val + (max_val_h2 - min_val) * 0.96,
           y = min_val + (max_val_h2 - min_val) * 0.04,
           label = paste0("Worsened: ", n_worsened),
           hjust = 1, vjust = 0, size = 7.0, fontface = "bold",
           color = "#d62728", fill = "white", label.size = 0,
           label.padding = unit(0.45, "lines")) +
  scale_x_continuous(limits = c(min_val, max_val_h2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(min_val, max_val_h2), expand = c(0, 0)) +
  scale_shape_manual(values = shape_vals) +
  scale_fill_viridis_d(option = "turbo", end = 0.92) +
  coord_equal() +
  labs(
    title = "(b)",
    x = expression(bold("Fixed Area Yield (kg ha"^-1*"):  H"^2)),
    y = expression(bold("Spatially Rescaled Yield (kg ha"^-1*"):  H"^2)),
    fill = NULL, shape = NULL
  ) +
  guides(
    fill  = guide_legend(ncol = 3, byrow = TRUE,
                         override.aes = list(size = 5.0, shape = 21)),
    shape = guide_legend(ncol = 3, byrow = TRUE,
                         override.aes = list(size = 5.0))
  ) +
  theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 22, hjust = 0,
                                    margin = margin(b = 6)),
    axis.title.x     = element_text(face = "bold", size = 17, margin = margin(t = 10)),
    axis.title.y     = element_text(face = "bold", size = 17, margin = margin(r = 10)),
    axis.text        = element_text(size = 14, color = "black"),
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.7),
    legend.position  = "bottom",
    legend.text      = element_text(size = 13),
    legend.title     = element_text(size = 13, face = "bold"),
    legend.box.margin = margin(t = 6),
    plot.margin      = margin(8, 12, 8, 12)
  )

panel_b_grob <- patchwork::wrap_elements(full = p_h2)

# =========================================================
# Combine panels (a) and (b) side by side, equal widths
# =========================================================
combined <- panel_a_grob + panel_b_grob +
  plot_layout(widths = c(1, 1))

print(combined)

output_path <- file.path(dirname(file_path), "Combined_Tornado_Heritability.png")
ggsave(
  filename = output_path,
  plot     = combined,
  width    = 24, height = 14, dpi = 350, bg = "white"
)
cat("Saved to:", normalizePath(output_path), "\n")
