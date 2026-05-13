# ============================================================
# Heritability (H^2) Comparison Scatter
#   Fixed Area Yield (kg ha^-1)         vs
#   Spatially Rescaled Yield (kg ha^-1)
# Each point is one Program x Environment combination. Points
# above the 1:1 line indicate spatial rescaling improved H^2.
# R / ggplot2 port of the original Python (matplotlib + seaborn) script
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

# ---- 1. Load data ---------------------------------------------------------
file_path <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python/h2_summary.xlsx"
df <- read_excel(file_path)
names(df) <- str_trim(names(df))

# Keep the original column names from the Python script
baseline_col  <- "H2_CWT_A"        # baseline = Fixed Area Yield's H^2
corrected_col <- "H2_Corrected"    # corrected = Spatially Rescaled Yield's H^2

df <- df %>%
  mutate(
    Group     = paste(Program, Env),
    Baseline  = .data[[baseline_col]],
    Corrected = .data[[corrected_col]]
  ) %>%
  filter(!is.na(Baseline), !is.na(Corrected))

# ---- 2. Counts above / below the 1:1 line --------------------------------
# Improved: Corrected (Spatially Rescaled) > Baseline (Fixed Area)
# Worsened: Corrected < Baseline
n_improved <- sum(df$Corrected > df$Baseline)
n_worsened <- sum(df$Corrected < df$Baseline)
cat("Improved (above 1:1):", n_improved,
    " | Worsened (below 1:1):", n_worsened, "\n")

# ---- 3. Axis range (square plot) -----------------------------------------
min_val <- 0
max_val <- max(c(df$Baseline, df$Corrected), na.rm = TRUE) + 0.05

# Triangular polygon for the green "improvement zone" above the 1:1 line
shade_df <- data.frame(
  x = c(min_val, min_val, max_val),
  y = c(min_val, max_val, max_val)
)

# ---- 4. Build plot --------------------------------------------------------
n_groups <- length(unique(df$Group))
# Filled shape codes 21-25 support both 'fill' (interior color) and 'color' (border)
shape_pool <- c(21, 22, 23, 24, 25)
shape_vals <- rep_len(shape_pool, n_groups)

p <- ggplot(df, aes(x = Baseline, y = Corrected,
                    shape = Group, fill = Group)) +
  # Improvement zone shading (above 1:1)
  geom_polygon(data = shade_df, aes(x = x, y = y),
               inherit.aes = FALSE,
               fill = "#d9f0d3", alpha = 0.55) +
  # 1:1 reference line
  geom_abline(intercept = 0, slope = 1,
              color = "grey45", linetype = "dashed", linewidth = 0.9) +
  # Points: filled shapes with black border so colors show through
  geom_point(size = 6.5, alpha = 0.92, stroke = 1.0, color = "black") +
  # Annotations
  annotate("label",
           x = min_val + (max_val - min_val) * 0.04,
           y = min_val + (max_val - min_val) * 0.96,
           label = paste0("Improved: ", n_improved),
           hjust = 0, vjust = 1, size = 7.2, fontface = "bold",
           color = "#1b7837", fill = "white", label.size = 0,
           label.padding = unit(0.45, "lines")) +
  annotate("label",
           x = min_val + (max_val - min_val) * 0.96,
           y = min_val + (max_val - min_val) * 0.04,
           label = paste0("Worsened: ", n_worsened),
           hjust = 1, vjust = 0, size = 7.2, fontface = "bold",
           color = "#d62728", fill = "white", label.size = 0,
           label.padding = unit(0.45, "lines")) +
  # Scales
  scale_x_continuous(limits = c(min_val, max_val), expand = c(0, 0)) +
  scale_y_continuous(limits = c(min_val, max_val), expand = c(0, 0)) +
  scale_shape_manual(values = shape_vals) +
  scale_fill_viridis_d(option = "turbo", end = 0.92) +
  coord_equal() +
  labs(
    x = expression(bold("Fixed Area Yield (kg ha"^-1*"):  H"^2)),
    y = expression(bold("Spatially Rescaled Yield (kg ha"^-1*"):  H"^2)),
    fill = NULL, shape = NULL
  ) +
  guides(
    fill  = guide_legend(ncol = 3, byrow = TRUE,
                         override.aes = list(size = 5.5, shape = 21)),
    shape = guide_legend(ncol = 3, byrow = TRUE,
                         override.aes = list(size = 5.5))
  ) +
  theme_minimal(base_size = 19, base_family = "sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 22, hjust = 0.5,
                                    margin = margin(b = 10)),
    axis.title.x     = element_text(face = "bold", size = 21, margin = margin(t = 10)),
    axis.title.y     = element_text(face = "bold", size = 21, margin = margin(r = 10)),
    axis.text        = element_text(size = 18, color = "black"),
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.7),
    legend.position  = "bottom",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16, face = "bold"),
    legend.box.margin = margin(t = 8),
    plot.margin      = margin(14, 18, 14, 18)
  )

# Optional title (uncomment if desired):
# p <- p + labs(title = expression(bold(
#   "Heritability (H"^2*"): Fixed Area Yield vs. Spatially Rescaled Yield")))

# ---- 5. Display & save ----------------------------------------------------
print(p)

output_path <- file.path(dirname(file_path), "Heritability_FixedVsRescaled.png")
ggsave(
  filename = output_path,
  plot     = p,
  width    = 13, height = 14, dpi = 350, bg = "white"
)
cat("Saved to:", normalizePath(output_path), "\n")
