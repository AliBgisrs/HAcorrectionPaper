# ============================================================
# Grouped Yield Analysis — Missing Area (%) vs g/plant & g/m^2
#   per Super Group + an All-Combined panel
# R / ggplot2 port of the original Python (matplotlib + seaborn) script
# ============================================================

suppressPackageStartupMessages({
  library(readxl)       # read .xlsx
  library(dplyr)        # data wrangling
  library(tidyr)        # pivoting / joins
  library(stringr)      # string ops
  library(ggplot2)      # plotting
  library(scales)       # axis helpers (sec_axis transform)
  library(patchwork)    # multi-panel layout
})

# ---- 1. Load data ---------------------------------------------------------
file_path <- "C:/Users/bazrafka/Desktop/counting/DiscussionPaperData/Python/Ali_Data_kg_ha_updated.xlsx"
df <- read_excel(file_path)

# Clean column names and rename to match the analysis vocabulary
names(df) <- str_trim(names(df))
df <- df %>%
  rename(
    UncorrectedYield = Uncorrected_Yield,
    HA_Percent       = `HA%`
  )

# Derived variables (mirrors Python: g/plant from PLOTWT / Join_Count, g/m^2 from yield * 0.1)
df <- df %>%
  mutate(
    GramPerM2     = UncorrectedYield * 0.1,
    Join_Count    = na_if(Join_Count, 0),
    GramPerPlant  = PLOTWT / Join_Count
  )

# ---- 2. Market-class lookup table ----------------------------------------
# Same key/value pairs as the Python dict, in a long-format tibble for joins
market_class_map <- tibble::tribble(
  ~prefix, ~base_class,
  "2115", "Kidney",     "2116", "Yellow",          "2201", "Black",
  "2202", "Navy",       "2217", "Black",           "2218", "Navy",
  "2219", "Kidney",     "2402", "Black",           "2403", "Navy",
  "2404", "Black",      "2405", "GN & Pinto",      "2406", "GN & Pinto",
  "2407", "Red & Pink", "2408", "Mixed",           "2409", "Mixed",
  "2410", "Mixed",      "2411", "Mixed",           "2412", "Mixed",
  "2502", "Black",      "2503", "Navy",            "2504", "Black",
  "2505", "GN & Pinto", "2506", "GN & Pinto",      "2507", "Red & Pink",
  "2508", "Red & Pink", "2512", "Mixed",           "2510", "Mixed/General",
  "2511", "Mixed/General", "2520", "Mixed/General","24116", "Black",
  "25103", "Cran",      "25104", "Black",          "25105", "White & Red Kidney",
  "25106", "Cran",      "25107", "Black & Navy",   "25108", "Pinto",
  "25109", "Kidney",    "25110", "Cran",           "25111", "Black & Navy",
  "25112", "Pinto",     "25113", "Kidney",         "25114", "Cran"
)

# ---- 3. Super-group assignment (mirrors Python assign_super_group) -------
assign_super_group <- function(exp_name, base_class) {
  nm <- tolower(exp_name)
  if (!is.na(base_class) && base_class %in% c("Black", "Navy", "Black & Navy") ||
      str_detect(nm, "black|navy")) return("Black + Navy")
  if (!is.na(base_class) && base_class %in% c("Red & Pink", "Red and Pink") ||
      str_detect(nm, "red|pink")) return("Red + Pink")
  if (!is.na(base_class) && base_class %in% c("GN & Pinto", "Pinto") ||
      str_detect(nm, "pinto|gn")) return("Great Northern + Pinto")
  if (!is.na(base_class) && base_class %in% c("Kidney", "Cran") ||
      str_detect(nm, "kidney|cran")) return("Kidney + Cranberry")
  NA_character_
}

df <- df %>%
  mutate(
    exp_name = as.character(`Experiment Name`),
    prefix5  = str_sub(exp_name, 1, 5),
    prefix4  = str_sub(exp_name, 1, 4)
  ) %>%
  # Try 5-digit prefix first, then 4-digit
  left_join(market_class_map, by = c("prefix5" = "prefix")) %>%
  rename(class5 = base_class) %>%
  left_join(market_class_map, by = c("prefix4" = "prefix")) %>%
  rename(class4 = base_class) %>%
  mutate(base_class = coalesce(class5, class4)) %>%
  rowwise() %>%
  mutate(Super_Group = assign_super_group(exp_name, base_class)) %>%
  ungroup()

# Filter to plot-ready rows
plot_ready_df <- df %>%
  filter(!is.na(GramPerPlant), !is.na(GramPerM2),
         !is.na(HA_Percent),  !is.na(Super_Group),
         GramPerPlant <= 510)

# ---- 4. Panel-builder function -------------------------------------------
# Two regressions per panel:
#   - g/plant (poly order 4) -> primary y-axis (black)
#   - g/m^2  (poly order 1) -> secondary y-axis (darkorange, dashed)
# R^2 is computed from the polynomial fits (matches sklearn r2_score on
# np.polyfit predictions used in the Python script).
plot_panel <- function(data, group_name, order1 = 4, order2 = 1) {
  d <- data %>% select(x = HA_Percent, y1 = GramPerPlant, y2 = GramPerM2)

  # Polynomial fits for R^2 annotation
  fit1 <- lm(y1 ~ poly(x, order1, raw = TRUE), data = d)
  fit2 <- lm(y2 ~ poly(x, order2, raw = TRUE), data = d)
  r2_1 <- summary(fit1)$r.squared
  r2_2 <- summary(fit2)$r.squared

  # Linear slope of g/m^2 vs Missing Area (the dashed orange line)
  slope_y2 <- coef(lm(y2 ~ x, data = d))[2]   # units: g/m^2 per 1% missing area

  # Map secondary axis (g/m^2) into primary axis range for dual-axis display.
  # Use FIXED ranges so all panels share identical axes (consistency across
  # super groups). y1 is g/plant 0-500; y2 is g/m^2 over the global range.
  y1_rng <- c(0, 500)
  y2_rng <- range(plot_ready_df$GramPerM2, na.rm = TRUE)
  scale_y2 <- function(v) (v - y2_rng[1]) / diff(y2_rng) * diff(y1_rng) + y1_rng[1]
  inv_y2   <- function(v) (v - y1_rng[1]) / diff(y1_rng) * diff(y2_rng) + y2_rng[1]

  # ---- Slope inset (rotated bold arrow) -------------------------------------
  # Draw a bold arrow whose angle matches the slope of the dashed orange line
  # in panel coordinates. We need the slope expressed in y1 units per x unit
  # so the arrow's visible angle on screen agrees with the orange line.
  x_rng <- c(0, 100)
  slope_in_y1 <- slope_y2 * diff(y1_rng) / diff(y2_rng)  # convert g/m^2 -> y1 units

  arrow_x_len <- diff(x_rng) * 0.20                      # horizontal run of the arrow
  arrow_y_len <- slope_in_y1 * arrow_x_len               # vertical rise (signed)

  # Center the arrow horizontally; place it in the upper portion of the panel
  arrow_xc <- x_rng[1] + diff(x_rng) * 0.50
  arrow_yc <- y1_rng[1] + diff(y1_rng) * 0.70

  arrow_x0 <- arrow_xc - arrow_x_len / 2
  arrow_x1 <- arrow_xc + arrow_x_len / 2
  arrow_y0 <- arrow_yc - arrow_y_len / 2
  arrow_y1 <- arrow_yc + arrow_y_len / 2

  # Label sits above the highest end of the arrow with a clear vertical gap
  arrow_y_top <- max(arrow_y0, arrow_y1)
  label_x <- arrow_xc
  label_y <- arrow_y_top + diff(y1_rng) * 0.07

  slope_label <- sprintf("slope == %.1f~~g/m^2~per~'%%'", slope_y2)

  d_pts <- bind_rows(
    d %>% transmute(x, y = y1,           series = "g/plant"),
    d %>% transmute(x, y = scale_y2(y2), series = "g/m^2")
  )

  ggplot() +
    geom_point(data = d_pts,
               aes(x = x, y = y, color = series),
               alpha = 0.25, size = 0.7, show.legend = FALSE) +
    geom_smooth(data = d, aes(x = x, y = y1, color = "g/plant", fill = "g/plant"),
                method = "lm", formula = y ~ poly(x, order1, raw = TRUE),
                se = TRUE, alpha = 0.18, linewidth = 1.0) +
    geom_smooth(data = d, aes(x = x, y = scale_y2(y2),
                              color = "g/m^2", fill = "g/m^2"),
                method = "lm", formula = y ~ poly(x, order2, raw = TRUE),
                se = TRUE, alpha = 0.18, linewidth = 1.0, linetype = "dashed") +
    # ---- Slope arrow ---------------------------------------------------------
    # Arrow goes from the upper-left end to the lower-right end when slope < 0
    # (NW -> SE), and from lower-left to upper-right when slope > 0.
    # The geometry above already encodes the correct sign in arrow_y0/arrow_y1.
    geom_segment(aes(x = arrow_x0, xend = arrow_x1,
                     y = arrow_y0, yend = arrow_y1),
                 data = data.frame(arrow_x0 = arrow_x0, arrow_x1 = arrow_x1,
                                   arrow_y0 = arrow_y0, arrow_y1 = arrow_y1),
                 color = "grey15", linewidth = 1.7,
                 arrow = grid::arrow(length = unit(0.38, "cm"),
                                     type = "closed", angle = 22)) +
    annotate("text", x = label_x, y = label_y, label = slope_label,
             parse = TRUE, size = 5.6, fontface = "bold", color = "grey15") +
    scale_color_manual(
      values = c("g/plant" = "black", "g/m^2" = "darkorange"),
      breaks = c("g/plant", "g/m^2"),
      guide  = "none"
    ) +
    scale_fill_manual(
      values = c("g/plant" = "grey40", "g/m^2" = "darkorange"),
      guide  = "none"
    ) +
    scale_x_continuous(name = "Missing Area (%)", limits = c(0, 100),
                       breaks = seq(0, 100, 20),
                       expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(
      name   = "Gram per plant",
      limits = y1_rng,
      sec.axis = sec_axis(~ inv_y2(.), name = expression(bold(g/m^2)))
    ) +
    labs(
      title = group_name,
      subtitle = bquote(
        italic(R)^2 ~ "pl/m"^2 ~ "= " * .(sprintf("%.3f", r2_1)) ~ " | " ~
        italic(R)^2 ~ "g/m"^2 ~ "= " * .(sprintf("%.3f", r2_2))
      )
    ) +
    theme_bw(base_size = 19, base_family = "sans") +
    theme(
      plot.title         = element_text(face = "bold", size = 22, hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = 17, color = "grey15", hjust = 0.5,
                                        margin = margin(b = 10)),
      axis.title.x       = element_text(face = "bold", size = 19, margin = margin(t = 10)),
      axis.title.y       = element_text(face = "bold", size = 19, color = "black"),
      axis.title.y.right = element_text(face = "bold", size = 19, color = "darkorange",
                                        angle = 90),
      axis.text          = element_text(size = 17, color = "black"),
      axis.text.y.right  = element_text(color = "darkorange"),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.7),
      plot.margin        = margin(10, 18, 10, 18)
    )
}

# ---- 5. Build panels ------------------------------------------------------
super_groups <- c("Black + Navy", "Red + Pink",
                  "Great Northern + Pinto", "Kidney + Cranberry")

panels <- lapply(super_groups, function(g) {
  plot_panel(plot_ready_df %>% filter(Super_Group == g), g)
})

panel_all <- plot_panel(plot_ready_df, "All Combined")

# ---- 6. Compose layout: 2x2 grid of group panels + bottom full-width row -
combined <- (panels[[1]] | panels[[2]]) /
            (panels[[3]] | panels[[4]]) /
             panel_all +
  plot_layout(heights = c(1, 1, 1))

# ---- 7. Display & save ----------------------------------------------------
print(combined)

output_path <- file.path(dirname(file_path), "Final_Grouped_Yield_Analysis.png")
ggsave(
  filename = output_path,
  plot     = combined,
  width    = 18, height = 22, dpi = 350, bg = "white"
)
cat("Saved to:", normalizePath(output_path), "\n")
