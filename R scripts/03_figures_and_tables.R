## Created with R version 4.2.

# Load Required Libraries ----
library(readr) # For reading and writing CSV files
library(dplyr) # For data manipulation (e.g., filter, mutate, summarise)
library(tidyr) # For data tidying (e.g., pivoting)
library(ggplot2) # For creating plots
library(cowplot) # For combining plots (e.g., plot_grid)
library(canadianmaps) # For Canadian map layers
library(webr) # For creating PieDonut charts
library(moments) # For calculating skewness
library(sf) # For handling spatial data in ggplot
library(tibble) # For working with data frames

# Constants and Configurations ----
system_base_dir <- gsub("\\\\", "/", normalizePath(file.path(getwd(), "..")))

project_dir <- file.path(system_base_dir, "Codes and Inputs")
output_dir <- file.path(project_dir, "final datasets")
fig_save_dir <- file.path(project_dir, "figures and tables")

constants <- list(
  processed_data_path = file.path(output_dir, "sample_details_with_derived_parameters.csv"),
  borehole_location_path = file.path(output_dir, "borehole_location_and_datasource.csv"),
  emp_methods_fbd_path = file.path(output_dir, "emp_method_fbd_corr.csv"),
  emp_methods_vw_path = file.path(output_dir, "emp_method_wv_corr.csv"),
  emp_method_calculated = file.path(output_dir, "emp_method_final_df.csv")
)


pts_df <- load_data(constants$processed_data_path) %>%
  filter(data_quality_flag != 0 & !is.na(soil_group))

borehole_loc_and_ref_df <- load_data(constants$borehole_location_path)
FBD_df <- load_data(constants$emp_methods_fbd_path)
VW_df <- load_data(constants$emp_methods_vw_path)

# Print Messages ----
message("All datasets loaded successfully for figure and table generation.")

# Expressions for Plot Labels
expressions <- list(
  A0_xp = expression(italic(A[0]) ~ "" ~ "(%)"),
  a0_xp = expression(italic(a[0]) ~ "" ~ "(% per kPa)"),
  Cc_xp = expression(italic("C*"[c])),
  fbd_xp = expression(italic(rho[f]) ~ " " ~ (kg.m^-3)),
  e_f_xp = expression(italic(e[f])),
  e_th_xp = expression(italic("e*"[th])),
  w_xp = expression(italic(w) * " (%)"),
  sigma_v_kpa_xp = expression(italic(sigma[v]) * " (kPa)"),
  sigma_v_kpa_log_xp = expression(italic(sigma[v]) * " (kPa) " * italic((Log - Scale))),
  epsilon_pct_xp = expression(italic(epsilon) * " (%)")
)

# Color Scales
data_source_color_1 <- c("#ddaa33", "#bb5566", "#004488", "gray30")
soil_group_color <- c("Fine-grained" = "#0077BB", "Coarse-grained" = "#EE7733", "Peat" = "#009988")
ice_level_color <- c("Ice-rich" = "#6699CC", "Ice-poor" = "#EECC66")


# Define color and linetype scales
soil_group_color_emp <- c(
  "Fine-grained" = "#0077BB",
  "Coarse-grained" = "#EE7733",
  "Organic Soils" = "#33BBEE",
  "Peat" = "#009988"
)

# Define soil subgroup colors for emperical methods
soil_subgroup_color_emp <- c(
  "Peat" = "#1b7837", "Organics" = "#5aae61", "Organic Silt" = "#a6dba0",
  "Fat Clay" = "#54278f", "Lean Clay" = "#6a51a3", "High Plastic Soil" = "#807dba",
  "ML Lowland" = "#6baed6", "ML Upland" = "#4292c6", "Silt" = "#2171b5",
  "Low Plastic Fines" = "#08519c", "Silty Sand" = "#feb24c", "Clean Sand" = "#fd8d3c",
  "Silty Gravel" = "#fc4e2a", "Clean Gravel" = "#e31a1c", "Gravel" = "#bd0026",
  "Not Differentiated" = "#737373"
)


# Define line type scale for emperical methods
linetype_scale_1 <- c(
  "Ladanyi (1996)" = "dotdash",
  "Luscher and Afifi (1973)" = "twodash",
  "Nelson et al. (1983)" = "solid",
  "Nixon and Ladanyi (1987)" = "dashed",
  "Speer et al. (1973)" = "longdash",
  "Hanna et al. (1983)" = "solid"
)

linetype_scale_fig_12 <- c(
  "Ladanyi (1996)" = "solid",
  "Nixon and Ladanyi (1987)" = "dashed",
  "Luscher and Afifi (1973)" = "twodash",
  "Nelson et al. (1983)" = "solid",
  "Speer et al. (1973)" = "dotdash",
  "Hanna et al. (1983)" = "solid"
)

spacer <- ggdraw() + theme_void()




# Plot Theme
dp_theme <- theme(
  legend.text = element_text(size = 8),
  legend.key.height = unit(.4, "cm"),
  legend.key.width = unit(.5, "cm"),
  legend.title = element_text(size = 9),
  legend.background = element_rect(color = "gray30", fill = "white", size = .2),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 9),
  plot.title = element_text(size = 11, face = "bold"),
  legend.spacing.y = unit(.1, "cm")
)

fig_12_theme <- theme_bw() +
  theme(
    legend.position = "right",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.box.just = "right"
  ) + dp_theme



# Figure 1: base plot: Typical e-sigmav Relationship----

points <- c("a", "b", "c", rep(" ", length(seq(5, 30, 0.1)) - 126), "d", rep(" ", 125))
voids <- c(3, (3 / 1.01), 1.5, (-0.5 * (log(seq(5, 30, 0.1)) - log(5)) + 1.5))
sigma_vec <- c(0.2, 5, 5, seq(5, 30, 0.1))

fig_1_df <- data.frame(point = points, void_ratio = voids, pressures = sigma_vec)

fig_1 <- fig_1_df %>%
  ggplot() +
  geom_line(aes(x = pressures, y = void_ratio)) +
  geom_text(aes(x = pressures, y = void_ratio, label = point), vjust = -0.5, hjust = -1, size = 4) +
  labs(x = " ", y = " ", color = "Soil Type") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.line = element_line(arrow = arrow(angle = 10, type = "closed", length = unit(.15, "inch"))),
    axis.ticks.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  geom_segment(aes(x = 5, xend = 5, y = 0, yend = 1.5), linetype = "dashed") +
  geom_segment(aes(x = 18, xend = 18, y = 0, yend = 0.85), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 5, y = 3 / 1.01, yend = 3 / 1.01), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 5, y = 1.5, yend = 1.5), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 18, y = 0.85, yend = 0.85), linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5))

save_figure(fig_1, "Fig_01_dp.jpg", fig_save_dir, 4, 4)

# Fig_2: Typical Test Result ----

# Data Preparation
fig_2_data <- data.frame(
  sigmav = c(10, 10, 18, 25, 50, 100, 150),
  void_ratio = c(1.17, 1.17, 0.85, 0.71, 0.43, 0.3, 0.2),
  thaw_strain = (3 - c(1.17, 1.17, 0.85, 0.71, 0.43, 0.3, 0.2)) / 4
)

# Panel (a): Thaw Strain vs Vertical Stress
fig_2_a <- ggplot(fig_2_data) +
  geom_point(aes(x = sigmav, y = thaw_strain)) +
  geom_smooth(aes(x = sigmav, y = thaw_strain), se = FALSE, linetype = "dotted", color = "gray50", size = 0.5) +
  labs(x = NULL, y = NULL, title = "(a)") +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.line = element_line(arrow = arrow(length = unit(0.15, "inch"), type = "closed")),
    axis.ticks = element_blank()
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 160)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.2, 1)) +
  geom_segment(
    aes(x = 150, xend = 0, y = 0.7, yend = 0.62),
    linetype = "dashed", color = "gray30",
    arrow = arrow(length = unit(0.15, "inch"), type = "closed")
  )

# Panel (b): Example Log-Scale Plot
fig_2_b <- ggplot() +
  labs(x = NULL, y = NULL, title = "(b)") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.line = element_line(arrow = arrow(angle = 10, type = "closed", length = unit(0.15, "inch"))),
    axis.ticks = element_blank()
  ) +
  geom_point(aes(x = c(7, 8, 10, 18, 25), y = c(1.35, 1.25, 1.17, 0.85, 0.7))) +
  geom_point(aes(x = 5, y = 3), shape = 1) +
  geom_segment(aes(y = 3, yend = 3, x = 0, xend = 5), linetype = "dotted") +
  geom_segment(aes(x = 5, xend = 5, y = 0, yend = 1.5), linetype = "dotted") +
  geom_segment(aes(x = 5, xend = 5, y = 3, yend = 1.5), linetype = "dashed") +
  geom_segment(aes(x = 5, xend = 18, y = 1.5, yend = 0.85), linetype = "dashed") +
  geom_segment(aes(x = 18, xend = 18, y = 0, yend = 0.85), linetype = "dotted") +
  geom_segment(aes(y = 1.5, yend = 1.5, x = 0, xend = 5), linetype = "dotted") +
  geom_segment(aes(y = 0.85, yend = 0.85, x = 0, xend = 18), linetype = "dotted") +
  geom_segment(aes(x = 18, xend = 18 + ((18 - 5) / 2), y = 0.85, yend = 0.85 - ((1.5 - 1.15) / 2)), linetype = "dashed") +
  scale_x_log10(expand = c(0, 0), limits = c(3, 35)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5))


# Combine Panels into One Plot
fig_2_combined <- plot_grid(fig_2_a, NULL, fig_2_b, ncol = 3, rel_widths = c(0.8, 0.2, 1))

# Save the Combined Figure
save_figure(fig_2_combined, "Fig_02_dp.jpg", fig_save_dir, width = 8, height = 3)


# Fig_3: Emperical methods ----
FBD_df <- FBD_df %>%
  filter(!is.na(FBD), !is.na(thaw_strain), !is.na(soil_group)) %>%
  mutate(
    method = as.factor(method),
    soil_group = as.factor(soil_group),
    sub_group = as.factor(sub_group)
  )

# Plot FBD Methods
FBD_methods <- FBD_df %>%
  filter(!is.na(soil_group)) %>%
  ggplot() +
  geom_line(aes(
    x = FBD, y = thaw_strain, group = interaction(method, sub_group),
    linetype = method, color = sub_group
  ), size = 0.65) +
  scale_linetype_manual(values = linetype_scale_1) +
  scale_color_manual(values = soil_subgroup_color_emp) +
  guides(linetype = guide_legend(order = 1)) +
  labs(x = expressions$fbd_xp, y = expressions$epsilon_pct_xp, color = "Subgroups", linetype = "Method", title = "(a)") +
  facet_wrap(~soil_group) +
  theme_bw() +
  dp_theme +
  theme(
    legend.position = c(0.895, 0.555),
    legend.box.just = "right",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.35, "cm")
  )

# Plot VW Methods
VW_methods <- ggplot(VW_df) +
  geom_line(aes(
    x = vw, y = thaw_strain, group = interaction(method, sub_group),
    linetype = method, color = sub_group
  ), size = 0.65) +
  scale_linetype_manual(values = linetype_scale_1) +
  scale_color_manual(values = soil_subgroup_color_emp) +
  xlim(0, 100) +
  ylim(0, 100) +
  guides(linetype = guide_legend(order = 1)) +
  labs(
    x = "Volumetric Water Content (%)", y = expressions$epsilon_pct_xp,
    color = "Subgroups", linetype = "Method", title = "(b)"
  ) +
  facet_wrap(~major_category) +
  theme_bw() +
  dp_theme +
  theme(
    legend.position = "right",
    legend.box.just = "left",
    legend.key.width = unit(1, "cm")
  )

# Combine the plots
empirical_methods_corr <- plot_grid(
  ncol = 1, nrow = 2, rel_heights = c(1.2, 1),
  FBD_methods, VW_methods
)

# Save the combined plot
output_file <- file.path(fig_save_dir, "Fig_03_dp.jpg")
ggsave(output_file, plot = empirical_methods_corr, width = 10, height = 8)

# Fig_4: Map of Borehole Locations ----

# Province labels
prov_label <- data.frame(
  prov_name = c("BC", "QC", "NU", "SK", "YT", "MB", "ON", "NT", "AB", "NL"),
  lat = c(53, 52, 65, 53, 63.5, 55, 52, 63.5, 55, 54),
  long = c(-122, -71, -95, -106, -135.8, -98, -86, -119, -115, -61.5)
)

# Base map
Fig_4_dp <- ggplot() +
  geom_prov(fill = "white", colour = "gray40") +
  geom_text(
    data = prov_label, aes(x = long, y = lat, label = prov_name),
    color = "gray40", size = 3
  ) +
  geom_point(
    data = borehole_loc_and_ref_df, aes(x = long, y = lat, color = prim_source),
    size = 2.5, alpha = 0.15
  ) +
  geom_point(
    data = borehole_loc_and_ref_df, aes(x = long, y = lat, color = prim_source),
    shape = 8, size = 0.5
  ) +
  scale_color_manual(values = data_source_color_1) +
  labs(x = "Longitude (°)", y = "Latitude (°)", color = "Data Source") +
  theme_bw() +
  theme(legend.position = "top") +
  dp_theme

# Save the figure
ggsave(file.path(fig_save_dir, "Fig_04_dp.jpg"), Fig_4_dp, width = 7.5, height = 6)

# Figure 5: Variation in Test Procedure ----

# Panel (a): First (Min.) Applied Vertical Stress
fig_5a <- pts_df %>%
  ggplot(aes(y = sigma_v_min, x = prim_source, fill = prim_source)) +
  geom_boxplot(size = 0.2) +
  scale_fill_manual(values = alpha(data_source_color_1, 0.4)) +
  labs(
    x = "Data Source",
    y = "First (Min.) Applied Vertical Stress (kPa)",
    title = "(a)"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  dp_theme

# Panel (b): Last (Max.) Applied Vertical Stress
fig_5b <- pts_df %>%
  ggplot(aes(y = sigma_v_max, x = prim_source, fill = prim_source)) +
  geom_boxplot(size = 0.2) +
  scale_fill_manual(values = alpha(data_source_color_1, 0.4)) +
  labs(
    x = "Data Source",
    y = "Last (Max.) Applied Vertical Stress (kPa)",
    title = "(b)"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  dp_theme

# Panel (c): Number of Loading Steps (Histogram)
fig_5c <- pts_df %>%
  ggplot(aes(x = loading_steps_count, fill = prim_source)) +
  geom_bar(color = "black", size = 0.2) +
  scale_fill_manual(values = alpha(data_source_color_1, 0.4)) +
  labs(
    x = "Number of Loading Steps",
    y = "Frequency",
    title = "(c)",
    fill = "Data Source"
  ) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.75)) +
  dp_theme

# Combine Panels into One Figure
Fig_5_dp <- plot_grid(
  fig_5a, NULL, fig_5b, NULL, fig_5c,
  nrow = 1, ncol = 5, rel_widths = c(1, 0.05, 1, 0.05, 1)
)

# Save Figure
save_figure(Fig_5_dp, "Fig_05_dp.jpg", fig_save_dir, width = 10, height = 3.5)


# Fig_6: Soil Groups (Pie chart) ----

# Prepare data for the plot
data_dist_df <- pts_df %>%
  filter(!is.na(soil_group)) %>%
  group_by(soil_group, prim_source) %>%
  summarize(freq = sum(!is.na(soil_group)))

# Summarize the data for export
pie_donut_data <- data_dist_df %>%
  group_by(soil_group, prim_source) %>%
  summarise(n = sum(freq))

fig_6 <- PieDonut(
  pie_donut_data,
  aes(pies = soil_group, donuts = prim_source, count = n),
  r0 = 0.2, r1 = 0.8,
  showPieName = FALSE
)

# Save the Plot
save_figure(fig_6, "Fig_06_dp.jpg", fig_save_dir, width = 5, height = 5)

# Export the data to a CSV file
write.csv(pie_donut_data, file.path(fig_save_dir, "pie_donut_data.csv"), row.names = FALSE)

# NOTE: The figure included in the paper is created in Excel using the 'pie_donut_data.csv' file.
# This was necessary because the R script could not save the PieDonut chart in the correct format.

# Fig_7: Index properties ----

# Gravimetric Water Content Plots (Histogram and Boxplot)
gravimetric_w_hist <- function(soil_type, title) {
  pts_df %>%
    filter(soil_group == soil_type) %>%
    ggplot(aes(x = gravimetric_wc_init, fill = soil_group)) +
    geom_histogram(color = "black", alpha = 0.5, size = 0.3) +
    scale_fill_manual(values = alpha(soil_group_color, 0.5)) +
    labs(x = expressions$w_xp, y = "Frequency", title = title) +
    theme_bw() +
    theme(legend.position = "none") +
    dp_theme
}

Fig_7_w_hist_dp <- plot_grid(
  ncol = 4, nrow = 1, rel_widths = c(0.175, 1, 1, 1), spacer,
  gravimetric_w_hist("Coarse-grained", "(a.1) Coarse-Grained Samples"),
  gravimetric_w_hist("Fine-grained", "(a.2) Fine-Grained Samples"),
  gravimetric_w_hist("Peat", "(a.3) Peat Samples")
)

Fig_7_w_dp <- plot_grid(
  ncol = 1, nrow = 2, rel_heights = c(0.9, 1),
  pts_df %>%
    ggplot(aes(x = gravimetric_wc_init, y = soil_group, fill = soil_group)) +
    geom_boxplot(color = "black", size = 0.3) +
    scale_fill_manual(values = alpha(soil_group_color, 0.5)) +
    labs(x = expressions$w_xp, y = NULL, fill = "Soil Group", title = "(a) Gravimetric Water Content") +
    theme_bw() +
    theme(legend.position = "right") +
    dp_theme,
  Fig_7_w_hist_dp
)

# Frozen Bulk Density Plots (Histogram and Boxplot)
frozen_bd_hist <- function(soil_type, title) {
  pts_df %>%
    filter(soil_group == soil_type) %>%
    ggplot(aes(x = frozen_bulk_density_rep, fill = soil_group)) +
    geom_histogram(color = "black", alpha = 0.5, size = 0.3) +
    scale_fill_manual(values = alpha(soil_group_color, 0.5)) +
    labs(x = expressions$fbd_xp, y = "Frequency", title = title) +
    theme_bw() +
    theme(legend.position = "none") +
    dp_theme
}

Fig_7_fbd_hist_dp <- plot_grid(
  ncol = 4, nrow = 1, rel_widths = c(0.175, 1, 1, 1), spacer,
  frozen_bd_hist("Coarse-grained", "(b.1) Coarse-Grained Samples"),
  frozen_bd_hist("Fine-grained", "(b.2) Fine-Grained Samples"),
  frozen_bd_hist("Peat", "(b.3) Peat Samples")
)

Fig_7_fbd_dp <- plot_grid(
  ncol = 1, nrow = 2, rel_heights = c(0.9, 1),
  pts_df %>%
    ggplot(aes(x = frozen_bulk_density_rep, y = soil_group, fill = soil_group)) +
    geom_boxplot(color = "black", size = 0.3) +
    scale_fill_manual(values = alpha(soil_group_color, 0.5)) +
    labs(x = expressions$fbd_xp, y = NULL, fill = "Soil Group", title = "(b) Frozen Bulk Density") +
    theme_bw() +
    theme(legend.position = "right") +
    dp_theme,
  Fig_7_fbd_hist_dp
)

# Frozen Void Ratio Plots (Histogram and Boxplot)
frozen_vr_hist <- function(soil_type, title) {
  pts_df %>%
    filter(soil_group == soil_type) %>%
    ggplot(aes(x = frozen_void_ratio_calc, fill = soil_group)) +
    geom_histogram(color = "black", alpha = 0.5, size = 0.3) +
    scale_fill_manual(values = alpha(soil_group_color, 0.5)) +
    labs(x = expressions$e_f_xp, y = "Frequency", title = title) +
    theme_bw() +
    theme(legend.position = "none") +
    dp_theme
}

Fig_7_ef_hist_dp <- plot_grid(
  ncol = 4, nrow = 1, rel_widths = c(0.175, 1, 1, 1), spacer,
  frozen_vr_hist("Coarse-grained", "(c.1) Coarse-Grained Samples"),
  frozen_vr_hist("Fine-grained", "(c.2) Fine-Grained Samples"),
  frozen_vr_hist("Peat", "(c.3) Peat Samples")
)

Fig_7_ef_dp <- plot_grid(
  ncol = 1, nrow = 2, rel_heights = c(0.9, 1),
  pts_df %>%
    ggplot(aes(x = frozen_void_ratio_calc, y = soil_group, fill = soil_group)) +
    geom_boxplot(color = "black", size = 0.3) +
    scale_fill_manual(values = alpha(soil_group_color, 0.5)) +
    labs(x = expressions$e_f_xp, y = NULL, fill = "Soil Group", title = "(c) Frozen Void Ratio") +
    theme_bw() +
    theme(legend.position = "right") +
    dp_theme,
  Fig_7_ef_hist_dp
)

# Combine All Components into Final Figure
Fig_7_all <- plot_grid(
  ncol = 1, nrow = 5, rel_heights = c(1, 0.05, 1, 0.05, 1),
  Fig_7_w_dp, spacer, Fig_7_fbd_dp, spacer, Fig_7_ef_dp
)

# Save the Final Figure
save_figure(Fig_7_all, "Fig_07_dp.jpg", fig_save_dir, width = 10, height = 12)


# Fig_8: Ice Condition vs Soil Type ----

# Helper Function: Boxplot and Histogram
create_ice_condition_plot <- function(data, soil_group, title_box, title_hist) {
  boxplot <- data %>%
    filter(
      !is.na(soil_group) & !is.na(ice_level) & data_quality_flag != 0 & !is.na(A0),
      soil_group == !!soil_group
    ) %>%
    ggplot(aes(y = frozen_void_ratio_calc, x = ice_level)) +
    geom_boxplot(size = 0.3, fill = "white") +
    labs(y = expressions$e_f_xp, x = "Ice Condition", title = title_box) +
    theme_bw() +
    dp_theme +
    guides(fill = "none")

  histogram <- data %>%
    filter(
      !is.na(soil_group) & !is.na(ice_level) & data_quality_flag != 0 & !is.na(A0),
      soil_group == !!soil_group
    ) %>%
    ggplot(aes(x = mass_fraction_of_fines_in_soil, color = ice_level, fill = as.factor(USCS_symbol_rep_or_calc))) +
    geom_histogram(size = 0.3, alpha = 0.5) +
    scale_color_manual(values = c("white", "black")) +
    labs(x = "Mass Fraction of Fines (%)", y = "Frequency", color = "Ice Condition", fill = "USCS Symbol", title = title_hist) +
    theme_bw() +
    dp_theme +
    guides(color = "none")

  # Combine into a two-panel grid
  plot_grid(boxplot, histogram, nrow = 1, rel_widths = c(1, 2))
}

# Generate Plots for Each Soil Type
ice_condition_cg <- create_ice_condition_plot(
  pts_df, "Coarse-grained", "(a) Coarse-Grained Samples", ""
)

ice_condition_fg <- create_ice_condition_plot(
  pts_df, "Fine-grained", "(b) Fine-Grained Samples", ""
)

ice_condition_pt <- create_ice_condition_plot(
  pts_df, "Peat", "(c) Peat Samples", ""
)

# Combine All Plots into Final Figure
Fig_8_dp <- plot_grid(
  nrow = 3, ncol = 1, rel_heights = c(0.95, 0.05, 1.1),
  ice_condition_cg, spacer, ice_condition_fg
)

# Save the Figure
save_figure(Fig_8_dp, "Fig_08_dp.jpg", fig_save_dir, width = 8, height = 8)

# Fig_9: Swelling Condition vs Soil Type ----

# Helper Function: Boxplot and Histogram for Swelling Condition
create_swelling_condition_plot <- function(data, soil_group, title_box, title_hist) {
  filtered_data <- data %>%
    filter(
      !is.na(soil_group) & !is.na(ice_level) & data_quality_flag != 0 & !is.na(A0),
      soil_group == !!soil_group,
      ice_level == "Ice-poor"
    )

  boxplot <- filtered_data %>%
    ggplot(aes(y = frozen_void_ratio_calc, x = swelling_flag)) +
    geom_boxplot(size = 0.3) +
    labs(y = expressions$e_f_xp, x = "Swelling Flagged", title = title_box) +
    theme_bw() +
    dp_theme

  histogram <- filtered_data %>%
    ggplot(aes(x = mass_fraction_of_fines_in_soil, color = swelling_flag, fill = as.factor(USCS_symbol_rep_or_calc))) +
    geom_histogram(size = 0.3, alpha = 0.5) +
    scale_color_manual(values = c("white", "black")) +
    labs(x = "Mass Fraction of Fines (%)", y = "Frequency", color = "Swelling Flagged", fill = "USCS Symbol", title = title_hist) +
    theme_bw() +
    dp_theme +
    guides(color = "none")

  # Combine boxplot and histogram
  plot_grid(boxplot, histogram, nrow = 1, rel_widths = c(1, 2))
}

# Generate Plots for Each Soil Group
swelling_flag_cg <- create_swelling_condition_plot(
  pts_df, "Coarse-grained", "(a) Coarse-Grained Samples", ""
)

swelling_flag_fg <- create_swelling_condition_plot(
  pts_df, "Fine-grained", "(b) Fine-Grained Samples", ""
)

# Combine All Plots into Final Figure
Fig_9_dp <- plot_grid(
  nrow = 3, ncol = 1, rel_heights = c(0.95, 0.05, 1.1),
  swelling_flag_cg, spacer, swelling_flag_fg
)

# Save the Figure
save_figure(Fig_9_dp, "Fig_09_dp.jpg", fig_save_dir, width = 8, height = 8)

# Fig 10: A0 and a0 Histograms-----
# Filtered Data
Fig_10_df_A0 <- pts_df %>%
  filter(!is.na(soil_group) & !is.na(A0) & (!is.na(ice_level))) %>%
  filter(R_sq >= .8)

Fig_10_df_a0 <- pts_df %>%
  filter(!is.na(soil_group) & !is.na(a0) & (!is.na(ice_level))) %>%
  filter(R_sq >= .8)

# Panel (a): A0 Histogram
Fig_10_A0_dp <- Fig_10_df_A0 %>%
  ggplot() +
  geom_histogram(aes(x = A0, fill = as.factor(ice_level)), binwidth = 2, alpha = 0.6, size = 0.2, color = "black") +
  scale_fill_manual(values = ice_level_color) +
  labs(x = expressions$A0_xp, y = "Frequency", fill = "Ice Condition", title = "(a)") +
  geom_text(
    data = Fig_10_df_A0 %>%
      summarise(mean = round(mean(A0), 1), sd = round(sd(A0), 1), .by = c(soil_group)),
    aes(x = 60, y = 18, label = paste("Mean=", mean, "\nSD=", sd)), size = 3
  ) +
  facet_wrap(~ interaction(soil_group)) +
  theme_bw() +
  dp_theme +
  theme(legend.position = c(0.075, 0.8))


# Panel (b): a0 Histogram
Fig_10_a0_dp <- Fig_10_df_a0 %>%
  ggplot() +
  geom_histogram(aes(x = a0, fill = as.factor(ice_level)), binwidth = 0.025, alpha = 0.6, size = 0.2, color = "black") +
  scale_fill_manual(values = ice_level_color) +
  labs(x = expressions$a0_xp, y = "Frequency", fill = "Ice Condition", title = "(b)") +
  geom_text(
    data = Fig_10_df_a0 %>%
      summarise(mean = round(mean(a0), 3), sd = round(sd(a0), 3), .by = soil_group),
    aes(x = 1, y = 55, label = paste("Mean=", mean, "\nSD=", sd)), size = 3
  ) +
  facet_wrap(~ interaction(soil_group)) +
  theme_bw() +
  dp_theme +
  theme(legend.position = "none")

# Combine panels (a) and (b)
Fig_10_dp <- plot_grid(Fig_10_A0_dp, Fig_10_a0_dp, ncol = 1, nrow = 2)
save_figure(Fig_10_dp, "Fig_10_dp.jpg", fig_save_dir, width = 8, height = 6.5)


# Fig 11: Cc and e_th Histograms-----

# Filtered Data
Fig_11_df_e_th <- pts_df %>%
  filter(!is.na(soil_group) & !is.na(e_th) & !is.na(ice_level) & data_quality_flag != 0) %>%
  filter(R_sq_semilog >= 0.8)

Fig_11_df_Cc <- pts_df %>%
  filter(!is.na(soil_group) & !is.na(Cc) & !is.na(ice_level) & data_quality_flag != 0) %>%
  filter(R_sq_semilog >= 0.8)

# Panel (a): e_th
Fig_11_e_th_dp <- plot_grid(
  Fig_11_df_e_th %>%
    filter(soil_group != "Peat") %>%
    ggplot() +
    geom_histogram(aes(x = e_th, fill = ice_level), binwidth = 0.2, alpha = 0.6, size = 0.2, color = "black") +
    scale_fill_manual(values = ice_level_color) +
    labs(x = expressions$e_th_xp, y = "Frequency", fill = "Ice Condition", title = "(a)") +
    geom_text(
      data = Fig_11_df_e_th %>%
        filter(soil_group != "Peat") %>%
        summarise(mean = round(mean(e_th), 2), sd = round(sd(e_th), 2), .by = soil_group),
      aes(x = 6, y = 60, label = paste("Mean=", mean, "\nSD=", sd)), size = 3
    ) +
    facet_wrap(~ interaction(soil_group)) +
    theme_bw() +
    dp_theme +
    theme(legend.position = c(0.11, 0.8)),
  Fig_11_df_e_th %>%
    filter(soil_group == "Peat") %>%
    ggplot() +
    geom_histogram(aes(x = e_th, fill = ice_level), binwidth = 1, alpha = 0.6, size = 0.2, color = "black") +
    scale_fill_manual(values = ice_level_color) +
    labs(x = expressions$e_th_xp, y = NULL, fill = "Ice Condition", title = " ") +
    geom_text(
      data = Fig_11_df_e_th %>%
        filter(soil_group == "Peat") %>%
        summarise(mean = round(mean(e_th), 2), sd = round(sd(e_th), 2), .by = soil_group),
      aes(x = 25, y = 6, label = paste("Mean=", mean, "\nSD=", sd)), size = 3
    ) +
    facet_wrap(~ interaction(soil_group)) +
    theme_bw() +
    dp_theme +
    theme(legend.position = "none"),
  ncol = 2, rel_widths = c(2, 1)
)

# Panel (b): Cc
Fig_11_Cc_dp <- plot_grid(
  Fig_11_df_Cc %>%
    filter(soil_group != "Peat") %>%
    ggplot() +
    geom_histogram(aes(x = -Cc, fill = ice_level), binwidth = 0.06, alpha = 0.6, size = 0.2, color = "black") +
    scale_fill_manual(values = ice_level_color) +
    labs(x = expressions$Cc_xp, y = "Frequency", fill = "Ice Condition", title = "(b)") +
    geom_text(
      data = Fig_11_df_Cc %>%
        filter(soil_group != "Peat") %>%
        summarise(mean = round(mean(-Cc), 2), sd = round(sd(-Cc), 2), .by = soil_group),
      aes(x = 2.25, y = 60, label = paste("Mean=", mean, "\nSD=", sd)), size = 3
    ) +
    facet_wrap(~ interaction(soil_group)) +
    theme_bw() +
    dp_theme +
    theme(legend.position = "none"),
  Fig_11_df_Cc %>%
    filter(soil_group == "Peat") %>%
    ggplot() +
    geom_histogram(aes(x = -Cc, fill = ice_level), binwidth = 0.35, alpha = 0.6, size = 0.2, color = "black") +
    scale_fill_manual(values = ice_level_color) +
    labs(x = expressions$Cc_xp, y = NULL, fill = "Ice Condition", title = " ") +
    geom_text(
      data = Fig_11_df_Cc %>%
        filter(soil_group == "Peat") %>%
        summarise(mean = round(mean(-Cc), 2), sd = round(sd(-Cc), 2), .by = soil_group),
      aes(x = 10, y = 6, label = paste("Mean=", mean, "\nSD=", sd)), size = 3
    ) +
    facet_wrap(~ interaction(soil_group)) +
    theme_bw() +
    dp_theme +
    theme(legend.position = "none"),
  ncol = 2, rel_widths = c(2, 1)
)

# Combine Fig 11 Panels
Fig_11_dp <- plot_grid(Fig_11_e_th_dp, Fig_11_Cc_dp, ncol = 1, nrow = 2)
save_figure(Fig_11_dp, "Fig_11_dp.jpg", fig_save_dir, width = 8, height = 6.5)

# Figure 12 -----
pts_plus_emp_df <- read.csv(constants$emp_method_calculated)

# Plot 1: Not Differentiated Comparison
not_diff_comp <- plot_grid(
  ncol = 2,
  rel_widths = c(1, 0),
  ggplot() +
    # Ladanyi (1996) and Nixon and Ladanyi (1987)
    geom_line(
      data = FBD_df %>%
        filter(method %in% c("Ladanyi (1996)", "Nixon and Ladanyi (1987)")),
      aes(x = FBD, y = thaw_strain, color = soil_group, linetype = method),
      size = 0.7
    ) +
    # Luscher and Afifi (1973)
    geom_line(
      data = FBD_df %>%
        filter(method == "Luscher and Afifi (1973)"),
      aes(x = FBD, y = thaw_strain, group = sub_group, color = soil_group, linetype = method),
      size = 0.7
    ) +
    # Points for PTS Data
    geom_point(
      data = pts_plus_emp_df %>%
        filter(soil_group != "Peat", !is.na(frozen_bulk_density_rep), !is.na(epsilon_sigma_v_e_logP_100)),
      aes(x = frozen_bulk_density_rep, y = epsilon_sigma_v_e_logP_100, shape = soil_group, color = soil_group),
      alpha = 0.6, size = 1.5
    ) +
    # Scales
    scale_linetype_manual(values = linetype_scale_fig_12) +
    scale_color_manual(values = soil_group_color_emp) +
    xlim(550, 2600) +
    ylim(0, 70) +
    labs(
      x = expressions$fbd_xp,
      y = "Thaw Strain at 100 kPa (%)",
      color = "Major Soil Group",
      linetype = "Method",
      title = "(a)",
      shape = "Soil Category - PTS Data"
    ) +
    guides(
      linetype = guide_legend(order = 1),
      color = guide_legend(order = 2)
    ) +
    fig_12_theme,
  spacer
)

# Plot 2: Nelson et al. (1983) and Speer et al. (1973) Comparison
nelson_comp <- plot_grid(
  ncol = 2,
  rel_widths = c(1, 0.05),
  ggplot() +
    # Speer et al. (1973)
    geom_line(
      data = FBD_df %>%
        filter(method == "Speer et al. (1973)"),
      aes(x = FBD, y = thaw_strain, group = method, color = soil_group, linetype = method),
      size = 0.7
    ) +
    # Nelson et al. (1983)
    geom_line(
      data = FBD_df %>%
        filter(method == "Nelson et al. (1983)"),
      aes(x = FBD, y = thaw_strain, group = sub_group, color = soil_group, linetype = method),
      size = 0.7
    ) +
    # Points for PTS Data
    geom_point(
      data = pts_plus_emp_df %>%
        filter(!is.na(soil_group), !is.na(frozen_bulk_density_rep), !is.na(epsilon_sigma_v_e_logP_in_situ)),
      aes(x = frozen_bulk_density_rep, y = epsilon_sigma_v_e_logP_in_situ, shape = soil_group, color = soil_group),
      alpha = 0.6, size = 1.5
    ) +
    # Scales
    scale_color_manual(values = soil_group_color_emp) +
    scale_linetype_manual(values = linetype_scale_fig_12) +
    xlim(550, 2600) +
    ylim(0, 70) +
    labs(
      x = expressions$fbd_xp,
      y = "Thaw Strain at Overburden Pressure (%)",
      color = "Major Soil Group",
      linetype = "Method",
      title = "(b)",
      shape = "Soil Category - PTS Data"
    ) +
    guides(
      linetype = guide_legend(order = 1),
      color = guide_legend(order = 2)
    ) +
    fig_12_theme,
  spacer
)

# Plot 3: Hanna et al. (1983) Comparison
hanna_comp <- plot_grid(
  ncol = 2,
  rel_widths = c(1, 0.05),
  ggplot() +
    geom_line(
      data = hanna_df %>%
        filter(!is.na(vw), !is.na(thaw_strain)),
      aes(x = vw, y = thaw_strain, group = group_hanna, color = soil_group, linetype = method),
      size = 0.7
    ) +
    geom_point(
      data = pts_plus_emp_df %>%
        filter(!is.na(soil_group), !is.na(volumetric_wc_calc), !is.na(epsilon_sigma_v_e_logP_50)),
      aes(x = volumetric_wc_calc, y = epsilon_sigma_v_e_logP_50, shape = soil_group, color = soil_group),
      alpha = 0.6, size = 1.5
    ) +
    scale_color_manual(values = soil_group_color_emp) +
    xlim(0, 100) +
    ylim(0, 100) +
    labs(
      x = "Volumetric Water Content (%)",
      y = "Thaw Strain at 50 kPa (%)",
      color = "Major Soil Group",
      linetype = "Method",
      title = "(c)",
      shape = "Soil Category - PTS Data"
    ) +
    guides(
      linetype = guide_legend(order = 1),
      color = guide_legend(order = 2)
    ) +
    fig_12_theme,
  spacer
)

# Combine all plots into Figure 12
comp_visual <- plot_grid(nrow = 3, ncol = 1, not_diff_comp, nelson_comp, hanna_comp)

# Save the figure
ggsave("Fig_12_dp.jpg", plot = comp_visual, path = fig_save_dir, width = 7, height = 12)


# Table 2: Index Properties Statistics ----

# Function to Summarize Data
summarize_index_properties <- function(data, variable, variable_name, group_col) {
  data %>%
    summarise(
      var = variable_name,
      sample_size = sum(!is.na({{ variable }})),
      min = round(min({{ variable }}, na.rm = TRUE), 2),
      max = round(max({{ variable }}, na.rm = TRUE), 2),
      mean = round(mean({{ variable }}, na.rm = TRUE), 2),
      sd = round(sd({{ variable }}, na.rm = TRUE), 2),
      skewness = round(skewness({{ variable }}, na.rm = TRUE), 2),
      .by = {{ group_col }}
    )
}

# Generate Tables
table_2_dp_wc <- summarize_index_properties(pts_df, gravimetric_wc_init, "Gravimetric Water Content", soil_group)
table_2_dp_fbd <- summarize_index_properties(pts_df, frozen_bulk_density_rep, "Frozen Bulk Density", soil_group)
table_2_dp_ef <- summarize_index_properties(pts_df, frozen_void_ratio_calc, "Frozen Void Ratio", soil_group)

# Combine Tables
table_2_dp <- bind_rows(table_2_dp_wc, table_2_dp_fbd, table_2_dp_ef) %>%
  mutate(cov = round(sd * 100 / mean, 1)) %>%
  select(var, soil_group, sample_size, min, max, mean, sd, cov, skewness)

# Save as CSV
write.csv(table_2_dp, file.path(fig_save_dir, "Table_02_index_properties.csv"), row.names = FALSE)


# Table 4: A0 and a0 summary----
# Summarize A0
A0_table <- Fig_10_df_A0 %>%
  summarise(
    q_10 = round(quantile(A0, 0.1), 1),
    q_90 = round(quantile(A0, 0.9), 1),
    mean = round(mean(A0), 1),
    sd = round(sd(A0), 1),
    cov = round(sd * 100 / mean, 1),
    ske = round(skewness(A0), 2),
    .by = c(soil_group, ice_level)
  )

# Summarize a0
a0_table <- Fig_10_df_a0 %>%
  summarise(
    q_10 = round(quantile(a0, 0.1), 3),
    q_90 = round(quantile(a0, 0.9), 3),
    mean = round(mean(a0), 3),
    sd = round(sd(a0), 3),
    cov = round(sd * 100 / mean, 1),
    ske = round(skewness(a0), 2),
    .by = c(soil_group, ice_level)
  )

# Combine A0 and a0 Tables
A0_a0_table <- rbind(A0_table, a0_table)
write.csv(A0_a0_table, file.path(fig_save_dir, "Table_04_A0_a0.csv"), row.names = FALSE)


# Table 5: Summarize e_th, Cc, and sigma_v_th----
# e_th Table
e_th_table <- Fig_11_df_e_th %>%
  summarise(
    q_10 = round(quantile(e_th, 0.1), 2),
    q_90 = round(quantile(e_th, 0.9), 2),
    mean = round(mean(e_th), 2),
    sd = round(sd(e_th), 2),
    cov = round(sd * 100 / mean, 1),
    ske = round(skewness(e_th), 2),
    .by = soil_group
  )

# Cc Table
Cc_table <- Fig_11_df_Cc %>%
  summarise(
    q_10 = round(quantile(-Cc, 0.1), 2),
    q_90 = round(quantile(-Cc, 0.9), 2),
    mean = round(mean(-Cc), 2),
    sd = round(sd(-Cc), 2),
    cov = round(sd * 100 / mean, 1),
    ske = round(skewness(-Cc), 2),
    .by = soil_group
  )

# σ_v_th Table for Ice-Poor Conditions
sigma_v_th_table <- Fig_11_df_Cc %>%
  filter(ice_level == "Ice-poor") %>%
  summarise(
    q_10 = round(quantile(sigma_v_th, 0.1), 2),
    q_90 = round(quantile(sigma_v_th, 0.9), 2),
    mean = round(mean(sigma_v_th), 2),
    sd = round(sd(sigma_v_th), 2),
    cov = round(sd * 100 / mean, 1),
    ske = round(skewness(sigma_v_th), 2),
    .by = c(soil_group, swelling_flag)
  )

# Combine Tables
e_th_Cc_table <- bind_rows(
  e_th_table %>% mutate(variable = "e_th"),
  Cc_table %>% mutate(variable = "Cc"),
  sigma_v_th_table %>% mutate(variable = "sigma_v_th")
)

# Reorganize Columns for Clarity
e_th_Cc_table <- e_th_Cc_table %>%
  select(variable, soil_group, swelling_flag, q_10, q_90, mean, sd, cov, ske)

# Save Table
write.csv(e_th_Cc_table, file.path(fig_save_dir, "Table_05_e_th_Cc.csv"), row.names = FALSE)


# Table 5 and Table 6: comparison of emperical methods----


# Major Group Calculations ----
hanna_rmsd_and_bias <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "soil_group", "hanna_strain", "epsilon_sigma_v_e_logP_50", "Hannan et al. (1983)"
)



nelson_rmsd_and_bias <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "soil_group", "nelson_strain", "epsilon_sigma_v_e_logP_in_situ", "Nelson et al. (1983)"
)

luscher_rmsd_and_bias <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "soil_group", "luscher_strain", "epsilon_sigma_v_e_logP_100", "Luscher & Afifi (1973)"
)

nixon_rmsd_and_bias <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "soil_group", "nixon_strain", "epsilon_sigma_v_e_logP_100", "Nixon and Ladanyi (1978)"
)

ladanyi_rmsd_and_bias <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "soil_group", "ladanyi_strain", "epsilon_sigma_v_e_logP_100", "Ladanyi (1996)"
)

speer_rmsd_and_bias <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "soil_group", "speer_strain", "epsilon_sigma_v_e_logP_in_situ", "Speer et al. (1973)"
)

# Subgroup Calculations ----
hanna_rmsd_and_bias_by_subgroup <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "group_hanna", "hanna_strain", "epsilon_sigma_v_e_logP_50", "Hannan et al. (1983)",
  filter_soil = FALSE
) %>%
  filter(count > 5) %>%
  rename(sub_group = group_hanna)

nelson_rmsd_and_bias_by_subgroup <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "group_nelson", "nelson_strain", "epsilon_sigma_v_e_logP_in_situ", "Nelson et al. (1983)",
  filter_soil = FALSE
) %>%
  filter(count > 5) %>%
  rename(sub_group = group_nelson)

luscher_rmsd_and_bias_by_subgroup <- calculate_rmsd_and_bias(
  pts_plus_emp_df, "group_luscher", "luscher_strain", "epsilon_sigma_v_e_logP_100", "Luscher & Afifi (1973)",
  filter_soil = FALSE
) %>%
  rename(sub_group = group_luscher)

hanna_rmsd_and_bias_peat <- hanna_rmsd_and_bias_by_subgroup %>%
  filter(method == "Hannan et al. (1983)" & sub_group == "Peat") %>%
  rename(soil_group = sub_group)

# Combine and Finalize Data ----
rmsd_and_bias_df_final <- bind_rows(
  speer_rmsd_and_bias,
  ladanyi_rmsd_and_bias,
  nixon_rmsd_and_bias,
  hanna_rmsd_and_bias,
  nelson_rmsd_and_bias,
  luscher_rmsd_and_bias,
  hanna_rmsd_and_bias_peat
) %>%
  select(soil_group, method, count, RMSD_e_logP, Bias_e_logP) %>%
  arrange(soil_group, method)

rmsd_and_bias_df_by_sub_groups <- bind_rows(
  hanna_rmsd_and_bias_by_subgroup,
  nelson_rmsd_and_bias_by_subgroup,
  luscher_rmsd_and_bias_by_subgroup
) %>%
  mutate(
    sub_group = factor(
      sub_group,
      levels = c(
        "Peat", "Organics", "Organic Silt", "Fat Clay", "Lean Clay", "High Plastic Soil",
        "ML Lowland", "ML Upland", "Silt", "Low Plastic Fines",
        "Silty Sand", "Clean Sand",
        "Silty Gravel", "Clean Gravel", "Gravel",
        "Not Differentiated"
      ),
      ordered = TRUE
    )
  ) %>%
  select(method, sub_group, count, RMSD_e_logP, Bias_e_logP) %>%
  arrange(method, sub_group)

# Save Results ----
write.csv(rmsd_and_bias_df_final, file.path(fig_save_dir, "Table_06_comp_empirical_major_group.csv"), row.names = FALSE)

write.csv(rmsd_and_bias_df_by_sub_groups, file.path(fig_save_dir, "Table_07_comp_empirical_subgroup.csv"), row.names = FALSE)



# Appendix A: Figure 1 - Plot of Various Components of Thaw Strain ----

# Define a function to create strain component plots for a given ice level
create_strain_component_plot <- function(data, ice_level, title) {
  data %>%
    filter(data_quality_flag == "0" | !is.na(soil_group)) %>%
    filter(is.finite(soil_compression_strain_at_100_kpa)) %>%
    filter(ice_level == ice_level) %>%
    ggplot() +
    geom_boxplot(aes(x = "(1) Phase change", y = (phase_change_strain), fill = soil_group)) +
    geom_boxplot(aes(x = "(2) Excess water expulsion", y = (excess_water_strain), fill = soil_group)) +
    geom_boxplot(aes(x = "(3) Soil skeleton compression", y = (soil_compression_strain_at_100_kpa), fill = soil_group)) +
    geom_boxplot(aes(x = "(4) Total thaw strain", y = (total_strain_at_100_kpa), fill = soil_group)) +
    scale_fill_manual(values = alpha(soil_group_color, 0.5)) +
    theme_bw() +
    theme(legend.position = c(0.1, .75)) +
    dp_theme +
    ylim(-5, 90) +
    labs(
      x = "Thaw Strain Component",
      y = "Thaw Strain (%)",
      fill = "Soil Category",
      title = title
    )
}


# Create plots for Ice-Poor and Ice-Rich samples
epsilon_components_ice_poor <- create_strain_component_plot(pts_df, "Ice-poor", "Ice-poor samples at 100 kPa vertical stress")
epsilon_components_ice_rich <- create_strain_component_plot(pts_df, "Ice-rich", "Ice-rich samples at 100 kPa vertical stress")

# Combine plots into a single figure
Figure_A_1 <- plot_grid(
  ncol = 1,
  nrow = 2,
  epsilon_components_ice_poor,
  epsilon_components_ice_rich
)

# Save the figure
save_figure(Figure_A_1, "Figure_A_1.jpg", fig_save_dir, width = 8, height = 6.5)
