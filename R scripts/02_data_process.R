# This script is the main R file for importing raw data, validating consistency across datasets,
# processing the data, and preparing it for analysis and visualization.
# It serves as the primary workflow for generating the Permafrost Thaw Settlement Dataset.


## Created with R version 4.2.
library(readr) # For reading and writing CSV files
library(dplyr) # For data manipulation (e.g., filter, mutate, summarise)


# Constants and Configurations ----

# Set the "system_base_dir" variable to point to the folder where you have copied the `Codes and Inputs` directory.
# Ensure you replace all backslashes (`\`) in the path with forward slashes (`/`). For example:

system_base_dir <- gsub("\\\\", "/", normalizePath(file.path(getwd(), "..")))

# Change the system_base_dir to you local path


project_dir <- file.path(system_base_dir, "ThawSettlement_DataPaper")
output_dir <- file.path(project_dir, "final datasets")
input_dir <- file.path(project_dir, "input")

constants <- list(
  raw_data_path = file.path(input_dir, "raw_data.csv"),
  borehole_info_raw = file.path(input_dir, "raw_borehole_locations_and_sources.csv"),
  particle_size_raw = file.path(input_dir, "raw_particle_size_distribution.csv")
)


init_df_0 <- load_raw_data(constants$raw_data_path)
particle_size_df <- load_raw_data(constants$particle_size_raw)
borehole_loc_and_ref_df <- load_raw_data(constants$borehole_info_raw)

message("All datasets loaded successfully.")

# Cross-Dataset Consistency Validation ----
# Check particle size consistency
grain_size_ids <- init_df_0 %>%
  filter(grain_size_curve == TRUE) %>%
  pull(test_id) %>%
  unique()

missing_ids_in_grain_size <- setdiff(particle_size_df$test_id, grain_size_ids)

if (length(missing_ids_in_grain_size) == 0) {
  message("All test_id in particle_size_df are consistent with init_df_0 (grain_size_curve == TRUE).")
} else {
  warning("The following test_id in particle_size_df are missing in init_df_0:")
  print(missing_ids_in_grain_size)
}

# Check borehole consistency
borehole_ids <- init_df_0$borehole %>% unique()
borehole_ref_ids <- borehole_loc_and_ref_df$borehole %>% unique()
missing_boreholes <- setdiff(borehole_ref_ids, borehole_ids)

if (length(missing_boreholes) == 0) {
  message("All borehole IDs in borehole_loc_and_ref_df are consistent with init_df_0.")
} else {
  warning("The following borehole IDs in borehole_loc_and_ref_df are missing in init_df_0:")
  print(missing_boreholes)
}

# # Main Script ----

# Step 1: Load raw data----
raw_data_path <- file.path(project_dir, "input", "raw_data.csv")
init_df_0 <- load_raw_data(raw_data_path)

# Step 2: Convert to factors and process data ----
factor_cols <- c("prim_source", "soil_group", "reported_test_result_type","source_id", "data_source", "data_quality_flag")
init_df_0 <- convert_to_factors(init_df_0, factor_cols)
init_df_0 <- add_calculated_variables(init_df_0)
init_df_0 <- calculate_loading_steps(init_df_0)
init_df_0 <- calculate_thaw_strain(init_df_0)
init_df_0 <- add_thaw_strain_with_void_ratio(init_df_0)
init_df_0 <- calculate_void_ratio(init_df_0)


# Step 3: Quality control----
init_df_0 <- flag_erroneous_data(init_df_0)

# Step 4: Identify samples showing swelling----
init_df_0 <- calculate_swelling(init_df_0)

# Step 5: Calculate in-situ stress----
init_df_0 <- calculate_average_depth(init_df_0)
init_df_0 <- calculate_depth_intervals(init_df_0)
init_df_0 <- calculate_in_situ_stress(init_df_0)

# Step 6: Save homogneized test results----

test_all_df <- create_filtered_test_results(init_df_0)
save_test_results(test_all_df, output_dir)


# Step 7: Calculate comparative parameters ----

# Calculate A0, a0, and R-squared
init_df_0 <- calculate_A0_a0(init_df_0, test_all)

# Calculate Cc, sigma_v_th, and R-squared for semi-log fitting
init_df_0 <- calculate_Cc_sigma_v_th(init_df_0, test_all)

# Classify samples based on ice level
init_df_0 <- classify_ice_level(init_df_0)

# Calculate threshold initial void ratio
init_df_0 <- calculate_threshold_void_ratio(init_df_0, test_all)


# Step 8: Calculate thaw strain components----
init_df_0 <- calculate_thaw_strain_components(init_df_0)


# Step 9: Organize and Select data to include in "sample_details_with_derived_parameters.csv" ----

# Storing selected original and proceeded data to create final version of "sample_details_with_derived_parameters.csv"

# Define variables to remove from the dataset
var_to_remove_names <- c(
  paste0("sigma_v_", 1:7),
  paste0("e_", 1:7),
  paste0("epsilon_", 1:7),
  paste0("e_", 1:7, "_calc"),
  paste0("epsilon_", 1:7, "_calc"),
  paste0("delta_h_", 1:7),
  "data_source"
)

# Remove specified columns
pts_df_01 <- init_df_0[, !(names(init_df_0) %in% var_to_remove_names)]


# Define the column order for the PTS dataset
col_names_pts <- c(
  "test_id", "prim_source", "borehole", "sample_name", "test_name",
  "top", "bottom", "grain_size_curve", "mass_fraction_of_gravel_in_soil",
  "mass_fraction_of_sand_in_soil", "mass_fraction_of_silt_in_soil",
  "mass_fraction_of_clay_in_soil", "mass_fraction_of_fines_in_soil",
  "loss_on_ignition", "LL", "PL", "IP", "D10", "D50", "D60",
  "soil_description", "ground_ice_description", "soil_group",
  "USCS_symbol_rep_or_calc", "sample_mass_init", "mass_of_dry_sample",
  "gravimetric_wc_init", "gravimetric_wc_fin", "sample_volume_init",
  "solid_grain_density_rpt_or_asm", "initial_degree_of_saturation_rep",
  "sample_height_ave_or_typ", "sample_diameter_ave_or_typ",
  "frozen_bulk_density_rep", "frozen_dry_density_calc",
  "final_bulk_density_rep", "reported_test_result_type", "volumetric_wc_calc",
  "frozen_void_ratio_calc", "loading_steps_count", "sigma_v_min",
  "sigma_v_max", "num_of_swelling_loading_step", "swelling_flag",
  "A0", "a0", "R_sq", "Cc", "sigma_v_th", "e_th",
  "R_sq_semilog", "ice_level", "phase_change_strain", "excess_water_strain",
  "soil_compression_strain_at_100_kpa", "total_strain_at_100_kpa", "data_quality_flag", "note","source_id"
)

# Select and order columns based on the defined list
pts_df <- pts_df_01[, col_names_pts, drop = FALSE]


# Ensure output directory exists
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Step 9: Save "sample_details_with_derived_parameters.csv"----
output_file <- file.path(output_dir, "sample_details_with_derived_parameters.csv")
write.csv(pts_df, output_file, row.names = FALSE)


message("Sample properties and derived parameters saved to: ", output_file)


# Step 10: Save ""borehole_location_and_datasource.csv"" Datasets----
borehole_info_final <- file.path(output_dir, "borehole_location_and_datasource.csv")
write.csv(borehole_loc_and_ref_df, borehole_info_final, row.names = FALSE)

# Step 11: Save "particle_size_distribution.csv" Datasets----
particle_size_final <- file.path(output_dir, "particle_size_distribution.csv")
write.csv(particle_size_df, particle_size_final, row.names = FALSE)



# Step 12: Generate data for illustrating empirical methods ----

# This data is needed for Figure 3 showing a plot of various emperical methods

# Improting constans needed for calculating thaw strain based on each method stored in "emp_method_constants.csv" located in input directory
constants_file <- file.path(project_dir, "input", "emp_method_constants.csv")
constants_df <- read.csv(constants_file)


# Filtering, de-duplicating and generating data for each empirical method

luscher_inputs <- deduplicate_constants(
  constants_df,
  "Luscher and Afifi (1973)",
  "group_luscher",
  c("slope_1", "intercept_1", "slope_2", "intercept_2", "input_limits")
)
luscher_df <- generate_luscher_data(luscher_inputs)

nelson_inputs <- deduplicate_constants(
  constants_df,
  "Nelson et al. (1983)",
  "group_nelson",
  c("FDD_min", "FDD_max", "gs", "a1", "a2", "a3", "a4", "a5", "a6")
)
nelson_df <- generate_nelson_data(nelson_inputs)

hanna_inputs <- deduplicate_constants(
  constants_df,
  "Hanna et al. (1983)",
  "group_hanna",
  c("input_limits", "slope_1", "intercept_1", "slope_2", "intercept_2")
)
hanna_df <- generate_hanna_data(hanna_inputs)

# Speer et al. (1973), Nixon and Ladanyi (1987), Ladanyi (1996)
additional_methods_df <- constants_df %>%
  filter(method %in% c("Speer et al. (1973)", "Nixon and Ladanyi (1987)", "Ladanyi (1996)")) %>%
  rowwise() %>%
  do({
    FBD_seq <- seq(1100, 2050, 10)
    thaw_strain <- switch(.$method,
      "Speer et al. (1973)" = 73.6 - 101.8 * log(FBD_seq * 0.001),
      "Nixon and Ladanyi (1987)" = 90 - 86.8 * ((FBD_seq * 0.001 - 1.15)^0.5),
      "Ladanyi (1996)" = 85 - 85 * ((FBD_seq * 0.001 - 1.1)^0.5)
    )
    data.frame(
      FBD = FBD_seq,
      thaw_strain = thaw_strain,
      method = .$method,
      soil_group = .$soil_group,
      sub_group = "Not Differentiated"
    )
  })

# Combine results
results <- list(
  luscher_df = luscher_df,
  nelson_df = nelson_df,
  hanna_df = hanna_df,
  additional_methods_df = additional_methods_df
)

# Combine data for empirical methods that use frozen bulk density (FBD) as predictive variable
FBD_df <- bind_rows(
  results$luscher_df %>% select(FBD, thaw_strain, method, sub_group, soil_group),
  results$nelson_df %>% select(FBD, thaw_strain, method, sub_group, soil_group),
  results$additional_methods_df %>%
    filter(method %in% c("Speer et al. (1973)", "Nixon and Ladanyi (1987)", "Ladanyi (1996)")) %>%
    select(FBD, thaw_strain, method, sub_group, soil_group)
)

# Combine data for methods that use volumetric water content (vw) as predictive variable
VW_df <- results$hanna_df %>%
  select(vw, thaw_strain, method, sub_group, soil_group)

# Assign factor levels for sub_group
subgroup_levels <- c(
  "Peat", "Organics", "Organic Silt", "Fat Clay", "Lean Clay",
  "High Plastic Soil", "ML Lowland", "ML Upland", "Silt",
  "Low Plastic Fines", "Silty Sand", "Clean Sand", "Silty Gravel",
  "Clean Gravel", "Gravel", "Not Differentiated"
)

FBD_df$sub_group <- factor(FBD_df$sub_group, levels = subgroup_levels, ordered = TRUE)
VW_df$sub_group <- factor(VW_df$sub_group, levels = subgroup_levels, ordered = TRUE)
VW_df$major_category <- ifelse(
  VW_df$soil_group %in% c("Fine-grained", "Coarse-grained"),
  "Sedimentary Soils",
  "Peat and Organic Soils"
)

# Save generated dataframes for plotting empirical methods in output directory
emp_method_fbd_corr <- file.path(output_dir, "emp_method_fbd_corr.csv")
emp_method_vw_corr <- file.path(output_dir, "emp_method_wv_corr.csv")

write.csv(FBD_df, emp_method_fbd_corr, row.names = FALSE)
write.csv(VW_df, emp_method_vw_corr, row.names = FALSE)

# Step 13: Generate data for emperical method comparison ----

# This data is used to generate figures and tables (such as Fig. 12, Table 5, and Table 6)
# comparing various empirical methods and evaluating their performance by comparing estimated
# values with actual measurements in PTS data.


# Initiating a dataframe with all processed data while excluding samples with low data quality or undefined soil groups
pts_emp_init_1 <- init_df_0 %>%
  filter(data_quality_flag != 0 & !is.na(soil_group))

# Load filtered validation subsets to create a subset for each method based on USCS name
filtered_validation_file <- file.path(project_dir, "input", "filtered_validation_subsets.csv")
filter_val_df <- read.csv(filtered_validation_file)


constants_unique <- constants_df %>%
  select(-group_hanna, -group_luscher, -group_nelson, -ave_a0, -sub_group) %>% # Drop specific columns
  group_by(method, USCS_symbol_rep_or_calc) %>%
  summarise(across(everything(), first), .groups = "drop")



# Add strain calculations to PTS data
pts_emp_init_1 <- pts_emp_init_1 %>%
  mutate(
    epsilon_sigma_v_e_logP_100 = (frozen_void_ratio_calc - e_th + (Cc * log10(sigma_v_th / 100))) * 100 / (1 + frozen_void_ratio_calc),
    epsilon_sigma_v_e_logP_50 = (frozen_void_ratio_calc - e_th + (Cc * log10(sigma_v_th / 50))) * 100 / (1 + frozen_void_ratio_calc),
    epsilon_sigma_v_e_logP_in_situ = (frozen_void_ratio_calc - e_th + (Cc * log10(sigma_v_th / in_situ_sigma_v))) * 100 / (1 + frozen_void_ratio_calc),
  )

pts_emp_init_1$epsilon_sigma_v_e_logP_in_situ


pts_emp_init_1 <- pts_emp_init_1 %>%
  filter(!is.nan(epsilon_sigma_v_e_logP_in_situ)) %>%
  filter(!is.nan(epsilon_sigma_v_e_logP_100)) %>%
  filter(!is.nan(epsilon_sigma_v_e_logP_50))



# Merge PTS data with validation subsets
pts_emp_init <- merge(
  pts_emp_init_1,
  filter_val_df,
  by = "USCS_symbol_rep_or_calc",
  all.x = TRUE
)


# Process Speer, Nixon, and Ladanyi
df_0 <- pts_emp_init %>%
  select(1:114, epsilon_sigma_v_e_logP_in_situ, group_speer, group_nixon, group_ladanyi, USCS_symbol_rep_or_calc) %>%
  mutate(
    speer_strain = 73.6 - 101.8 * log(frozen_bulk_density_rep * 0.001),
    nixon_strain = 90 - 86.8 * ((frozen_bulk_density_rep * 0.001 - 1.15)^0.5),
    ladanyi_strain = 85 - 85 * ((frozen_bulk_density_rep * 0.001 - 1.1)^0.5)
  )

# Process Luscher and Afifi (1973)
df_1 <- merge(
  pts_emp_init %>%
    select(1:114, group_luscher),
  constants_df %>%
    filter(method == "Luscher and Afifi (1973)") %>%
    group_by(group_luscher) %>%
    slice(1) %>% select(input_limits, slope_1, intercept_1, slope_2, intercept_2),
  by = "group_luscher",
  all.x = TRUE
) %>%
  mutate(
    luscher_strain = ifelse(frozen_bulk_density_rep <= input_limits,
      slope_1 * frozen_bulk_density_rep + intercept_1,
      slope_2 * frozen_bulk_density_rep + intercept_2
    ),
    slope_1_luscher = slope_1,
    slope_2_luscher = slope_2,
    intercept_1_luscher = intercept_1,
    intercept_2_luscher = intercept_2
  )

# Process Nelson et al. (1983)
df_2 <- merge(
  pts_emp_init %>%
    select(1:114, group_nelson),
  constants_df %>%
    filter(method == "Nelson et al. (1983)") %>%
    group_by(group_nelson) %>%
    slice(1) %>%
    select(FDD_min, FDD_max, a1, a2, a3, a4, a5, a6, gs),
  by = "group_nelson",
  all.x = TRUE
) %>%
  mutate(
    FDD = frozen_bulk_density_rep / (1 + (gravimetric_wc_init * 0.01)),
    porosity = 1 - (FDD / (1000 * solid_grain_density_rpt_or_asm)),
    nelson_strain = a1 * porosity^2 + a2 * porosity +
      a3 * (porosity^2 * (gravimetric_wc_init / 100)) +
      a4 * (porosity / 100) + a5 * (porosity / gravimetric_wc_init) + a6
  )


# Process Hanna et al. (1983)
df_3 <- merge(
  pts_emp_init %>%
    select(1:114, group_hanna),
  constants_df %>%
    filter(method == "Hanna et al. (1983)") %>%
    group_by(group_hanna) %>%
    slice(1) %>%
    select(input_limits, slope_1, slope_2, intercept_1, intercept_2),
  by = "group_hanna",
  all.x = TRUE
) %>%
  mutate(
    hanna_strain = ifelse(volumetric_wc_calc <= input_limits,
      slope_1 * volumetric_wc_calc + intercept_1,
      slope_2 * volumetric_wc_calc + intercept_2
    ),
    slope_1_hanna = slope_1,
    slope_2_hanna = slope_2,
    intercept_1_hanna = intercept_1,
    intercept_2_hanna = intercept_2
  )

# Combining processed dataframes into a unified one
# Step 1: Sort each data frame by `test_id`
df_0 <- df_0 %>% arrange(test_id)
df_1 <- df_1 %>% arrange(test_id)
df_2 <- df_2 %>% arrange(test_id)
df_3 <- df_3 %>% arrange(test_id)

# Step 2: Select only necessary columns from df_1, df_2, and df_3
df_1_clean <- df_1 %>% select(-c(2:115)) # Keep all rows and remove only unnecessary columns
df_2_clean <- df_2 %>% select(-c(2:115)) # Keep all rows and remove only unnecessary columns
df_3_clean <- df_3 %>% select(-c(2:115)) # Keep all rows and remove only unnecessary columns

# Step 3: Combine all data frames using cbind
combined_df <- cbind(
  df_0,
  df_1_clean,
  df_2_clean,
  df_3_clean
)


# Remove duplicate columns (if any)
pts_emp_calculated <- combined_df[, !duplicated(names(combined_df))]
colnames(pts_emp_calculated)

pts_emp_calculated <- pts_emp_calculated %>%
  filter(data_quality_flag != 0 & !is.na(soil_group))


# Save final dataframe
output_file <- file.path(project_dir, "final datasets", "emp_method_final_df.csv")
write.csv(pts_emp_calculated, output_file, row.names = FALSE)

