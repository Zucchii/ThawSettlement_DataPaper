# This script contains functions for importing, processing, visualizing results,
# and creating the final Permafrost Thaw Settlement Dataset.
# Run this script first before the other two R files.

# Load Required Libraries
library(readr) # For reading and writing CSV files
library(dplyr) # For data manipulation (e.g., filter, mutate, summarise)
library(tidyr) # For data tidying (e.g., pivoting)

# Data Upload Functions----

# Data Loading For Raw Data Processing
load_raw_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Error: File not found at: ", file_path)
  }
  read.csv(file_path)
}

# Data Loading For Creating Figures and Tables
load_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Error: File not found at: ", file_path)
  }
  read.csv(file_path)
}


# Data Transformation Functions ----

# Function for Converting Specified Columns in a Data Frame to Factors
convert_to_factors <- function(data, cols_to_factor) {
  data <- data %>% mutate(across(all_of(cols_to_factor), as.factor))
  return(data)
}

# Function to add calculated variables for volumetric water content and frozen void ratio to the data frame
add_calculated_variables <- function(data) {
  data <- data %>%
    mutate(
      volumetric_wc_calc = 100 * (gravimetric_wc_init * 0.01 / (1 + 0.01 * gravimetric_wc_init)) * (frozen_bulk_density_rep / 1000),
      frozen_void_ratio_calc = ((1 + 0.01 * gravimetric_wc_init) * solid_grain_density_rpt_or_asm * 1000 / frozen_bulk_density_rep) - 1
    )
  return(data)
}

# Function for summarizing and extracting loading step data for test comparisons
calculate_loading_steps <- function(data) {
  data <- data %>%
    mutate(
      loading_steps_count = 7 - rowSums(is.na(select(., starts_with("sigma_v_")))),
      sigma_v_min = sigma_v_1
    )

  sigma_v_max <- numeric(nrow(data))
  for (i in seq_len(nrow(data))) {
    if (data$loading_steps_count[i] != 0) {
      sigma_v_max[i] <- data[[paste0("sigma_v_", data$loading_steps_count[i])]][i]
    } else {
      sigma_v_max[i] <- NA
    }
  }
  data$sigma_v_max <- sigma_v_max
  return(data)
}


# Functions for Standardizing and Harmonizing Test Results Across Various Sources ----
calculate_thaw_strain <- function(data) {
  for (i in 1:7) {
    data[[paste0("epsilon_", i, "_calc")]] <- ifelse(
      is.na(data[[paste0("delta_h_", i)]]),
      data[[paste0("epsilon_", i)]],
      round(data[[paste0("delta_h_", i)]] * 100 / data$sample_height_ave_or_typ, 1)
    )
  }
  return(data)
}

add_thaw_strain_with_void_ratio <- function(data) {
  for (i in 1:7) {
    data[[paste0("epsilon_", i, "_calc")]] <- ifelse(
      !is.na(data[[paste0("epsilon_", i, "_calc")]]),
      data[[paste0("epsilon_", i, "_calc")]],
      round((data$frozen_void_ratio_calc - data[[paste0("e_", i)]]) * 100 / (1 + data$frozen_void_ratio_calc), 1)
    )
  }
  return(data)
}

calculate_void_ratio <- function(data) {
  for (i in 1:7) {
    data[[paste0("e_", i, "_calc")]] <- ifelse(
      is.na(data[[paste0("epsilon_", i, "_calc")]]),
      NA,
      round(data$frozen_void_ratio_calc - data[[paste0("epsilon_", i, "_calc")]] * 0.01 * (1 + data$frozen_void_ratio_calc), 3)
    )
  }
  return(data)
}


# Data Quality Control Functions----

# Functions for Checking Data and Identifying Erroneous Test Results

flag_erroneous_data <- function(data) {
  indices_to_flag <- which(
    data$epsilon_1_calc > data$epsilon_2_calc |
      data$epsilon_2_calc > data$epsilon_3_calc |
      data$epsilon_3_calc > data$epsilon_4_calc |
      data$epsilon_4_calc > data$epsilon_5_calc |
      data$epsilon_5_calc > data$epsilon_6_calc |
      data$epsilon_6_calc > data$epsilon_7_calc
  )
  data$note[indices_to_flag] <- "Erroneous data (thaw strain reduction or void ratio increase with vertical stress)"
  data$data_quality_flag[indices_to_flag] <- 0

  indices_to_flag_2 <- which(
    data$e_1_calc < 0 | data$e_2_calc < 0 | data$e_3_calc < 0 |
      data$e_4_calc < 0 | data$e_5_calc < 0 | data$e_6_calc < 0 | data$e_7_calc < 0
  )
  data$note[indices_to_flag_2] <- "Erroneous data (negative values for void ratio)"
  data$data_quality_flag[indices_to_flag_2] <- 0
  return(data)
}

# Swelling Identification Functions----

# Functions for Identifying Samples that Undergo Swelling

calculate_swelling <- function(data) {
  data$phase_change_strain <- round((8.26 * data$frozen_void_ratio_calc) / (1 + data$frozen_void_ratio_calc), 2)
  data$num_of_swelling_loading_step <- NA
  data$swelling_flag <- NA

  for (i in 1:nrow(data)) {
    strain_steps <- data[i, paste0("epsilon_", 1:7, "_calc")]
    swelling_conditions <- strain_steps <= data$phase_change_strain[i]
    data$num_of_swelling_loading_step[i] <- ifelse(all(is.na(swelling_conditions)), NA, sum(swelling_conditions, na.rm = TRUE))
    data$swelling_flag[i] <- ifelse(all(is.na(swelling_conditions)), NA, any(swelling_conditions, na.rm = TRUE))
  }
  return(data)
}

# In-Situ Pressure Calculation Functions----

# Function for Calculating Average Depth for Each Sample
calculate_average_depth <- function(data) {
  data <- data %>% mutate(ave_depth = (top + bottom) / 2)
  return(data)
}

# Function for Dividing Each Borehole into Depth Intervals Based on Available Samples at Each Borehole

calculate_depth_intervals <- function(data) {
  data$depth_interval_start <- NA
  data$depth_interval_end <- NA

  for (i in 1:nrow(data)) {
    if (i == 1 || data$sample_name[i] != data$sample_name[i - 1]) {
      if (!is.na(data$borehole[i])) {
        if (i == 1 || data$borehole[i] != data$borehole[i - 1]) {
          data$depth_interval_start[i] <- 0
        } else {
          data$depth_interval_start[i] <- data$ave_depth[i - 1]
        }
        data$depth_interval_end[i] <- data$ave_depth[i]
      }
    } else {
      data$depth_interval_start[i] <- data$depth_interval_start[i - 1]
      data$depth_interval_end[i] <- data$depth_interval_end[i - 1]
    }
  }
  return(data)
}

# FFunction for Calculating Total Overburden Pressure for Each Sample Using Frozen Bulk Density and Depth Intervals
calculate_in_situ_stress <- function(data) {
  data <- data %>% mutate(in_situ_sigma_v = (depth_interval_end - depth_interval_start) * frozen_bulk_density_rep * 9.81 / 1000)
  return(data)
}

# All Test Results Data set (thaw_settlement_test_result.csv) Creation Functions----

create_filtered_test_results <- function(data) {
  test_all <- list()
  for (i in 1:nrow(data)) {
    test_data <- data.frame(
      unique_id = rep(data$unique_id[i], each = 7),
      loading_step_number = 1:7,
      sigma_v = c(data$sigma_v_1[i], data$sigma_v_2[i], data$sigma_v_3[i], data$sigma_v_4[i], data$sigma_v_5[i], data$sigma_v_6[i], data$sigma_v_7[i]),
      deformation = c(data$delta_h_1[i], data$delta_h_2[i], data$delta_h_3[i], data$delta_h_4[i], data$delta_h_5[i], data$delta_h_6[i], data$delta_h_7[i]),
      strain = c(data$epsilon_1_calc[i], data$epsilon_2_calc[i], data$epsilon_3_calc[i], data$epsilon_4_calc[i], data$epsilon_5_calc[i], data$epsilon_6_calc[i], data$epsilon_7_calc[i]),
      void_ratio = c(data$e_1_calc[i], data$e_2_calc[i], data$e_3_calc[i], data$e_4_calc[i], data$e_5_calc[i], data$e_6_calc[i], data$e_7_calc[i])
    )
    test_data <- test_data[!is.na(test_data$sigma_v), ]
    if (nrow(test_data) > 0) test_all[[i]] <- test_data
  }
  test_all_df <- do.call(rbind, test_all)
  test_all_df <- test_all_df[order(test_all_df$unique_id, test_all_df$sigma_v), ]

  # Assign the outputs to global variables
  assign("test_all", test_all, envir = .GlobalEnv)
  assign("test_all_df", test_all_df, envir = .GlobalEnv)
}

# Function for Saving Test Results to Include in the PTS Dataset

save_test_results <- function(data, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_file <- file.path(output_dir, "thaw_settlement_test_result.csv")
  write.csv(data, output_file, row.names = FALSE)
  message("Test results saved to: ", output_file)
}


# Comparative Parameter Calculation Functions ----

# Calculate A0, a0, and R-squared
calculate_A0_a0 <- function(data, test_results) {
  A0 <- numeric(nrow(data))
  a0 <- numeric(nrow(data))
  R_sq <- numeric(nrow(data))

  for (i in 1:nrow(data)) {
    sigma_v <- test_results[[i]]$sigma_v
    strain <- test_results[[i]]$strain
    valid_indices <- which(!is.na(sigma_v) & !is.na(strain))

    while (length(valid_indices) >= 3) {
      x <- sigma_v[valid_indices]
      y <- strain[valid_indices]

      model <- lm(y ~ x)
      R_sq[i] <- summary(model)$r.squared

      if (R_sq[i] >= 0.99 || length(valid_indices) <= 3) {
        A0[i] <- coef(model)[1] # Intercept
        a0[i] <- coef(model)[2] # Slope
        break
      } else {
        valid_indices <- valid_indices[-1]
      }
    }

    if (length(valid_indices) < 3) {
      A0[i] <- NA
      a0[i] <- NA
      R_sq[i] <- NA
    }
  }

  data <- data %>%
    mutate(A0 = A0, a0 = a0, R_sq = R_sq)

  return(data)
}

# Calculate Cc, sigma_v_th, and R-squared for semi-log fitting
calculate_Cc_sigma_v_th <- function(data, test_results) {
  Cc <- numeric(nrow(data))
  R_sq_semilog <- numeric(nrow(data))
  sigma_v_th_init <- numeric(nrow(data))

  for (i in 1:nrow(data)) {
    sigma_v <- test_results[[i]]$sigma_v
    void_ratio <- test_results[[i]]$void_ratio
    valid_indices <- which(!is.na(sigma_v))

    if (length(valid_indices) < 3 || all(is.na(void_ratio))) {
      Cc[i] <- NA
      R_sq_semilog[i] <- NA
      sigma_v_th_init[i] <- NA
    } else {
      x <- sigma_v[valid_indices]
      y <- void_ratio[valid_indices]
      model <- lm(y ~ log10(x))

      sigma_v_th_init[i] <- 10^(((data$frozen_void_ratio_calc[i] / 1.09) - coef(model)[1]) / coef(model)[2])
      Cc[i] <- coef(model)[2]
      R_sq_semilog[i] <- summary(model)$r.squared
    }
  }

  data <- data %>%
    mutate(Cc = Cc, sigma_v_th_init = sigma_v_th_init, R_sq_semilog = R_sq_semilog)

  return(data)
}

# Classify samples based on ice level
classify_ice_level <- function(data) {
  data$ice_level <- ifelse(
    is.na(data$sigma_v_th_init), NA,
    ifelse(data$sigma_v_th_init <= 1, "Ice-rich", "Ice-poor")
  )
  return(data)
}



# Calculate threshold initial void ratio
calculate_threshold_void_ratio <- function(data, test_results) {
  e_th <- numeric(nrow(data))

  for (i in 1:nrow(data)) {
    sigma_v <- test_results[[i]]$sigma_v
    void_ratio <- test_results[[i]]$void_ratio
    valid_indices <- which(!is.na(sigma_v))

    if (length(valid_indices) < 3 || all(is.na(void_ratio))) {
      e_th[i] <- NA
    } else {
      x <- sigma_v[valid_indices]
      y <- void_ratio[valid_indices]
      model <- lm(y ~ log10(x))

      if (!is.na(data$ice_level[i])) {
        if (data$ice_level[i] == "Ice-rich") {
          e_th[i] <- predict(model, newdata = data.frame(x = 1))
        } else {
          e_th[i] <- predict(model, newdata = data.frame(x = data$sigma_v_th_init[i]))
        }
      } else {
        e_th[i] <- NA
      }
    }
  }

  data <- data %>%
    mutate(e_th = e_th)

  return(data)
}




# Thaw Strain Components Calculation Functions----

calculate_thaw_strain_components <- function(data) {
  data <- data %>%
    mutate(
      excess_water_strain = round((91.74 * frozen_void_ratio_calc - 100 * e_th) / (1 + frozen_void_ratio_calc), 2),
      sigma_v_th = ifelse(ice_level == "Ice-rich", 1, sigma_v_th_init),
      soil_compression_strain_at_100_kpa = round((Cc * log10(sigma_v_th / 100)) * 100 / (1 + frozen_void_ratio_calc), 2),
      total_strain_at_100_kpa = excess_water_strain + phase_change_strain + soil_compression_strain_at_100_kpa
    )
  return(data)
}

# Empirical Methods Calculation Functions----

# Function to deduplicate constants needed for generating thaw strains values using each empirical method
deduplicate_constants <- function(df, method, group_col, additional_cols) {
  df %>%
    filter(method == !!method, !is.na(!!sym(group_col))) %>%
    group_by(!!sym(group_col), soil_group) %>%
    summarise(across(all_of(additional_cols), first), .groups = "drop")
}

# Function to generate thaw strain data for Luscher and Afifi (1973)
generate_luscher_data <- function(inputs) {
  inputs %>%
    group_by(group_luscher) %>%
    do({
      FBD_seq <- seq(750, 2250, 10)
      thaw_strain <- ifelse(
        FBD_seq <= .$input_limits,
        .$slope_1 * FBD_seq + .$intercept_1,
        .$slope_2 * FBD_seq + .$intercept_2
      )
      data.frame(
        FBD = FBD_seq,
        sub_group = .$group_luscher,
        soil_group = .$soil_group,
        thaw_strain = thaw_strain,
        method = "Luscher and Afifi (1973)"
      )
    })
}

# Function to generate thaw strain data for Nelson et al. (1983)
generate_nelson_data <- function(inputs) {
  inputs %>%
    group_by(group_nelson) %>%
    do({
      FDD_seq <- seq(.$FDD_min, .$FDD_max, 10)
      porosity <- 1 - (FDD_seq / .$gs)
      void_ratio <- porosity / (1 - porosity)
      w <- void_ratio / (.$gs * 0.001) * 100
      FBD <- FDD_seq * (1 + (w / 100))

      thaw_strain <- .$a1 * porosity^2 + .$a2 * porosity +
        .$a3 * (porosity^2 * (w / 100)) + .$a4 * (porosity / 100) +
        .$a5 * (porosity / w) + .$a6

      data.frame(
        Gs = .$gs,
        FDD = FDD_seq,
        sub_group = .$group_nelson,
        soil_group = .$soil_group,
        porosity = porosity,
        void_ratio = void_ratio,
        w = w,
        FBD = FBD,
        thaw_strain = thaw_strain,
        method = "Nelson et al. (1983)"
      )
    })
}

# Function to generate thaw strain data for Hanna et al. (1983)
generate_hanna_data <- function(inputs) {
  inputs %>%
    group_by(group_hanna) %>%
    do({
      vw_seq <- seq(0.5, 100.5, 0.5)
      thaw_strain <- ifelse(
        vw_seq <= .$input_limits,
        .$slope_1 * vw_seq + .$intercept_1,
        .$slope_2 * vw_seq + .$intercept_2
      )
      data.frame(
        vw = vw_seq,
        sub_group = .$group_hanna,
        soil_group = .$soil_group,
        thaw_strain = thaw_strain,
        method = "Hanna et al. (1983)",
        major_category = ifelse(
          .$soil_group %in% c("Fine-grained", "Coarse-grained"),
          "Sedimentary Soils", "Peat and Organic Soils"
        )
      )
    })
}


# RMDS and Bias Calculation Functions----

# Functions for Calculating Root Mean Squared Deviation (RMSD) to Compare Empirical Methods with PTS Data
rmsd_x_y <- function(x, y) {
  sqrt(sum((x - y)^2, na.rm = TRUE) / sum(!is.na(y)))
}

# Functions for Calculating Bias to Compare Empirical Methods with PTS Data
bias_x_y <- function(x, y) {
  mean(x - y, na.rm = TRUE)
}

# RMSD and Bias Summarizing Functions----

calculate_rmsd_and_bias <- function(data, group_col, strain_col, epsilon_col, method_name, filter_soil = TRUE) {
  if (filter_soil) {
    data <- data %>% filter(soil_group != "Peat")
  }
  data %>%
    filter(
      !!sym(group_col) != "NO",
      !is.na(!!sym(group_col))
    ) %>%
    summarize(
      method = method_name,
      RMSD_e_logP = round(rmsd_x_y(!!sym(strain_col), !!sym(epsilon_col)), 2),
      Bias_e_logP = round(bias_x_y(!!sym(strain_col), !!sym(epsilon_col)), 2),
      count = sum(!is.na(!!sym(strain_col))),
      .by = !!sym(group_col)
    )
}

# Figures and Tables Saving Functions ----

save_figure <- function(plot, filename, path, width = 8, height = 6) {
  ggsave(filename = file.path(path, filename), plot = plot, width = width, height = height)
}

# Function for Summarizing Descriptive Statistics
summarize_data <- function(data, variable, group_by_var) {
  data %>%
    summarise(
      var = deparse(substitute(variable)),
      sample_size = sum(!is.na({{ variable }})),
      min = min({{ variable }}, na.rm = TRUE),
      max = max({{ variable }}, na.rm = TRUE),
      mean = round(mean({{ variable }}, na.rm = TRUE), 2),
      sd = round(sd({{ variable }}, na.rm = TRUE), 2),
      skewness = round(skewness({{ variable }}, na.rm = TRUE), 2),
      .by = {{ group_by_var }}
    )
}
