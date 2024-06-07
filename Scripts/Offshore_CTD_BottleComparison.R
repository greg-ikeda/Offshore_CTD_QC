# source(here("Offshore_CTDQC_DataPrep.R"))

# depth_bin function from the contour script
depth_bin <- function(depth, width) {
  end <- ceiling(max(depth) / width) * width
  break_points <- seq(from = 0, to = end, by = width)
  bins <- cut(depth, breaks = break_points)
  mid <- seq(0 + bin_width/2, end - bin_width/2, bin_width)
  names(mid) <- levels(bins)
  out <- data.frame(bins, unname(mid[bins]))
  names(out) <- c("Bin", "BinDepth")
  return(out)
}

QC_test <- "discrete_comparison"
test_save_dir <- paste0(save_folder, "/", QC_test)


# Tidy and Prep Bottle Data -----------------------------------------------
# Put bottle data into a tidier wide-format
# Note: bottle_data is created in the Offshore_CTDQC_DataPrep.R
# Note: Changed <MDL NA values to the MDL value for that parameter

working_bottle_data <- bottle_data %>% 
  mutate(detect = !grepl("<MDL", QfrCode), 
         # Changes the <MDL NA values to the that parameter's MDL value
         Value = if_else(detect, Value, Mdl)) %>%
  select(ParmDisplayName, Value, Units, QualityId, QfrCode, CollectDateTime, Depth, LabSampleNum) %>%
  pivot_wider(names_from = c(ParmDisplayName, Units),
              values_from = Value,
              id_cols = c(CollectDateTime, LabSampleNum, Depth))  %>%
  mutate(datetime = ymd_hms(CollectDateTime),
         num_date = as.numeric(datetime)) 

# Join bottle and CTD tibbles ---------------------------------------------

# working_data <- working_data %>%
#   mutate(Sampledate = ymd_hms(Sampledate),
#          date = date(Sampledate),
#          num_date_ctd = as.numeric(Sampledate))

CTD_and_bottle_data <- left_join(working_data, working_bottle_data,
                                 by = c('Sampledate'='datetime'),
                                 relationship = "many-to-many") %>%
  select(`Depth.x`, `Depth.y`, everything()) 

# Binning ---------------------------------------------------------
# Note: profile_dates is created in the CTD_Data_Appender script

# Sets the ± tolerance level for the bin (e.g. 2 = bottle depth ±2)
tolerance <- 2

for(doi in profile_dates){
  # filters the bottle data into just the specific date of interest 
  bottle_doi <- working_bottle_data %>%
    filter(date(CollectDateTime) == ymd(doi))
  # filters the CTD data into just the specific date of interest 
  CTD_doi <- working_data %>%
    filter(cast_date == ymd(doi))
  
  # grabs the depths of the bottle samples in the date of interest
  depths <- unique(bottle_doi$Depth)
  # Creates a tibble that contains the depth, bottom of the bin ("low"), and top of the bin ("high")
  depth_tibble <- tibble(depths) %>%
    mutate(low = depths - tolerance,
           high = depths + tolerance)
  
  # Creates the "CTD_binned" tibble, which contains the data within depth bins 
  for(i in 1:nrow(depth_tibble)){
    CTD <- CTD_doi %>%
      filter(Depth >= depth_tibble$low[i],
             Depth <= depth_tibble$high[i]) %>%
      mutate(depth_bin = depth_tibble$depths[i])
    if(i == 1){
      CTD_binned <- CTD
    } else {
      CTD_binned <- rbind(CTD_binned, CTD)
    }
  }
  
  # Averages the CTD data bins into single averages.
  # Note that this will combine any duplicate samples into a single bin (e.g. replicate taken at 200 m)
  CTD_binned_averaged <- CTD_binned %>%
    group_by(depth_bin) %>%
    summarize(chl_mean = mean(Chlorophyll, na.rm = T),
              DO_mean = mean(DO, na.rm = T),
              NO23_mean = mean(NO23, na.rm = T),
              sal_mean = mean(Salinity, na.rm = T))
  
  # Rejoin the bin averaged data into with the bottle data for this cast
  binned_ctd_and_bottle <- left_join(bottle_doi, CTD_binned_averaged,
                                     by = c('Depth'='depth_bin')) %>%
    arrange(by = Depth) %>%
    mutate(chl_diff = chl_mean - `Chlorophyll a_ug/L`,
           DO_diff = DO_mean - `Dissolved Oxygen_mg/L`,
           NO23_diff = NO23_mean - `Nitrite + Nitrate Nitrogen_mg/L`,
           sal_diff = sal_mean - `Salinity_PSS`) %>%
    select(CollectDateTime,
           Depth,
           matches("_diff"),
           everything())
  
  # Write a csv file containing the binned_ctd_and_bottle data
  write_csv(binned_ctd_and_bottle, 
            paste0(test_save_dir, "/", doi, "_bottle_comparison_data.csv"),
            na = "")
  
  # Create a tibble that contains all of the binned CTD and bottle data, for plotting
  if(doi == profile_dates[1]){
    all_binned_ctd_and_bottle <- binned_ctd_and_bottle
  } else{
    all_binned_ctd_and_bottle <- rbind(all_binned_ctd_and_bottle, binned_ctd_and_bottle)
  }
}


# Apply Quality Flags to binned CTD and bottle based on difference --------

# List containing cutoff values for CTD bin - bottle that determine "q" and "Rej" flags.
difference_cutoff <- list(
  Chlorophyll = c(0.2, 2.0),
  DO = c(0.5, 1.0),
  NO23 = c(0.034, 0.60),
  Salinity = c(0.01, 0.05))

all_binned_ctd_and_bottle_flagged <- all_binned_ctd_and_bottle %>%
  mutate(
    Chlorophyll_Qual_Auto = case_when(
      abs(chl_diff) > difference_cutoff$Chlorophyll[1] ~ "q",
      abs(chl_diff) > difference_cutoff$Chlorophyll[2] ~ "Rej"),
    DO_Qual_Auto = case_when(
      abs(DO_diff) > difference_cutoff$DO[1] ~ "q",
      abs(DO_diff) > difference_cutoff$DO[2] ~ "Rej"),
    NO23_Qual_Auto = case_when(
      abs(NO23_diff) > difference_cutoff$NO23[1] ~ "q",
      abs(NO23_diff) > difference_cutoff$NO23[2] ~ "Rej"),
    Salinity_Qual_Auto = case_when(
      abs(sal_diff) > difference_cutoff$Salinity[1] ~ "q",
      abs(sal_diff) > difference_cutoff$Salinity[2] ~ "Rej"))

# Plotting Data -----------------------------------------------------------


# Plots chl, DO, sal, NO23, temp, sigmaTheta, density, light_t, and PAR in a single figure 

plot_errors_multipanel_bottle <- function(df, bottle_df, date_input){
  date_to_plot <- ymd(date_input)
  
  CTD_plot_data <- df %>%
    filter(cast_date == date_to_plot)
  
  bottle_ctdbin_data <- bottle_df %>%
    filter(date(CollectDateTime) == date_to_plot)

  # Chlorophyll
  chl <- ggplot(data = CTD_plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Chlorophyll, y = Depth)) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = expression(Chlorophyll~(mg/m^{3})), y = "Depth (m)") +
    # Adding in bottle, CTD bins, and quality flags 
    geom_point(data = bottle_ctdbin_data,
               aes(x = `chl_mean`,
                   y = Depth,
                   fill = Chlorophyll_Qual_Auto,
                   size = Chlorophyll_Qual_Auto),
               shape = 23) +
    scale_fill_manual(values = c("q" = "yellow", "Rej" = "red"),
                      na.value = "green")+
    scale_size_manual(values = c("q" = 5, "Rej" = 6),
                      na.value = 4)+
    geom_point(data = bottle_ctdbin_data,
               aes(x = `Chlorophyll a_ug/L`, 
                   y = Depth,
                   fill = Chlorophyll_Qual_Auto),
               size = 2.5,
               shape = 24)
  # Dissolved Oxygen
  DO <- ggplot(data = CTD_plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = DO, y = Depth)) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Dissolved Oxygen (mg/L)", y = "")+
    # Adding in bottle, CTD bins, and quality flags 
    geom_point(data = bottle_ctdbin_data,
               aes(x = `DO_mean`, 
                   y = Depth,
                   fill = DO_Qual_Auto,
                   size = DO_Qual_Auto),
               shape = 23) +
    scale_fill_manual(values = c("q" = "yellow", "Rej" = "red"),
                      na.value = "green")+
    scale_size_manual(values = c("q" = 3, "Rej" = 4),
                      na.value = 2)+
    geom_point(data = bottle_ctdbin_data,
               aes(x = `Dissolved Oxygen_mg/L`, 
                   y = Depth,
                   fill = DO_Qual_Auto),
               size = 2.5,
               shape = 24)

  #Nitrate + Nitrite
  NO23 <- ggplot(data = CTD_plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = NO23, y = Depth)) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Nitrate+Nitrite N (mg/L)", y = "Depth (m)") +
    # Adding in bottle, CTD bins, and quality flags 
    geom_point(data = bottle_ctdbin_data,
               aes(x = `NO23_mean`, 
                   y = Depth,
                   fill = NO23_Qual_Auto,
                   size = NO23_Qual_Auto),
               shape = 23) +
    scale_fill_manual(values = c("q" = "yellow", "Rej" = "red"),
                      na.value = "green")+
    scale_size_manual(values = c("q" = 3, "Rej" = 4),
                      na.value = 2)+
    geom_point(data = bottle_ctdbin_data,
               aes(x = `Nitrite + Nitrate Nitrogen_mg/L`, 
                   y = Depth,
                   fill = NO23_Qual_Auto),
               size = 2.5,
               shape = 24)
  
  # Salinity
  sal <- ggplot(data = CTD_plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Salinity, y = Depth)) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Salinity (PSS)", y = "") +
    # Adding in bottle, CTD bins, and quality flags 
    geom_point(data = bottle_ctdbin_data,
               aes(x = `sal_mean`, 
                   y = Depth,
                   fill = Salinity_Qual_Auto,
                   size = Salinity_Qual_Auto),
               shape = 23) +
    scale_fill_manual(values = c("q" = "yellow", "Rej" = "red"),
                      na.value = "green")+
    scale_size_manual(values = c("q" = 3, "Rej" = 4),
                      na.value = 2)+
    geom_point(data = bottle_ctdbin_data,
               aes(x = `Salinity_PSS`, 
                   y = Depth,
                   fill = Salinity_Qual_Auto),
               size = 2.5,
               shape = 24)
  
  png(paste0(save_folder, "/", station, "_", date_to_plot, "_", QC_test, ".png"), width = 1500, height = 1500)
  multiplot(chl, NO23, DO, sal, cols = 2)
  dev.off()
}

for(cast in profile_dates){
  cast_doi <- ymd(cast)
  plot_errors_multipanel_bottle(working_data, all_binned_ctd_and_bottle_flagged, cast_doi)
}