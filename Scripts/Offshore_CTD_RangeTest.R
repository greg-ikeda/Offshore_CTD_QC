# This test rejects data that is outside of a given range.
# The min and max of the range are extreme values. For example, DO will never reasonably be below zero, or above 20 mg/L

# source(here("Offshore_CTDQC_DataPrep.R"))

QC_test <- "range_test"
test_save_dir <- paste0(save_folder, "/", QC_test)

# Flag data based on extreme ranges, as defined in rv ---------------------
  # Applies "Rej" flag to data outside the extreme minimum (rv[1]) and extreme maximum (rv[2]) 
  # Flags are appended to a new column "###_Qual_Auto" for each param
  # Creates new "flag_reason" column that denotes every param that recieved a flag

extreme_df <- working_data %>%
  mutate(
    Depth_Qual_Auto = case_when(
      Depth < rv$Depth[1] ~ "Rej",
      Depth > rv$Depth[2] ~ "Rej"),
    Chlorophyll_Qual_Auto = case_when(
      Chlorophyll < rv$Chlorophyll[1] ~ "Rej",
      Chlorophyll > rv$Chlorophyll[2] ~ "Rej"),
    Density_Qual_Auto = case_when(
      Density < rv$Density[1] ~ "Rej",
      Density > rv$Density[2] ~ "Rej"),
    DO_Qual_Auto = case_when(
      DO < rv$DO[1] ~ "Rej",
      DO > rv$DO[2] ~ "Rej"),
    SigmaTheta_Qual_Auto = case_when(
      SigmaTheta < rv$SigmaTheta[1] ~ "Rej",
      SigmaTheta > rv$SigmaTheta[2] ~ "Rej"),
    Light_Transmission_Qual_Auto = case_when(
      Light_Transmission < rv$Light_Transmission[1] ~ "Rej",
      Light_Transmission > rv$Light_Transmission[2] ~ "Rej"),
    PAR_Qual_Auto = case_when(
      PAR < rv$PAR[1] ~ "Rej",
      PAR > rv$PAR[2] ~ "Rej"),
    Surface_PAR_Qual_Auto = case_when(
      Surface_PAR < rv$Surface_PAR[1] ~ "Rej",
      Surface_PAR > rv$Surface_PAR[2] ~ "Rej"),
    Salinity_Qual_Auto = case_when(
      Salinity <  rv$Salinity[1] ~ "Rej",
      Salinity > rv$Salinity[2] ~ "Rej"),
    Temperature_Qual_Auto = case_when(
      Temperature < rv$Temperature[1] ~ "Rej",
      Temperature > rv$Temperature[2] ~ "Rej"),
    Turbidity_Qual_Auto = case_when(
      Turbidity < rv$Turbidity[1] ~ "Rej",
      Turbidity > rv$Turbidity[2] ~ "Rej"),
    NO23_Qual_Auto = case_when(
      NO23 < rv$NO23[1] ~ "Rej",
      NO23 > rv$NO23[2] ~ "Rej")) %>%
  mutate(flag_reason = "",
         flag_reason = if_else(!is.na(Depth_Qual_Auto), paste0(flag_reason, "Depth_"), flag_reason),
         flag_reason = if_else(!is.na(Chlorophyll_Qual_Auto), paste0(flag_reason, "chl_"), flag_reason),
         flag_reason = if_else(!is.na(Density_Qual_Auto), paste0(flag_reason, "density_"), flag_reason),
         flag_reason = if_else(!is.na(DO_Qual_Auto), paste0(flag_reason, "DO_"), flag_reason),
         flag_reason = if_else(!is.na(SigmaTheta_Qual_Auto), paste0(flag_reason, "SigmaT_"), flag_reason),
         flag_reason = if_else(!is.na(Light_Transmission_Qual_Auto), paste0(flag_reason, "Light_"), flag_reason),
         flag_reason = if_else(!is.na(PAR_Qual_Auto), paste0(flag_reason, "PAR_"), flag_reason),
         flag_reason = if_else(!is.na(Surface_PAR_Qual_Auto), paste0(flag_reason, "SPAR_"), flag_reason),
         flag_reason = if_else(!is.na(Salinity_Qual_Auto), paste0(flag_reason, "Sal_"), flag_reason),
         flag_reason = if_else(!is.na(Temperature_Qual_Auto), paste0(flag_reason, "Temp_"), flag_reason),
         flag_reason = if_else(!is.na(Turbidity_Qual_Auto), paste0(flag_reason, "Turb_"), flag_reason),
         flag_reason = if_else(!is.na(NO23_Qual_Auto), paste0(flag_reason, "NO23_"), flag_reason)) %>%
  filter(flag_reason != "") %>%
  mutate(flag_reason = str_sub(flag_reason, end = -2))

# Add _Qual_Auto flags to the working_data, so that the full profile is available to plot
extreme_df_full <- left_join(working_data, extreme_df)

# Makes a barplot to see the most common culprits, if any

extreme_rej_plot <- ggplotly(ggplot(extreme_df)+
                          geom_bar(aes(x = flag_reason))+
                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
ggplotly(extreme_rej_plot)
htmlwidgets::saveWidget(ggplotly(extreme_rej_plot), 
                        title = paste0("Extreme Rejected: ", station, "_", Sys.Date()), 
                        file = paste0(test_save_dir, "/",station, "_", Sys.Date(), ".html"))

# Saves a .csv file with the results
write_csv(extreme_df, paste0(test_save_dir, "/Extreme_values_range_test.csv"))

# Plot Data and save as png files---------------------------------------------------------------

# Saves to a png file in the save_folder
for(cast in profile_dates){
  cast_doi <- ymd(cast)
  profile_of_interest <- extreme_df_full %>%
    filter(date(Sampledate) == cast_doi)
  plot_errors_multipanel(profile_of_interest, cast_doi)
}

# Moves png files from the save_folder to the appropriate folder for this test
move_png_files(save_folder, QC_test, station)

shell.exec(here(save_folder, QC_test)) 
