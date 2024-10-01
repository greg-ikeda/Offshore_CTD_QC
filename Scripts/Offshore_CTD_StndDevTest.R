# source(here("Offshore_CTDQC_DataPrep.R"))

QC_test <- "stnd_dev"
test_save_dir <- paste0(save_folder, "/", QC_test)

# Flag data based on extreme ranges, as defined in rv ---------------------
# Applies "q" flag to data outside  minimum (mean - rv[3]*sd) and extreme maximum (mean + rv[3]*sd) 
# Flags are appended to a new column "###_Qual_Auto" for each param
# Creates new "flag_reason" column that denotes every param that received a flag

stnddev_df <- working_data %>%
  mutate(
    Chlorophyll_Qual_Auto = case_when(
      Chlorophyll < (Chlorophyll_mean - rv$Chlorophyll[3]*Chlorophyll_sd) ~ "q",
      Chlorophyll > (Chlorophyll_mean + rv$Chlorophyll[3]*Chlorophyll_sd) ~ "q"),
    Density_Qual_Auto = case_when(
      Density < (Density_mean - rv$Density[3]*Density_sd) ~ "q",
      Density > (Density_mean + rv$Density[3]*Density_sd) ~ "q"),
    DO_Qual_Auto = case_when(
      DO < (DO_mean - rv$DO[3]*DO_sd) ~ "q",
      DO > (DO_mean + rv$DO[3]*DO_sd) ~ "q"),
    Light_Transmission_Qual_Auto = case_when(
      Light_Transmission < (Light_Transmission_mean - rv$Light_Transmission[3]*Light_Transmission_sd) ~ "q",
      Light_Transmission > (Light_Transmission_mean + rv$Light_Transmission[3]*Light_Transmission_sd) ~ "q"),
    PAR_Qual_Auto = case_when(
      PAR < (PAR_mean - rv$PAR[3]*PAR_sd) ~ "q",
      PAR > (PAR_mean + rv$PAR[3]*PAR_sd) ~ "q"),
    Surface_PAR_Qual_Auto = case_when(
      Surface_PAR < (Surface_PAR_mean - rv$Surface_PAR[3]*Surface_PAR_sd) ~ "q",
      Surface_PAR > (Surface_PAR_mean + rv$Surface_PAR[3]*Surface_PAR_sd) ~ "q"),
    Salinity_Qual_Auto = case_when(
      Salinity < (Salinity_mean - rv$Salinity[3]*Salinity_sd) ~ "q",
      Salinity > (Salinity_mean + rv$Salinity[3]*Salinity_sd) ~ "q"),
    Temperature_Qual_Auto = case_when(
      Temperature < (Temperature_mean - rv$Temperature[3]*Temperature_sd) ~ "q",
      Temperature > (Temperature_mean + rv$Temperature[3]*Temperature_sd) ~ "q"),
    Turbidity_Qual_Auto = case_when(
      Turbidity < (Turbidity_mean - rv$Turbidity[3]*Turbidity_sd) ~ "q",
      Turbidity > (Turbidity_mean + rv$Turbidity[3]*Turbidity_sd) ~ "q"),
    NO23_Qual_Auto = case_when(
      NO23 < (NO23_mean - rv$NO23[3]*NO23_sd) ~ "q",
      NO23 > (NO23_mean + rv$NO23[3]*NO23_sd) ~ "q"),
    SigmaTheta_Qual_Auto = case_when(
      SigmaTheta < (SigmaTheta_mean - 5*SigmaTheta_sd) ~ "q",
      SigmaTheta > (SigmaTheta_mean + 5*SigmaTheta_sd) ~ "q")) %>%
  mutate(flag_reason = "",
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
  mutate(flag_reason = str_sub(flag_reason, end = -2)) %>%
  select(flag_reason, everything())

stnddev_df_full <- left_join(working_data, stnddev_df)

# Creates a barplot of flag_reason, to see what the main culprits are
stnddev_q <- ggplot(stnddev_df)+
  geom_bar(aes(x = flag_reason))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
htmlwidgets::saveWidget(ggplotly(stnddev_q), 
                        title = paste0("Standard Dev Flagged: ", station, "_", Sys.Date()), 
                        file = paste0(test_save_dir, "/",station, "_", Sys.Date(), ".html"))

write_csv(stnddev_df, paste0(test_save_dir, "/standard_deviation_values.csv"))

# Plot Data and save as png files---------------------------------------------------------------
# Saves to a png file in the save_folder
for(cast in profile_dates){
  cast_doi <- ymd(cast)
  profile_of_interest <- stnddev_df_full %>%
    filter(date(Sampledate) == cast_doi)
  plot_errors_multipanel_sd_shading(profile_of_interest, cast_doi)
}

# Moves png files to the appropriate folder
move_png_files(save_folder, QC_test, station)

shell.exec(here(save_folder, QC_test)) 
