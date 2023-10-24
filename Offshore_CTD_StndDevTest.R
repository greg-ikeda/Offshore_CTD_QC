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

shell.exec(save_folder)

# Scratchpad --------------------------------------------------------------

# 
# plot_data <- ggplot(data = stnddev_df_full %>%
#                       filter(Date == ymd("2023-05-15")))+
#   scale_x_reverse()+
#   coord_flip()+
#   geom_ribbon(aes(x = BinDepth,
#                   y = DO_mean,
#                   ymin = DO_mean - 2*DO_sd,
#                   ymax = DO_mean + 2*DO_sd),
#               alpha = 0.2)+
#   geom_line(aes(x = BinDepth,
#                 y = DO_mean),
#             linewidth = 2,
#             alpha = 0.1,
#             color = "blue")+
#   geom_line(data = stnddev_df_full %>%
#               filter(Date == ymd("2023-05-15")),
#             aes(x = BinDepth,
#                 y = DO),
#             linewidth = 1.2)+
#   ggtitle("2023-05-15")
# plot_data
# 
# test_extreme <- extreme_df %>%
#   filter(Date > mdy("1-1-2012"))
# 
# for(errordate in unique(test_extreme$Date)){
#   date <- as.Date(errordate)
#   errormonth <- month(date)
#   baseline_errormonth <- baseline %>%
#     filter(Month == errormonth)
#   plot_data <- ggplot(baseline_errormonth)+
#     geom_line(aes(x = BinDepth,
#                   y = DO_mean),
#               linewidth = 2,
#               alpha = 0.1,
#               color = "blue")+
#     geom_ribbon(aes(x = BinDepth,
#                     y = DO_mean,
#                     ymin = DO_mean - (2*DO_sd),
#                     ymax = DO_mean + (2*DO_sd)),
#                 alpha = 0.3)+
#     geom_line(data = test_extreme %>%
#                 filter(Date == errordate),
#               aes(x = BinDepth,
#                   y = DO))+
#     scale_x_reverse()+
#     coord_flip()+
#     ggtitle(paste0(date))
#   print(plot_data)
# }
# 
# 
# errorplotter <- function(date_input, param_input){
#   
#   param_input <- "DO"
#   date_input <- "2016-03-09"
#   
#   parm_to_plot <- as.name(param_input)
#   mean_to_plot <- as.name(paste0(param_input, "_mean"))
#   sd_to_plot <- as.name(paste0(param_input, "_sd"))
#   qual_to_plot <- as.name(paste0(param_input, "_Qual_Auto"))
#   
#   baseline_errormonth <- baseline %>%
#     filter(Month == month(as.Date(date_input)))
#   
#   plot_data <- ggplot(baseline_errormonth)+
#     geom_ribbon(aes(x = BinDepth,
#                     y = !!mean_to_plot,
#                     ymin = !!mean_to_plot - 2*!!sd_to_plot,
#                     ymax = !!mean_to_plot + 2*!!sd_to_plot),
#                 alpha = 0.2)+
#     geom_line(aes(x = BinDepth,
#                   y = !!mean_to_plot),
#               linewidth = 2,
#               alpha = 0.1,
#               color = "blue")+
#     geom_line(data = test_extreme %>%
#                 filter(Date == errordate),
#               aes(x = BinDepth,
#                   y = !!param_input),
#               linewidth = 1.2)+
#     geom_line(data = test_extreme %>%
#                 filter(Date == errordate),
#               aes(x = BinDepth,
#                   y = !!param_input),
#               linewidth = 1.2,
#               alpha = 0.1)+
#     geom_point(data = test_extreme %>%
#                  filter(Date == errordate),
#                aes(x = BinDepth,
#                    y = !!param_input),
#                size = 0.3)+
#     scale_x_reverse()+
#     coord_flip()+
#     ggtitle(date_input)
#   print(plot_data)
#   # ggplotly(plot_data)
# }
# errorplotter("2016-03-09", colnames(test_extreme)[8])
# 
# test <- baseline %>%
#   filter(Month == 1)
# 
# test_plot2 <- ggplot(test)+
#   geom_line(aes(x = BinDepth,
#                 y = DO_mean))+
#   geom_ribbon(aes(x = BinDepth,
#                   y = DO_mean,
#                   ymin = (DO_mean - (2*DO_sd)),
#                   ymax = (DO_mean + (2*DO_sd))),
#               alpha = 0.3)+
#   scale_x_reverse()+
#   coord_flip()
# test_plot2
# 
# plotly::ggplotly(test_plot2)
