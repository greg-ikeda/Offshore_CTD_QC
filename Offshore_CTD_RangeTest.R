# source(here("Offshore_CTDQC_DataPrep.R"))

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

# Creates a df for each individual profile. Flagging criteria are the same as above  ----------------------------------------
  # Each profile is a list element of "extreme_profiles" 

extreme_profiles <- list()
temp_extreme_profiles <- list()
for(i in seq(1:length(profiles))){
  temp_extreme_profiles[[i]] <- profiles[[i]] %>%
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
}
count <- 0
for(i in seq(1:length(temp_extreme_profiles))){
  if(nrow(temp_extreme_profiles[[i]]) != 0){
    count <- count + 1
    extreme_profiles[[count]] <- temp_extreme_profiles[[i]]
  }
}

for(i in seq(1:length(extreme_profiles))){
  print(unique(extreme_profiles[[i]]$Date))
}
rm(temp_extreme_profiles)

# Makes a barplot to see the most common culprits
extreme_rej_plot <- ggplotly(ggplot(extreme_df)+
                          geom_bar(aes(x = flag_reason))+
                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
ggplotly(extreme_rej_plot)

# Saves a .csv file with the results
write_csv(extreme_df, paste0(save_folder, "/Extreme_values.csv"))

# Flagging based on standard deviation range ------------------------------

stnddev_df <- working_data %>%
  mutate(
    Chlorophyll_Qual_Auto = case_when(
      Chlorophyll < (Chlorophyll_mean - 2*Chlorophyll_sd) ~ "q",
      Chlorophyll > (Chlorophyll_mean + 2*Chlorophyll_sd) ~ "q"),
    Density_Qual_Auto = case_when(
      Density < (Density_mean - 2*Density_sd) ~ "q",
      Density > (Density_mean + 2*Density_sd) ~ "q"),
    DO_Qual_Auto = case_when(
      DO < (DO_mean - 2*DO_sd) ~ "q",
      DO > (DO_mean + 2*DO_sd) ~ "q"),
    Light_Transmission_Qual_Auto = case_when(
      Light_Transmission < (Light_Transmission_mean - 2*Light_Transmission_sd) ~ "q",
      Light_Transmission > (Light_Transmission_mean + 2*Light_Transmission_sd) ~ "q"),
    PAR_Qual_Auto = case_when(
      PAR < (PAR_mean - 2*PAR_sd) ~ "q",
      PAR > (PAR_mean + 2*PAR_sd) ~ "q"),
    Surface_PAR_Qual_Auto = case_when(
      Surface_PAR < (Surface_PAR_mean - 2*Surface_PAR_sd) ~ "q",
      Surface_PAR > (Surface_PAR_mean + 2*Surface_PAR_sd) ~ "q"),
    Salinity_Qual_Auto = case_when(
      Salinity < (Salinity_mean - 2*Salinity_sd) ~ "q",
      Salinity > (Salinity_mean + 2*Salinity_sd) ~ "q"),
    Temperature_Qual_Auto = case_when(
      Temperature < (Temperature_mean - 2*Temperature_sd) ~ "q",
      Temperature > (Temperature_mean + 2*Temperature_sd) ~ "q"),
    Turbidity_Qual_Auto = case_when(
      Turbidity < (Turbidity_mean - 2*Turbidity_sd) ~ "q",
      Turbidity > (Turbidity_mean + 2*Turbidity_sd) ~ "q"),
    NO23_Qual_Auto = case_when(
      NO23 < (NO23_mean - 2*NO23_sd) ~ "q",
      NO23 > (NO23_mean + 2*NO23_sd) ~ "q"),
    SigmaT_Qual_Auto = case_when(
      SigmaTheta < (SigmaTheta_mean - 5*SigmaTheta_sd) ~ "q",
      SigmaTheta > (SigmaTheta_mean + 5*SigmaTheta_sd) ~ "q")) %>%
  mutate(flag_reason = "",
         flag_reason = if_else(!is.na(Chlorophyll_Qual_Auto), paste0(flag_reason, "chl_"), flag_reason),
         flag_reason = if_else(!is.na(Density_Qual_Auto), paste0(flag_reason, "density_"), flag_reason),
         flag_reason = if_else(!is.na(DO_Qual_Auto), paste0(flag_reason, "DO_"), flag_reason),
         flag_reason = if_else(!is.na(SigmaT_Qual_Auto), paste0(flag_reason, "SigmaT_"), flag_reason),
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

# Creates a barplot of flag_reason, to see what the main culprits are
stnddev_q <- ggplot(stnddev_df)+
  geom_bar(aes(x = flag_reason))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplotly(stnddev_q)

write_csv(stnddev_df, paste0(save_folder, "/standard_deviation_values.csv"))
shell.exec(save_folder)


# Plotting baseline +- standard dev, with shading --------

test_extreme <- extreme_df %>%
  filter(Date > mdy("1-1-2012"))

for(errordate in unique(test_extreme$Date)){
  date <- as.Date(errordate)
  errormonth <- month(date)
  baseline_errormonth <- baseline %>%
    filter(Month == errormonth)
  plot_data <- ggplot(baseline_errormonth)+
    geom_line(aes(x = BinDepth,
                  y = DO_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    geom_ribbon(aes(x = BinDepth,
                    y = DO_mean,
                    ymin = DO_mean - (2*DO_sd),
                    ymax = DO_mean + (2*DO_sd)),
                alpha = 0.3)+
    geom_line(data = test_extreme %>%
                filter(Date == errordate),
              aes(x = BinDepth,
                  y = DO))+
    scale_x_reverse()+
    coord_flip()+
    ggtitle(paste0(date))
  print(plot_data)
}


errorplotter <- function(date_input, param_input){
  parm_to_plot <- as.name(param_input)
  mean_to_plot <- as.name(paste0(param_input, "_mean"))
  sd_to_plot <- as.name(paste0(param_input, "_sd"))
  qual_to_plot <- as.name(paste0(param_input, "_Qual_Auto"))
  
  baseline_errormonth <- baseline %>%
    filter(Month == month(as.Date(date_input)))
  
  plot_data <- ggplot(baseline_errormonth)+
    geom_ribbon(aes(x = BinDepth,
                    y = !!mean_to_plot,
                    ymin = !!mean_to_plot - 2*!!sd_to_plot,
                    ymax = !!mean_to_plot + 2*!!sd_to_plot),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y = !!mean_to_plot),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    geom_line(data = test_extreme %>%
                filter(Date == errordate),
              aes(x = BinDepth,
                  y = !!param_input),
              linewidth = 1.2)+
    geom_line(data = test_extreme %>%
                filter(Date == errordate),
              aes(x = BinDepth,
                  y = !!param_input),
              linewidth = 1.2,
              alpha = 0.1)+
    geom_point(data = test_extreme %>%
                filter(Date == errordate),
              aes(x = BinDepth,
                  y = !!param_input),
              size = 0.3)+
    scale_x_reverse()+
    coord_flip()+
    ggtitle(date_input)
  print(plot_data)
  # ggplotly(plot_data)
}
errorplotter("2023-03-06", colnames(test_extreme)[8])

test <- baseline %>%
  filter(Month == 1)

test_plot2 <- ggplot(test)+
  geom_line(aes(x = BinDepth,
                y = DO_mean))+
  geom_ribbon(aes(x = BinDepth,
                  y = DO_mean,
                  ymin = (DO_mean - (2*DO_sd)),
                  ymax = (DO_mean + (2*DO_sd))),
              alpha = 0.3)+
  scale_x_reverse()+
  coord_flip()
test_plot2

plotly::ggplotly(test_plot2)


endtime <- Sys.time()

(timetorun <- endtime - starttime)

# Scratchpad --------------------------------------------------------------

# test <- extreme_df %>%
#   filter(!is.na(flag_reason)) %>%
#   select(flag_reason, 
#          Chlorophyll,
#          Chlorophyll_Qual,
#          NO23,
#          NO23_Qual,
#          everything())

# autoqual_fields <- c(
#   "Depth_Qual_Auto",
#   "Chlorophyll_Qual_Auto",
#   "Density_Qual_Auto",
#   "DO_Qual_Auto",
#   "SigmaTheta_Qual_Auto",
#   "Light_Transmission_Qual_Auto",
#   "PAR_Qual_Auto",
#   "Surface_PAR_Qual_Auto",
#   "Salinity_Qual_Auto",
#   "Temperature_Qual_Auto",
#   "Turbidity_Qual_Auto",
#   "NO23_Qual_Auto")


# Depth"             0, 500
# Chlorophyll"       0, 200
# Density"           500, 1100
# DO"                0, 15
# SigmaTheta"        0, 35
# Light_Transmission 0, 100
# PAR"               0, 10000
# Surface_PAR"       0, 5000
# Salinity"          0, 40
# Temperature"       0, 25
# Turbidity"         0, 125
# NO23"              0, 5
# SigmaT"            0, 35
