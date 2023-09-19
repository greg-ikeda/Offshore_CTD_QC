# Function to plot specific profiles and parameters --------

# Add _Qual_Auto flags to the working_data, so that the full profile is available to plot
extreme_df_full <- left_join(working_data, extreme_df)

plot_extreme_error <- function(df, date_input, param_input){
  parm_to_plot <- as.name(param_input)
  mean_to_plot <- as.name(paste0(param_input, "_mean"))
  sd_to_plot <- as.name(paste0(param_input, "_sd"))
  qual_to_plot <- as.name(paste0(param_input, "_Qual_Auto"))

  ggplot(df)+
    geom_line(data = df %>%
                filter(Date == as.Date(date_input)),
              aes(x = BinDepth,
                  y = !!parm_to_plot),
              linewidth = 1.2,
              alpha = 0.1)+
    geom_point(data = df %>%
                 filter(Date == date_input),
               aes(x = BinDepth,
                   y = !!parm_to_plot,
                   color = !!qual_to_plot,
                   size = is.na(!!qual_to_plot)))+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_x_reverse()+
    coord_flip()+
    ggtitle(date_input)
}



# Examples, to see of plot_extreme_error worked ---------------------------
# NO23 SUNA error example
plot_extreme_error(extreme_df_full, "2022-07-05", "NO23")
# Negative Chlorophyll example
plot_extreme_error(extreme_df_full, "2014-05-07", "Chlorophyll")
# No Qual flags example
plot_extreme_error(extreme_df_full, "2014-05-07", "DO")

# Make a scatterplot of the profiles with rejected data -------------------

for(i in paste0(unique(date(extreme_df$Sampledate)))){
  error_profile <- extreme_df_full %>%
    filter(date(Sampledate) == ymd(i))
  error_profile_date <- unique(date(error_profile$Sampledate)) 
  error_profile_plot <- plot_extreme_error(error_profile, error_profile_date, "DO")
  print(error_profile_plot)
  # print(head(error_profile))
  # print(unique(error_profile$flag_reason))
}

# Print flag reason for each profile
for(i in paste0(unique(date(extreme_df$Sampledate)))){
  error_profile <- extreme_df_full %>%
    filter(date(Sampledate) == ymd(i))
  error_profile_date <- unique(date(error_profile$Sampledate)) 
  if(anyNA(error_profile$Chlorophyll_Qual_Auto)){
    plot_extreme_error(error_profile, )
  }
}

anyNA(extreme_df$Chlorophyll_Qual_Auto)


# Multipanel Plots of all dataypes ----------------------------------------

for (i in 1:length(dates)) {
  date_to_plot <- dates[i]
  data_to_plot <- data_to_plot_all %>% filter(Date == date_to_plot)
  
  p1 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Chlorophyll, y = Depth, 
                   color = Chlorophyll_Qual %in% good_quals_CTD)) + 
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = expression(Chlorophyll~(mg/m^{3})), y = "Depth (m)")
  if (discrete) {
    p1 <- p1 +     
      geom_point(data = bottle_fdata %>% filter(CollectDate == date_to_plot, 
                                                Locator == station, 
                                                ParmId == 1), 
                 aes(x = Value, y = Depth, 
                     fill = QualityId %in% good_quals_discrete), 
                 color = "black", shape = 24) + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  }
  
  p4 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = DO, y = Depth, 
                   color = DO_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = "Dissolved Oxygen (mg/L)", y = "")
  if (discrete) {
    p4 <- p4 +     
      geom_point(data = bottle_fdata %>% filter(CollectDate == date_to_plot, 
                                                Locator == station, 
                                                ParmId == 5), 
                 aes(x = Value, y = Depth, 
                     fill = QualityId %in% good_quals_discrete), 
                 color = "black", shape = 24) + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  }
  
  p7 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Salinity, y = Depth, 
                   color = Salinity_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = "Salinity (PSS)", y = "")
  if (discrete) {
    p7 <- p7 +     
      geom_point(data = bottle_fdata %>% filter(CollectDate == date_to_plot, 
                                                Locator == station, 
                                                ParmId == 18), 
                 aes(x = Value, y = Depth, 
                     fill = QualityId %in% good_quals_discrete), 
                 color = "black", shape = 24) + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  }
  
  p2 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = NO23, y = Depth, 
                   color = NO23_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = "Nitrate+Nitrite N (mg/L)", y = "Depth (m)")
  if (discrete) {
    p2 <- p2 +     
      geom_point(data = bottle_fdata %>% filter(CollectDate == date_to_plot, 
                                                Locator == station, 
                                                ParmId == 14), 
                 aes(x = Value, y = Depth, 
                     fill = QualityId %in% good_quals_discrete), 
                 color = "black", shape = 24) + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  }
  
  p5 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Temperature, y = Depth, 
                   color = Temperature_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = expression(paste("Temperature (",degree,"C)")), y = "")
  if (discrete) {
    p5 <- p5 +     
      geom_point(data = bottle_fdata %>% filter(CollectDate == date_to_plot, 
                                                Locator == station, 
                                                ParmId == 22), 
                 aes(x = Value, y = Depth, 
                     fill = QualityId %in% good_quals_discrete), 
                 color = "black", shape = 24) + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  }
  
  p8 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = SigmaTheta, y = Depth, color = SigmaTheta_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = expression(sigma[theta]~(kg/m^{3})), y = "")
  p3 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Density, y = Depth, color = Density_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = expression(Density~(kg/m^{3})), y = "Depth (m)")
  p6 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Light_Transmission, y = Depth, color = Light_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = "Light transmission", y = "")
  p9 <- ggplot(data = data_to_plot) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = log(PAR), y = Depth, color = PAR_Qual %in% good_quals_CTD)) +
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = "log(PAR)", y = "")
  # Note - changed the image dimensions to 750 x 750 - Greg Ikeda
  png(paste0(folder, datatype, "/", station, "_", date_to_plot, "_", datatype, ".png"), width = 750, height = 750) 
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols = 3)
  dev.off()
}

