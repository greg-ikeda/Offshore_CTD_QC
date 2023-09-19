# Function to plot specific profiles and parameters --------

# Add _Qual_Auto flags to the working_data, so that the full profile is available to plot
extreme_df_full <- left_join(working_data, extreme_df)

plot_extreme_error <- function(df, date_input, param_input){
  parm_to_plot <- as.name(param_input)
  mean_to_plot <- as.name(paste0(param_input, "_mean"))
  sd_to_plot <- as.name(paste0(param_input, "_sd"))
  qual_to_plot <- as.name(paste0(param_input, "_Qual_Auto"))
  plot_data <- df %>%
    filter(Date == as.Date(date_input))
  
  if(all(is.na(plot_data[qual_to_plot]))){ # plots data without flags
    ggplot(plot_data)+
      geom_line(aes(x = BinDepth,
                    y = !!parm_to_plot),
                linewidth = 1.2,
                alpha = 0.1)+
      geom_point(aes(x = BinDepth,
                     y = !!parm_to_plot))+
      scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
      scale_x_reverse()+
      coord_flip()+
      ggtitle(date_input)
  } else{ #plots data with flags as a different color and size 
    ggplot(plot_data)+
      geom_line(data = plot_data %>%
                  filter(Date == as.Date(date_input)),
                aes(x = BinDepth,
                    y = !!parm_to_plot),
                linewidth = 1.2,
                alpha = 0.1)+
      geom_point(aes(x = BinDepth,
                     y = !!parm_to_plot,
                     color = !!qual_to_plot,
                     size = is.na(!!qual_to_plot)))+
      scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
      scale_x_reverse()+
      coord_flip()+
      ggtitle(date_input)
  }
}

# Examples, to see of plot_extreme_error worked 
# NO23 SUNA error example
plot_extreme_error(extreme_df_full, "2022-07-05", "NO23")
# Negative Chlorophyll example
plot_extreme_error(extreme_df_full, "2014-05-07", "Chlorophyll")
# No Qual flags example
plot_extreme_error(extreme_df_full, "2014-05-07", "DO")


# plot_multipanel_error ---------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_errors_multipanel <- function(df, date_input){
  date_to_plot <- ymd(date_input)
  plot_data <- df %>%
    filter(Date == date_to_plot)
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Chlorophyll, y = Depth, 
                   color = Chlorophyll_Qual_Auto,
                   size = is.na(Chlorophyll_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = expression(Chlorophyll~(mg/m^{3})), y = "Depth (m)") +
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 1),
               aes(x = Value, y = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))+
    geom_point(data = bottle_data_full %>% 
                 filter(CollectDate == date_to_plot, 
                        Locator == station, 
                        ParmId == 1), 
               aes(x = Value, y = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  if(all(is.na(plot_data$Chlorophyll_Qual_Auto))){
    p1 <- p1 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p4 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = DO, y = Depth, 
                   color = DO_Qual_Auto,
                   size = is.na(DO_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Dissolved Oxygen (mg/L)", y = "")+
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 5),
               aes(x = Value, y = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  if(all(is.na(plot_data$DO_Qual_Auto))){
    p4 <- p4 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p7 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Salinity, y = Depth, 
                   color = Salinity_Qual_Auto,
                   size = is.na(Salinity_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Salinity (PSS)", y = "") +
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 18),
               aes(x = Value, y = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  if(all(is.na(plot_data$Salinity_Qual_Auto))){
    p7 <- p7 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p2 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = NO23, y = Depth, 
                   color = NO23_Qual_Auto,
                   size = is.na(NO23_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Nitrate+Nitrite N (mg/L)", y = "Depth (m)") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 14),
               aes(x = Value, y = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  if(all(is.na(plot_data$NO23_Qual_Auto))){
    p2 <- p2 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p5 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Temperature, y = Depth, 
                   color = Temperature_Qual_Auto,
                   size = is.na(Temperature_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) + 
    labs(x = expression(paste("Temperature (",degree,"C)")), y = "") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot, 
                                                  Locator == station, 
                                                  ParmId == 22), 
               aes(x = Value, y = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  if(all(is.na(plot_data$Temperature_Qual_Auto))){
    p5 <- p5 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p8 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = SigmaTheta, 
                   y = Depth, 
                   color = SigmaTheta_Qual_Auto,
                   size = is.na(SigmaTheta_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = expression(sigma[theta]~(kg/m^{3})), y = "")
  if(all(is.na(plot_data$SigmaTheta_Qual_Auto))){
    p8 <- p8 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p3 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Density, 
                   y = Depth, 
                   color = Density_Qual_Auto,
                   size = is.na(Density_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = expression(Density~(kg/m^{3})), y = "Depth (m)")
  if(all(is.na(plot_data$Density_Qual_Auto))){
    p3 <- p3 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p6 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Light_Transmission, 
                   y = Depth, 
                   color = Light_Transmission_Qual_Auto,
                   size = is.na(Light_Transmission_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Light transmission", y = "")
  if(all(is.na(plot_data$Light_Transmission_Qual_Auto))){
    p6 <- p6 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  
  p9 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = log(PAR), 
                   y = Depth, 
                   color = PAR_Qual_Auto,
                   size = is.na(PAR_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "log(PAR)", y = "")
  if(all(is.na(plot_data$PAR_Qual_Auto))){
    p9 <- p9 + 
      scale_color_manual(values = c("TRUE" = "black", "FALSE"= "red")) +
      scale_size_manual(values = c(1, 1))
  }
  # png(paste0(folder, datatype, "/", station, "_", date_to_plot, "_", datatype, ".png"), width = 750, height = 750) 
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols = 3)
}

# Examples, to see if plot_errors_multipanel worked
plot_errors_multipanel(extreme_df_full, "2014-05-07")
plot_errors_multipanel(extreme_df_full, "2022-07-05")
plot_errors_multipanel(extreme_df_full, "2010-04-19")

# Testing with standard deviation flagging
stnddev_df_full <- left_join(working_data, stnddev_df)

plot_errors_multipanel(stnddev_df_full, "2014-05-07")
plot_errors_multipanel(stnddev_df_full, "2010-04-19")

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
