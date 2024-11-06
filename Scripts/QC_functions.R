# Moves png files from here("output") to the appropriate folder for the given QC test
# Helps with file organization
move_png_files <- function(save_dir, QC_test_type, locator){
  png_output <- list.files(save_dir, pattern = ".png")[which(str_detect(list.files(save_folder, pattern = ".png"), QC_test_type))]
  destination <- paste0(save_dir, "/", QC_test_type, "/", locator)
  
  archive_files <- list.files(destination, pattern = ".png")
  for(old_file in archive_files){
    file.rename(from = paste0(destination, "/", old_file), 
                to   = paste0(destination, "/", "archive", "/", old_file))
  }
  
  if(length(png_output) == 0){
    print(paste("None - no files to move from", QC_test_type))
  } else{
    print(paste0("Files moved to ", destination,":"))
    print(png_output)
  }
  for(png_file in png_output){
    file.rename(from = paste0(save_dir, "/", png_file), 
                to   = paste0(save_dir, "/", QC_test_type, "/", locator, "/", png_file))
  }
}

# Plot one parameter of one profile. Flags are different colors.
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

# Multiplot function from the multipanel QC Script. Required for error plotting
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

# Plots chl, DO, sal, NO23, temp, sigmaTheta, density, light_t, and PAR in a single figure 

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
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))+
    geom_point(data = bottle_data_full %>% 
                 filter(CollectDate == date_to_plot, 
                        Locator == station, 
                        ParmId == 1), 
               aes(x = Value, y = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
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
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
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
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p2 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = NO23, y = Depth, 
                   color = NO23_Qual_Auto,
                   size = is.na(NO23_Qual_Auto))) + 
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
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
  
  p5 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Temperature, y = Depth, 
                   color = Temperature_Qual_Auto,
                   size = is.na(Temperature_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +    
    labs(x = expression(paste("Temperature (",degree,"C)")), y = "") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot, 
                                                  Locator == station, 
                                                  ParmId == 22), 
               aes(x = Value, y = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p8 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = SigmaTheta, 
                   y = Depth, 
                   color = SigmaTheta_Qual_Auto,
                   size = is.na(SigmaTheta_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(x = expression(sigma[theta]~(kg/m^{3})), y = "")
  
  
  p3 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Density, 
                   y = Depth, 
                   color = Density_Qual_Auto,
                   size = is.na(Density_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(x = expression(Density~(kg/m^{3})), y = "Depth (m)")
  
  p6 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Light_Transmission, 
                   y = Depth, 
                   color = Light_Transmission_Qual_Auto,
                   size = is.na(Light_Transmission_Qual_Auto))) + 
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Light transmission", y = "")
  
  p9 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = log(PAR), 
                   y = Depth, 
                   color = PAR_Qual_Auto,
                   size = is.na(PAR_Qual_Auto))) + 
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "log(PAR)", y = "")
  
  png(paste0(save_folder, "/", station, "_", date_to_plot, "_", QC_test, ".png"), width = 1500, height = 1500)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols = 3)
  dev.off()
}
plot_errors_multipanel_inline <- function(df, date_input){
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
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))+
    geom_point(data = bottle_data_full %>% 
                 filter(CollectDate == date_to_plot, 
                        Locator == station, 
                        ParmId == 1), 
               aes(x = Value, y = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
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
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
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
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p2 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = NO23, y = Depth, 
                   color = NO23_Qual_Auto,
                   size = is.na(NO23_Qual_Auto))) + 
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
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
  
  p5 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Temperature, y = Depth, 
                   color = Temperature_Qual_Auto,
                   size = is.na(Temperature_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +    
    labs(x = expression(paste("Temperature (",degree,"C)")), y = "") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot, 
                                                  Locator == station, 
                                                  ParmId == 22), 
               aes(x = Value, y = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p8 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = SigmaTheta, 
                   y = Depth, 
                   color = SigmaTheta_Qual_Auto,
                   size = is.na(SigmaTheta_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(x = expression(sigma[theta]~(kg/m^{3})), y = "")
  
  
  p3 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Density, 
                   y = Depth, 
                   color = Density_Qual_Auto,
                   size = is.na(Density_Qual_Auto))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(x = expression(Density~(kg/m^{3})), y = "Depth (m)")
  
  p6 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = Light_Transmission, 
                   y = Depth, 
                   color = Light_Transmission_Qual_Auto,
                   size = is.na(Light_Transmission_Qual_Auto))) + 
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Light transmission", y = "")
  
  p9 <- ggplot(data = plot_data) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point(aes(x = log(PAR), 
                   y = Depth, 
                   color = PAR_Qual_Auto,
                   size = is.na(PAR_Qual_Auto))) + 
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "log(PAR)", y = "")
  
  return((p1 + p4 + p7) / (p2 + p5 + p8) / (p3 + p6 + p9) + 
           plot_annotation(paste0(station, " ", date_to_plot)))
  dev.off()
}

# Multipanel version with standard deviation shading
plot_errors_multipanel_sd_shading <- function(df, date_input){
  date_to_plot <- ymd(date_input)
  plot_data <- df %>%
    filter(Date == date_to_plot)
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Chlorophyll, x = Depth, 
                   color = Chlorophyll_Qual_Auto,
                   size = is.na(Chlorophyll_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Chlorophyll_mean,
                    ymin = Chlorophyll_mean - rv$Chlorophyll[3]*Chlorophyll_sd,
                    ymax = Chlorophyll_mean + rv$Chlorophyll[3]*Chlorophyll_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Chlorophyll_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = expression(Chlorophyll~(mg/m^{3})), x = "Depth (m)") +
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 1),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))+
    geom_point(data = bottle_data_full %>% 
                 filter(CollectDate == date_to_plot, 
                        Locator == station, 
                        ParmId == 1), 
               aes(y = Value, x = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p4 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = DO, x = Depth, 
                   color = DO_Qual_Auto,
                   size = is.na(DO_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  DO_mean,
                    ymin = DO_mean - rv$DO[3]*DO_sd,
                    ymax = DO_mean + rv$DO[3]*DO_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  DO_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Dissolved Oxygen (mg/L)", x = "")+
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 5),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p7 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Salinity, x = Depth, 
                   color = Salinity_Qual_Auto,
                   size = is.na(Salinity_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Salinity_mean,
                    ymin = Salinity_mean - rv$Salinity[3]*Salinity_sd,
                    ymax = Salinity_mean + rv$Salinity[3]*Salinity_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Salinity_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Salinity (PSS)", x = "") +
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 18),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p2 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = NO23, x = Depth, 
                   color = NO23_Qual_Auto,
                   size = is.na(NO23_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  NO23_mean,
                    ymin = NO23_mean - rv$NO23[3]*NO23_sd,
                    ymax = NO23_mean + rv$NO23[3]*NO23_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  NO23_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Nitrate+Nitrite N (mg/L)", x = "Depth (m)") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 14),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p5 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Temperature, x = Depth, 
                   color = Temperature_Qual_Auto,
                   size = is.na(Temperature_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Temperature_mean,
                    ymin = Temperature_mean - rv$Temperature[3]*Temperature_sd,
                    ymax = Temperature_mean + rv$Temperature[3]*Temperature_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Temperature_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +    
    labs(y = expression(paste("Temperature (",degree,"C)")), x = "") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot, 
                                                  Locator == station, 
                                                  ParmId == 22), 
               aes(y = Value, x = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p8 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = SigmaTheta, 
                   x = Depth, 
                   color = SigmaTheta_Qual_Auto,
                   size = is.na(SigmaTheta_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  SigmaTheta_mean,
                    ymin = SigmaTheta_mean - rv$SigmaTheta[3]*SigmaTheta_sd,
                    ymax = SigmaTheta_mean + rv$SigmaTheta[3]*SigmaTheta_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  SigmaTheta_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(y = expression(sigma[theta]~(kg/m^{3})), x = "")
  
  
  p3 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Density, 
                   x = Depth, 
                   color = Density_Qual_Auto,
                   size = is.na(Density_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Density_mean,
                    ymin = Density_mean - rv$Density[3]*Density_sd,
                    ymax = Density_mean + rv$Density[3]*Density_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Density_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(y = expression(Density~(kg/m^{3})), x = "Depth (m)")
  
  p6 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Light_Transmission, 
                   x = Depth, 
                   color = Light_Transmission_Qual_Auto,
                   size = is.na(Light_Transmission_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Light_Transmission_mean,
                    ymin = Light_Transmission_mean - rv$Light_Transmission[3]*Light_Transmission_sd,
                    ymax = Light_Transmission_mean + rv$Light_Transmission[3]*Light_Transmission_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Light_Transmission_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Light transmission", x = "")
  
  p9 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = log(PAR), 
                   x = Depth, 
                   color = PAR_Qual_Auto,
                   size = is.na(PAR_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  PAR_mean,
                    ymin = PAR_mean - rv$PAR[3]*PAR_sd,
                    ymax = PAR_mean + rv$PAR[3]*PAR_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  PAR_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "log(PAR)", x = "")
  
  png(paste0(save_folder, "/", 
             QC_test, "/", 
             station, "/", 
             station, "_", date_to_plot, "_", QC_test, ".png"), width = 1500, height = 1500)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols = 3)
  dev.off()
}
plot_errors_multipanel_sd_shading_inline <- function(df, date_input){
  date_to_plot <- ymd(date_input)
  plot_data <- df %>%
    filter(Date == date_to_plot)
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Chlorophyll, x = Depth, 
                   color = Chlorophyll_Qual_Auto,
                   size = is.na(Chlorophyll_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Chlorophyll_mean,
                    ymin = Chlorophyll_mean - rv$Chlorophyll[3]*Chlorophyll_sd,
                    ymax = Chlorophyll_mean + rv$Chlorophyll[3]*Chlorophyll_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Chlorophyll_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = expression(Chlorophyll~(mg/m^{3})), x = "Depth (m)") +
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 1),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))+
    geom_point(data = bottle_data_full %>% 
                 filter(CollectDate == date_to_plot, 
                        Locator == station, 
                        ParmId == 1), 
               aes(y = Value, x = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p4 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = DO, x = Depth, 
                   color = DO_Qual_Auto,
                   size = is.na(DO_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  DO_mean,
                    ymin = DO_mean - rv$DO[3]*DO_sd,
                    ymax = DO_mean + rv$DO[3]*DO_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  DO_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Dissolved Oxygen (mg/L)", x = "")+
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 5),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p7 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Salinity, x = Depth, 
                   color = Salinity_Qual_Auto,
                   size = is.na(Salinity_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Salinity_mean,
                    ymin = Salinity_mean - rv$Salinity[3]*Salinity_sd,
                    ymax = Salinity_mean + rv$Salinity[3]*Salinity_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Salinity_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Salinity (PSS)", x = "") +
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 18),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p2 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = NO23, x = Depth, 
                   color = NO23_Qual_Auto,
                   size = is.na(NO23_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  NO23_mean,
                    ymin = NO23_mean - rv$NO23[3]*NO23_sd,
                    ymax = NO23_mean + rv$NO23[3]*NO23_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  NO23_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Nitrate+Nitrite N (mg/L)", x = "Depth (m)") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot,
                                                  Locator == station,
                                                  ParmId == 14),
               aes(y = Value, x = Depth,
                   fill = QualityId %in% good_quals_discrete),
               color = "black", shape = 24) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p5 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Temperature, x = Depth, 
                   color = Temperature_Qual_Auto,
                   size = is.na(Temperature_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Temperature_mean,
                    ymin = Temperature_mean - rv$Temperature[3]*Temperature_sd,
                    ymax = Temperature_mean + rv$Temperature[3]*Temperature_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Temperature_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +    
    labs(y = expression(paste("Temperature (",degree,"C)")), x = "") +     
    geom_point(data = bottle_data_full %>% filter(CollectDate == date_to_plot, 
                                                  Locator == station, 
                                                  ParmId == 22), 
               aes(y = Value, x = Depth, 
                   fill = QualityId %in% good_quals_discrete), 
               color = "black", shape = 24) + 
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))
  
  p8 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = SigmaTheta, 
                   x = Depth, 
                   color = SigmaTheta_Qual_Auto,
                   size = is.na(SigmaTheta_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  SigmaTheta_mean,
                    ymin = SigmaTheta_mean - rv$SigmaTheta[3]*SigmaTheta_sd,
                    ymax = SigmaTheta_mean + rv$SigmaTheta[3]*SigmaTheta_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  SigmaTheta_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(y = expression(sigma[theta]~(kg/m^{3})), x = "")
  
  
  p3 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Density, 
                   x = Depth, 
                   color = Density_Qual_Auto,
                   size = is.na(Density_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Density_mean,
                    ymin = Density_mean - rv$Density[3]*Density_sd,
                    ymax = Density_mean + rv$Density[3]*Density_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Density_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    labs(y = expression(Density~(kg/m^{3})), x = "Depth (m)")
  
  p6 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = Light_Transmission, 
                   x = Depth, 
                   color = Light_Transmission_Qual_Auto,
                   size = is.na(Light_Transmission_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  Light_Transmission_mean,
                    ymin = Light_Transmission_mean - rv$Light_Transmission[3]*Light_Transmission_sd,
                    ymax = Light_Transmission_mean + rv$Light_Transmission[3]*Light_Transmission_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  Light_Transmission_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "Light transmission", x = "")
  
  p9 <- ggplot(data = plot_data) +
    theme_bw() +
    scale_x_reverse()+
    coord_flip()+
    theme(legend.position = "none") +
    geom_point(aes(y = log(PAR), 
                   x = Depth, 
                   color = PAR_Qual_Auto,
                   size = is.na(PAR_Qual_Auto))) + 
    geom_ribbon(aes(x = BinDepth,
                    y =  PAR_mean,
                    ymin = PAR_mean - rv$PAR[3]*PAR_sd,
                    ymax = PAR_mean + rv$PAR[3]*PAR_sd),
                alpha = 0.2)+
    geom_line(aes(x = BinDepth,
                  y =  PAR_mean),
              linewidth = 2,
              alpha = 0.1,
              color = "blue")+
    scale_color_manual(values = c("NA" = "black", "Rej" = "red", "q" = "orange")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    labs(y = "log(PAR)", x = "")
  
  return((p1 + p4 + p7) / (p2 + p5 + p8) / (p3 + p6 + p9) + 
           plot_annotation(paste0(station, " ", date_to_plot)))
}

# Multipanel plot for upcast/downcast % difference

plot_errors_multipanel_updown <- function(df, date_input){
  date_to_plot <- ymd(date_input)
  plot_data <- df %>%
    filter(Date == date_to_plot)
  
  # Temperature
  if(max(plot_data$Temperature_perc_diff, na.rm = TRUE) > 10){
    temperature_maxval = max(plot_data$Temperature_perc_diff, na.rm = TRUE) * 1.1
  } else{
    temperature_maxval = 10
  }
  temperature_plot <- ggplot(plot_data)+
    geom_point(aes(x = Temperature_perc_diff,
                   y = BinDepth,
                   color = Temperature_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, temperature_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # Salinity
  if(max(plot_data$Salinity_perc_diff, na.rm = TRUE) > 10){
    salinity_maxval = max(plot_data$Salinity_perc_diff, na.rm = TRUE) * 1.1
  } else{
    salinity_maxval = 10
  }
  sal <- ggplot(plot_data)+
    geom_point(aes(x = Salinity_perc_diff,
                   y = BinDepth,
                   color = Salinity_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, salinity_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # Chl
  if(max(plot_data$Chlorophyll_perc_diff, na.rm = TRUE) > 50){
    chl_maxval = max(plot_data$Chlorophyll_perc_diff, na.rm = TRUE) * 1.1
  } else{
    chl_maxval = 50
  }
  chl <- ggplot(plot_data)+
    geom_point(aes(x = Chlorophyll_perc_diff,
                   y = BinDepth,
                   color = Chlorophyll_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, chl_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # Density
  if(max(plot_data$Density_perc_diff, na.rm = TRUE) > 10){
    density_maxval = max(plot_data$Density_perc_diff, na.rm = TRUE) * 1.1
  } else{
    density_maxval = 0.25
  }
  density <- ggplot(plot_data)+
    geom_point(aes(x = Density_perc_diff,
                   y = BinDepth,
                   color = Density_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, density_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # DO
  if(max(plot_data$DO_perc_diff, na.rm = TRUE) > 10){
    DO_maxval = max(plot_data$DO_perc_diff, na.rm = TRUE) * 1.1
  } else{
    DO_maxval = 10
  }
  DO <- ggplot(plot_data)+
    geom_point(aes(x = DO_perc_diff,
                   y = BinDepth,
                   color = DO_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, DO_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # Light Transmission
  if(max(plot_data$Light_Transmission_perc_diff, na.rm = TRUE) > 10){
    lt_maxval = max(plot_data$Light_Transmission_perc_diff, na.rm = TRUE) * 1.1
  } else{
    lt_maxval = 10
  }
  light_transmission <- ggplot(plot_data)+
    geom_point(aes(x = Light_Transmission_perc_diff,
                   y = BinDepth,
                   color = Light_Transmission_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, lt_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # Nitrate + Nitrite
  if(max(plot_data$NO23_perc_diff, na.rm = TRUE) > 15){
    NO23_maxval = max(plot_data$NO23_perc_diff, na.rm = TRUE) * 1.1
  } else{
    NO23_maxval = 10
  }
  NO23 <- ggplot(plot_data)+
    geom_point(aes(x = NO23_perc_diff,
                   y = BinDepth,
                   color = NO23_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, NO23_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # Sigma-Theta
  if(max(plot_data$SigmaTheta_perc_diff, na.rm = TRUE) > 10){
    st_maxval = max(plot_data$SigmaTheta_perc_diff, na.rm = TRUE) * 1.1
  } else{
    st_maxval = 5
  }
  sigmatheta <- ggplot(plot_data)+
    geom_point(aes(x = SigmaTheta_perc_diff,
                   y = BinDepth,
                   color = SigmaTheta_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, st_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # Surface PAR
  if(max(plot_data$Surface_PAR_perc_diff, na.rm = TRUE) > 30){
    SPAR_maxval = max(plot_data$Surface_PAR_perc_diff, na.rm = TRUE) * 1.1
  } else{
    SPAR_maxval = 30
  }
  surface_PAR <- ggplot(plot_data)+
    geom_point(aes(x = Surface_PAR_perc_diff,
                   y = BinDepth,
                   color = Surface_PAR_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, SPAR_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  # PAR
  if(max(plot_data$PAR_perc_diff, na.rm = TRUE) > 10){
    PAR_maxval = max(plot_data$PAR_perc_diff, na.rm = TRUE) * 1.1
  } else{
    PAR_maxval = 10
  }
  PAR <- ggplot(plot_data)+
    geom_point(aes(x = PAR_perc_diff,
                   y = BinDepth,
                   color = PAR_Qual_Auto))+
    coord_cartesian(xlim = c(-0.1, PAR_maxval))+
    theme(legend.position = "none") +
    scale_y_reverse()
  
  p <-  (chl + DO + sal) / 
    (NO23 + temperature_plot + sigmatheta) / 
    (density + light_transmission + PAR) + 
    plot_annotation(paste0(station, " ", date_to_plot))
  
  save_dir <- paste0(save_folder, "/", 
                     QC_test, "/", 
                     station, "/")
  dir.create(save_dir, showWarnings = FALSE)  
  filename <- paste0(station, "_",date_to_plot, "_", QC_test, ".png")
  
  ggsave(paste0(save_dir,
                filename), 
         p,
         width = 20, height = 20)
  
  return(p)
}

# Calculates the difference between the upcast and downcast for each depth bin
calc_updown_percentdiff <- function(v1, v2){
  diff_perc <- 100 * abs(v1-v2)/((v1+v2)/2)
  return(diff_perc)
}

