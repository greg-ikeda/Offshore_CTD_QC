import_CTD_bin <- function(filename) {
  CTD_data <- read_csv(filename, 
                       col_types = cols(
                         Locator = col_character(), 
                         Date = col_date(), 
                         Year = col_integer(), 
                         Month = col_integer(), 
                         Depth = col_double(), 
                         Bin = col_character(), 
                         BinDepth = col_double(), 
                         Chlorophyll = col_double(), 
                         Density = col_double(), 
                         DO = col_double(), 
                         SigmaTheta = col_double(), 
                         Light_Transmission = col_double(), 
                         PAR = col_double(), 
                         Surface_PAR = col_double(), 
                         Salinity = col_double(), 
                         Temperature = col_double(), 
                         Turbidity = col_double(), 
                         NO23 = col_double(), 
                         Chlorophyll_Anomaly = col_double(), 
                         Density_Anomaly = col_double(), 
                         DO_Anomaly = col_double(), 
                         SigmaTheta_Anomaly = col_double(), 
                         Light_Transmission_Anomaly = col_double(), 
                         PAR_Anomaly = col_double(), 
                         Surface_PAR_Anomaly = col_double(), 
                         Temperature_Anomaly = col_double(), 
                         Turbidity_Anomaly = col_double(), 
                         NO23_Anomaly = col_double()))
  CTD_data <- CTD_data %>% mutate(YearDay = yday(Date))
  return(CTD_data)
}

bin_CTD <- function(filename, baseline, bin_width = 0.5, cutoff = T) {
  CTD_data <- import_CTD(filename)
  fancy_data <- process_CTD(CTD_data)
  bin_data <- fancy_data %>% 
    mutate(depth_bin(Depth, bin_width), 
           .after = 5)
  
  monthly_avg <- bin_data %>% 
    group_by(Locator, Bin, BinDepth, Year, Month) %>% 
    summarize(across(Chlorophyll:NO23, mean, na.rm = T)) %>% 
    ungroup()
  
  baseline <- monthly_avg %>% 
    filter(Year >= baseline[1], 
           Year <= baseline[2]) %>% 
    group_by(Bin, Locator, Month) %>% 
    summarize(across(Chlorophyll:NO23, mean, na.rm = T, .names = "{col}_Baseline")) %>% 
    ungroup() %>% 
    mutate(BinDepth = sapply(Bin, get_bin_depth), .after = Bin)
  
  depth_cutoff <- baseline %>% 
    group_by(Locator, Month) %>% 
    summarize(MaxDepth = max(BinDepth, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(Locator) %>% 
    summarize(MinMaxDepth = min(MaxDepth))
  
  anomaly_data <- left_join(bin_data, baseline)
  anomaly_data <- anomaly_data %>% 
    mutate(Chlorophyll_Anomaly = Chlorophyll - Chlorophyll_Baseline, 
           Density_Anomaly = Density - Density_Baseline, 
           DO_Anomaly = DO - DO_Baseline, 
           SigmaTheta_Anomaly = SigmaTheta - SigmaTheta_Baseline, 
           Light_Transmission_Anomaly = Light_Transmission - Light_Transmission_Baseline, 
           PAR_Anomaly = PAR - PAR_Baseline, 
           Surface_PAR_Anomaly = Surface_PAR - Surface_PAR_Baseline, 
           Salinity_Anomaly = Salinity - Salinity_Baseline, 
           Temperature_Anomaly = Temperature - Temperature_Baseline, 
           Turbidity_Anomaly = Turbidity - Turbidity_Baseline, 
           NO23_Anomaly = NO23 - NO23_Baseline) %>% 
    select(-contains("Baseline"))
  
  if (cutoff) {
    cutoff_data <- left_join(anomaly_data, depth_cutoff)
    cutoff_data <- cutoff_data %>% 
      filter(BinDepth <= MinMaxDepth)
  } else {cutoff_data <- anomaly_data}
  
  return(cutoff_data)
}

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

process_CTD <- function(CTD_data) {
  good_quals <- c(NA, "TA")
  CTD_data <- CTD_data %>% 
    relocate(Date, .before = 3) %>% 
    mutate(Chlorophyll = ifelse(Chlorophyll_Qual %in% good_quals, Chlorophyll, NA),
           Density = ifelse(Density_Qual %in% good_quals, Density, NA), 
           DO = ifelse(DO_Qual %in% good_quals, DO, NA), 
           SigmaTheta = ifelse(SigmaTheta_Qual %in% good_quals, SigmaTheta, NA), 
           Light_Transmission = ifelse(Light_Qual %in% good_quals, Light_Transmission, NA), 
           PAR = ifelse(PAR_Qual %in% good_quals, PAR, NA), 
           Surface_PAR = ifelse(Surface_PAR_Qual %in% good_quals, Surface_PAR, NA), 
           Salinity = ifelse(Salinity_Qual %in% good_quals, Salinity, NA), 
           Temperature = ifelse(Temperature_Qual %in% good_quals, Temperature, NA), 
           Turbidity = ifelse(Turbidity_Qual %in% good_quals, Turbidity, NA), 
           NO23 = ifelse(NO23_Qual %in% good_quals, NO23, NA), 
           Month = month(Date), 
           Year = year(Date)) %>% 
    select(!contains("_Qual"), -Sampledate, -CastNotes, -Updown) %>% 
    relocate(Year, .before = 3) %>% 
    relocate(Month, .before = 4) %>% 
    filter(rowSums(is.na(.[,6:16])) != 11)
  return(CTD_data)
}

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

craftbrewer_pal <- function (type = "seq", palette = 1, direction = 1) 
{
  pal <- scales:::pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette <- RColorBrewer:::maxcolors[names(RColorBrewer:::maxcolors) == palette]
    
    if (n < 3) {
      pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette created based on all colors of", 
                        palette))
      n_palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc <- grDevices::colorRampPalette(n_palette)
      pal <- colfunc(n)
    }
    else {
      pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

scale_fill_craftfermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

