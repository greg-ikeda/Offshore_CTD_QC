# # TO DO LIST
# - Try using % of rolling mean below ~60 m for params like chlorophyll and PAR 
# ---- Try it on the entire profile, as well as below a certain depth

# Three pass method:
# Pass 1 - identifies spikes based on upcast and downcast differences, with a wide tolerance
# Pass 2 - identifies spikes based on a rolling mean and rolling standard deviation
# Pass 3 - a narrowed version of Pass 2
library(pspline)

QC_test <- "spike_test"
test_save_dir <- paste0(save_folder, "/", QC_test)

parmlist <- c("Chlorophyll",
              "Density",
              "DO",
              "Light_Transmission",
              "SigmaTheta",
              "Salinity",
              "Temperature",
              "NO23")

# First pass - compare upcast and downcast with a wide tolerance ------------

# Seperate CTD data into upcast and downcast tibbles
CTDdata_up <- working_data %>%
  filter(Updown == "Up") %>% 
  rename_with(~ paste0(.x, "_up"), 
              .cols = Chlorophyll:NO23_Qual) %>% 
  select(-Updown)

CTDdata_down <- working_data %>%
  filter(Updown == "Down") %>% 
  rename_with(~ paste0(.x, "_down"), 
              .cols = Chlorophyll:NO23_Qual) %>% 
  select(-Updown)

# Calculate a percent difference from the upcast and the downcast
updown_working_data <- left_join(CTDdata_up, CTDdata_down,
                                 by = c("Locator", "Date", "Depth")) %>%
  select(Sampledate.x, Sampledate.y, everything()) %>%
  mutate(Chlorophyll_perc_diff = calc_updown_percentdiff(Chlorophyll_up, Chlorophyll_down),
         Density_perc_diff = calc_updown_percentdiff(Density_up, Density_down),
         DO_perc_diff = calc_updown_percentdiff(DO_up, DO_down),
         SigmaTheta_perc_diff = calc_updown_percentdiff(SigmaTheta_up, SigmaTheta_down),
         Light_Transmission_perc_diff = calc_updown_percentdiff(Light_Transmission_up, Light_Transmission_down),
         PAR_perc_diff = calc_updown_percentdiff(PAR_up, PAR_down),
         Surface_PAR_perc_diff = calc_updown_percentdiff(Surface_PAR_up, Surface_PAR_down),
         Salinity_perc_diff = calc_updown_percentdiff(Salinity_up, Salinity_down),
         Temperature_perc_diff = calc_updown_percentdiff(Temperature_up, Temperature_down),
         NO23_perc_diff = calc_updown_percentdiff(NO23_up, NO23_down),
         BinDepth = BinDepth.x) %>%
  filter(BinDepth > 3) %>% # Removing large variation at the surface 3m 
  select(Locator,
         BinDepth, 
         Date, 
         contains("perc_diff"), 
         everything()) %>%
  select(-contains("_mean"),
         -contains("_sd"),
         -contains("_n"))

# Flag data with a large margin
updown_flags <- updown_working_data %>%
  mutate(
    Chlorophyll_Qual_Auto = case_when(
      BinDepth > 15 & Chlorophyll_perc_diff > 50 ~ "q1"),
    Density_Qual_Auto = case_when(
      BinDepth > 15 & Density_perc_diff > 50 ~ "q1"),
    DO_Qual_Auto = case_when(
      BinDepth > 15 & DO_perc_diff > 50 ~ "q1"),
    SigmaTheta_Qual_Auto = case_when(
      BinDepth > 15 & SigmaTheta_perc_diff > 50 ~ "q1"),
    Light_Transmission_Qual_Auto = case_when(
      BinDepth > 15 & Light_Transmission_perc_diff > 50 ~ "q1"),
    PAR_Qual_Auto = case_when(
      BinDepth > 15 & PAR_perc_diff > 100 ~ "q1"),
    Surface_PAR_Qual_Auto = case_when(
      BinDepth > 15 & Surface_PAR_perc_diff > 100 ~ "q1"),
    Salinity_Qual_Auto = case_when(
      BinDepth > 15 & Salinity_perc_diff > 50 ~ "q1"),
    Temperature_Qual_Auto = case_when(
      BinDepth > 15 & Temperature_perc_diff > 50 ~ "q1"),
    NO23_Qual_Auto = case_when(
      BinDepth > 15 & NO23_perc_diff > 60 ~ "q1")) %>%
  mutate(flag_reason = "",
         flag_reason = if_else(!is.na(Chlorophyll_Qual_Auto), paste0(flag_reason, "Chl_"), flag_reason),
         flag_reason = if_else(!is.na(Density_Qual_Auto), paste0(flag_reason, "Density_"), flag_reason),
         flag_reason = if_else(!is.na(DO_Qual_Auto), paste0(flag_reason, "DO_"), flag_reason),
         flag_reason = if_else(!is.na(SigmaTheta_Qual_Auto), paste0(flag_reason, "SigmaT_"), flag_reason),
         flag_reason = if_else(!is.na(Light_Transmission_Qual_Auto), paste0(flag_reason, "Light_"), flag_reason),
         flag_reason = if_else(!is.na(PAR_Qual_Auto), paste0(flag_reason, "PAR_"), flag_reason),
         flag_reason = if_else(!is.na(Surface_PAR_Qual_Auto), paste0(flag_reason, "SPAR_"), flag_reason),
         flag_reason = if_else(!is.na(Salinity_Qual_Auto), paste0(flag_reason, "Sal_"), flag_reason),
         flag_reason = if_else(!is.na(Temperature_Qual_Auto), paste0(flag_reason, "Temp_"), flag_reason),
         flag_reason = if_else(!is.na(NO23_Qual_Auto), paste0(flag_reason, "NO23_"), flag_reason)) %>%
  mutate_if(is.character, ~replace_na(.,"")) %>%
  mutate(flag_reason = str_sub(flag_reason, end = -2)) %>%
  select(Locator, BinDepth, Date, contains("Qual_Auto"), flag_reason)

# Combine data, with upcast/downcast flags applied.
# Unflag data points that correspond w/ spikes but are otherwise good
# spike_pass 1 contains data that has been flagged by the upcast/downcast comparison

spike_pass1 <- left_join(working_data, updown_flags)  %>%
  mutate(across(c(flag_reason, contains("Qual_Auto")), ~ replace(., is.na(.), ""))) %>%
  mutate(Chlorophyll_Qual_Auto = 
           ifelse(Chlorophyll_Qual_Auto == "q1" &
                    calc_updown_percentdiff(Chlorophyll, lag(Chlorophyll)) > 65, "q1", ""),
         Density_Qual_Auto = 
           ifelse(Density_Qual_Auto == "q1" &
                    calc_updown_percentdiff(Density, lag(Density)) > 50, "q1", ""),
         DO_Qual_Auto = 
           ifelse(DO_Qual_Auto == "q1" &
                    calc_updown_percentdiff(DO, lag(DO)) > 50, "q1", ""),
         Light_Transmission_Qual_Auto = 
           ifelse(Light_Transmission_Qual_Auto == "q1" &
                    calc_updown_percentdiff(Light_Transmission, lag(Light_Transmission)) > 50, "q1", ""),
         SigmaTheta_Qual_Auto = 
           ifelse(SigmaTheta_Qual_Auto == "q1" &
                    calc_updown_percentdiff(SigmaTheta, lag(SigmaTheta)) > 50, "q1", ""),
         Salinity_Qual_Auto = 
           ifelse(Salinity_Qual_Auto == "q1" &
                    calc_updown_percentdiff(Salinity, lag(Salinity)) > 50, "q1", ""),
         Temperature_Qual_Auto = 
           ifelse(Temperature_Qual_Auto == "q1" &
                    calc_updown_percentdiff(Temperature, lag(Temperature)) > 50, "q1", ""),
         NO23_Qual_Auto = 
           ifelse(NO23_Qual_Auto == "q1" &
                    calc_updown_percentdiff(NO23, lag(NO23)) > 50, "q1", "")) %>%
  mutate_if(is.character, ~replace_na(.,"")) %>%
  mutate(flag_reason = "",
         flag_reason = if_else(Chlorophyll_Qual_Auto != "", paste0(flag_reason, "Chl_"), flag_reason),
         flag_reason = if_else(Density_Qual_Auto != "", paste0(flag_reason, "Density_"), flag_reason),
         flag_reason = if_else(DO_Qual_Auto != "", paste0(flag_reason, "DO_"), flag_reason),
         flag_reason = if_else(SigmaTheta_Qual_Auto != "", paste0(flag_reason, "SigmaT_"), flag_reason),
         flag_reason = if_else(Light_Transmission_Qual_Auto != "", paste0(flag_reason, "Light_"), flag_reason),
         flag_reason = if_else(PAR_Qual_Auto != "", paste0(flag_reason, "PAR_"), flag_reason),
         flag_reason = if_else(Surface_PAR_Qual_Auto != "", paste0(flag_reason, "SPAR_"), flag_reason),
         flag_reason = if_else(Salinity_Qual_Auto != "", paste0(flag_reason, "Sal_"), flag_reason),
         flag_reason = if_else(Temperature_Qual_Auto != "", paste0(flag_reason, "Temp_"), flag_reason),
         flag_reason = if_else(NO23_Qual_Auto != "", paste0(flag_reason, "NO23_"), flag_reason)) %>%
  mutate(flag_reason = str_sub(flag_reason, end = -2)) %>%
  select(Locator, Depth, BinDepth, Date, contains("Qual_Auto"), flag_reason, everything())


# Plotting pass 1 - to be removed -----------------------------------------

for(profiledate in profile_dates){
  df <- spike_pass1 %>%
    filter(Date == ymd(profiledate))
  for(parm in parmlist){
    xval <- paste0(parm)
    yval <- "Depth"
    colval <- paste0(parm, "_Qual_Auto")
    avgval <- paste0(parm, "_rolling_avg_50")
    sdval <- paste0(parm, "_rolling_sd_50")
    
    print(ggplot(df)+
            geom_point(aes(x = .data[[yval]],
                           y = .data[[xval]],
                           color = .data[[colval]]),
                       alpha = 0.8)+
            scale_x_reverse() +
            coord_flip() +
            labs(title = paste0(profiledate, "    ", parm)) +
            theme(legend.position = "none"))
  }
}


# Establish standard deviation thresholds ---------------------------------
# WILL PROBABLY REMOVE. BEEN REPLACED BY RATE OF CHANGE MULTIPLIER
# These are the cutoff values for a 2x or 4x std dev multiplier
# Anything below this value (e.g. a low standard dev) is multiplied by 4x 
# Based on the mean rolling standard deviation (width = 50) for monthly baselines

autoquals <- c("q1", "q2")

if(station == "KSBP01"){
  sd_thresholds <- list(
    Chlorophyll = 0.447,
    Density = 0.110,
    DO = 0.117,
    Light_Transmission = 1.47,
    SigmaTheta = 0.118,
    Salinity = 0.0836,
    Temperature = 0.0658,
    NO23 = 0.00592
  )
} else if(station == "LSNT01"){
  sd_thresholds <- list(
    Chlorophyll = 0.324,
    Density = 0.105,
    DO = 0.107,
    Light_Transmission = 1.35,
    SigmaTheta = 0.0815,
    Salinity = 0.0809,
    Temperature = 0.0628,
    NO23 = 0.00627)
} else if(station == "NSEX01"){
  sd_thresholds <- list(
    Chlorophyll = 0.685,
    Density = 0.140,
    DO = 0.218,
    Light_Transmission = 2.20,
    SigmaTheta = 0.135,
    Salinity = 0.120,
    Temperature = 0.0983,
    NO23 = 0.00653)
}

# Pass 2: Rolling Average and Rolling standard dev ------------------------
# Temorarily remove spikes to compute first round of rolling stats 
# Applies threshold values that change the sd_multiplier 



# Creating a for loop to split df by parameters ---------------------------

# Create a list of parameters
# Separate into individual dfs for each parameter
# create a for loop that iterates through each parameter

### Example
variable <- as.name("Chlorophyll")
!!variable

tet <- spike_pass1 %>%
  select(!!variable)
ggplot(data = data_to_plot, aes(x = !!variable))
###

parmlist <- c("Chlorophyll",
              "Density",
              "DO",
              "Light_Transmission",
              "SigmaTheta",
              "Salinity",
              "Temperature",
              "NO23")



spike_pass2 <- spike_pass1 %>%
  mutate(Chlorophyll = ifelse(Chlorophyll_Qual_Auto %in% autoquals, lag(Chlorophyll, 1), Chlorophyll), # Removes data flagged in pass 1 for rolling stats
         Density = ifelse(Density_Qual_Auto %in% autoquals, lag(Density, 1), Density), 
         DO = ifelse(DO_Qual_Auto %in% autoquals, lag(DO, 1), DO), 
         Light_Transmission = ifelse(Light_Transmission_Qual_Auto %in% autoquals, lag(Light_Transmission, 1), Light_Transmission), 
         SigmaTheta = ifelse(SigmaTheta_Qual_Auto %in% autoquals, lag(SigmaTheta, 1), SigmaTheta), 
         Salinity = ifelse(Salinity_Qual_Auto %in% autoquals, lag(Salinity, 1), Salinity), 
         Temperature = ifelse(Temperature_Qual_Auto %in% autoquals, lag(Temperature, 1), Temperature), 
         NO23 = ifelse(NO23_Qual_Auto %in% autoquals, lag(NO23, 1), NO23)) %>%
  mutate(Chlorophyll_rolling_avg = rollapply(Chlorophyll, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean), # Computes rolling avg
         Density_rolling_avg = rollapply(Density, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
         DO_rolling_avg = rollapply(DO, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
         SigmaTheta_rolling_avg = rollapply(SigmaTheta, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
         Light_Transmission_rolling_avg = rollapply(Light_Transmission, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
         Surface_PAR_rolling_avg = rollapply(Surface_PAR, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
         Salinity_rolling_avg = rollapply(Salinity, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
         Temperature_rolling_avg = rollapply(Temperature, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
         NO23_rolling_avg = rollapply(NO23, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = mean)) %>%
  mutate(Chlorophyll_rolling_sd = rollapply(Chlorophyll, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         Density_rolling_sd = rollapply(Density, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         DO_rolling_sd = rollapply(DO, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         SigmaTheta_rolling_sd = rollapply(SigmaTheta, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         Light_Transmission_rolling_sd = rollapply(Light_Transmission, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         Surface_PAR_rolling_sd = rollapply(Surface_PAR, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         Salinity_rolling_sd = rollapply(Salinity, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         Temperature_rolling_sd = rollapply(Temperature, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
         NO23_rolling_sd = rollapply(NO23, width = 25, fill = NA, partial = T, na.rm = TRUE, FUN = sd)) %>%
  mutate(Chlorophyll_rateofchange = predict(sm.spline(Depth, Chlorophyll), Depth, 1),
         Density_rateofchange = predict(sm.spline(Depth, Density), Depth, 1),
         DO_rateofchange = predict(sm.spline(Depth, DO), Depth, 1),
         Light_Transmission_rateofchange = predict(sm.spline(Depth, Light_Transmission), Depth, 1),
         SigmaTheta_rateofchange = predict(sm.spline(Depth, SigmaTheta), Depth, 1),
         Salinity_rateofchange = predict(sm.spline(Depth, Salinity), Depth, 1),
         Temperature_rateofchange = predict(sm.spline(Depth, Temperature), Depth, 1),
         NO23_rateofchange = predict(sm.spline(Depth, NO23), Depth, 1)) %>%
  mutate(Chlorophyll_rocMA = rollapply(abs(Chlorophyll_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T),
         Density_rocMA = rollapply(abs(Density_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T),
         DO_rocMA = rollapply(abs(DO_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T),
         Light_Transmission_rocMA = rollapply(abs(Light_Transmission_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T),
         SigmaTheta_rocMA = rollapply(abs(SigmaTheta_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T),
         Salinity_rocMA = rollapply(abs(Salinity_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T),
         Temperature_rocMA = rollapply(abs(Temperature_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T),
         NO23_rocMA = rollapply(abs(NO23_rateofchange), 5, mean, na.rm = T, fill = NA, partial = T)) %>%
  mutate(Chlorophyll_sd_multiplier = 1.2 * abs(log(Chlorophyll_rocMA)),
         Density_sd_multiplier = 0.8 * abs(log(Density_rocMA)),
         DO_sd_multiplier = 1 * abs(log(DO_rocMA)),
         Light_Transmission_sd_multiplier = 1.75 * abs(log(Light_Transmission_rocMA)),
         SigmaTheta_sd_multiplier = 0.8 * abs(log(SigmaTheta_rocMA)),
         Salinity_sd_multiplier = 0.8 * abs(log(Salinity_rocMA)),
         Temperature_sd_multiplier = 1 * abs(log(Temperature_rocMA)),
         NO23_sd_multiplier = 1 * abs(log(NO23_rocMA))) %>%
  mutate(Chlorophyll_Qual_Auto = case_when(Depth > 5 & Chlorophyll < (Chlorophyll_rolling_avg - (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd)) ~ "q2",
                                           Depth > 5 & Chlorophyll > (Chlorophyll_rolling_avg + (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd)) ~ "q2"),
         Density_Qual_Auto = case_when(Depth > 5 & Density < (Density_rolling_avg - (Density_sd_multiplier * Density_rolling_sd)) ~ "q2",
                                       Depth > 5 & Density > (Density_rolling_avg + (Density_sd_multiplier * Density_rolling_sd)) ~ "q2"),
         DO_Qual_Auto = case_when(Depth > 5 & DO < (DO_rolling_avg - (DO_sd_multiplier * DO_rolling_sd)) ~ "q2",
                                  Depth > 5 & DO > (DO_rolling_avg + (DO_sd_multiplier * DO_rolling_sd)) ~ "q2"),
         Light_Transmission_Qual_Auto = case_when(Depth > 5 & Light_Transmission < (Light_Transmission_rolling_avg - (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd)) ~ "q2",
                                                  Depth > 5 & Light_Transmission > (Light_Transmission_rolling_avg + (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd)) ~ "q2"),
         SigmaTheta_Qual_Auto = case_when(Depth > 5 & SigmaTheta < (SigmaTheta_rolling_avg - (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd)) ~ "q2",
                                          Depth > 5 & SigmaTheta > (SigmaTheta_rolling_avg + (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd)) ~ "q2"),
         Salinity_Qual_Auto = case_when(Depth > 5 & Salinity < (Salinity_rolling_avg - (Salinity_sd_multiplier * Salinity_rolling_sd)) ~ "q2",
                                        Depth > 5 & Salinity > (Salinity_rolling_avg + (Salinity_sd_multiplier * Salinity_rolling_sd)) ~ "q2"),
         Temperature_Qual_Auto = case_when(Depth > 5 & Temperature < (Temperature_rolling_avg - (Temperature_sd_multiplier * Temperature_rolling_sd)) ~ "q2",
                                           Depth > 5 & Temperature > (Temperature_rolling_avg + (Temperature_sd_multiplier * Temperature_rolling_sd)) ~ "q2"),
         NO23_Qual_Auto = case_when(Depth > 5 & NO23 < (NO23_rolling_avg - (NO23_sd_multiplier * NO23_rolling_sd)) ~ "q2",
                                    Depth > 5 & NO23 > (NO23_rolling_avg + (NO23_sd_multiplier * NO23_rolling_sd)) ~ "q2")) %>%
  mutate(flag_reason = "",
         flag_reason = if_else(Chlorophyll_Qual_Auto != "", paste0(flag_reason, "Chl_"), flag_reason),
         flag_reason = if_else(Density_Qual_Auto != "", paste0(flag_reason, "Density_"), flag_reason),
         flag_reason = if_else(DO_Qual_Auto != "", paste0(flag_reason, "DO_"), flag_reason),
         flag_reason = if_else(SigmaTheta_Qual_Auto != "", paste0(flag_reason, "SigmaT_"), flag_reason),
         flag_reason = if_else(Light_Transmission_Qual_Auto != "", paste0(flag_reason, "Light_"), flag_reason),
         flag_reason = if_else(Salinity_Qual_Auto != "", paste0(flag_reason, "Sal_"), flag_reason),
         flag_reason = if_else(Temperature_Qual_Auto != "", paste0(flag_reason, "Temp_"), flag_reason),
         flag_reason = if_else(NO23_Qual_Auto != "", paste0(flag_reason, "NO23_"), flag_reason)) %>%
  mutate(flag_reason = str_sub(flag_reason, end = -2)) %>%
  select(Locator, Depth, BinDepth, Date, contains("Qual_Auto"), flag_reason, everything())

# Plotting Pass 2 - to be removed ---------------------------------------------
# This is for testing purposes. Will remove this later.
# testdates <- c("2024-12-02", "2024-02-05")
testdates <- unique(paste0(spike_pass2$Date))

# plots in C:\Users\gikeda\R\Offshore_CTD_QC\spike_testing\rateofchange
for(date in testdates){
  df <- spike_pass2 %>%
    filter(Date == ymd(date))
  
  for(parm in parmlist){
    xval <- paste0(parm)
    yval <- "Depth"
    colval <- paste0(parm, "_Qual_Auto")
    avgval <- paste0(parm, "_rolling_avg")
    sdval <- paste0(parm, "_rolling_sd")
    multval <- paste0(parm, "_sd_multiplier")
    
    paramplot <- ggplot(df)+
      geom_point(aes(x = .data[[yval]],
                     y = .data[[avgval]]),
                 alpha = 0.2,
                 color = "lightgreen") +
      geom_point(aes(x = .data[[yval]],
                     y = .data[[xval]],
                     color = .data[[colval]]),
                 alpha = 0.8)+
      geom_ribbon(aes(x = .data[[yval]],
                      y = .data[[avgval]],
                      ymin = .data[[avgval]] - (.data[[multval]] * .data[[sdval]]),
                      ymax = .data[[avgval]] + (.data[[multval]] * .data[[sdval]]),
                      group = Updown),
                  alpha = 0.1,
                  fill = "blue")+
      scale_x_reverse() +
      coord_flip() +
      labs(title = paste(date, "-", parm)) +
      theme(legend.position = "none")
    
    multplot <- ggplot(df)+
      geom_line(aes(x = .data[[yval]],
                    y = .data[[multval]])) +
      geom_point(aes(x = .data[[yval]],
                     y = .data[[multval]])) +
      scale_x_reverse() +
      coord_flip() +
      labs(title = paste(date, "-", parm)) +
      theme(legend.position = "none")
    
    sdplot <- ggplot(df)+
      geom_point(aes(x = .data[[yval]],
                     y = .data[[sdval]])) +
      scale_x_reverse() +
      coord_flip() +
      labs(title = paste(date, "-", parm)) +
      theme(legend.position = "none")
    
    spike2_plot <- paramplot + 
      multplot +
      sdplot +
      plot_layout(widths = c(3, 1, 1))
    
    ggsave(here("spike_testing", "rateofchange", paste0(date, "-", parm, ".png")),
           spike2_plot,
           width = 14,
           height = 8)
  }
}
beepr::beep()

# Spike Pass 2 older version ----------------------------------------------
# TO BE REMVOED
# This is the older version that had a standard deviation multiplier
# 
# spike_pass2 <- spike_pass1 %>%
#   mutate(Chlorophyll = ifelse(Chlorophyll_Qual_Auto %in% autoquals, NA, Chlorophyll), # Removes data flagged in pass 1 for rolling stats
#          Density = ifelse(Density_Qual_Auto %in% autoquals, NA, Density), 
#          DO = ifelse(DO_Qual_Auto %in% autoquals, NA, DO), 
#          Light_Transmission = ifelse(Light_Transmission_Qual_Auto %in% autoquals, NA, Light_Transmission), 
#          SigmaTheta = ifelse(SigmaTheta_Qual_Auto %in% autoquals, NA, SigmaTheta), 
#          Salinity = ifelse(Salinity_Qual_Auto %in% autoquals, NA, Salinity), 
#          Temperature = ifelse(Temperature_Qual_Auto %in% autoquals, NA, Temperature), 
#          NO23 = ifelse(NO23_Qual_Auto %in% autoquals, NA, NO23)) %>%
#   mutate(Chlorophyll_rolling_avg_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean), # Computes rolling avg
#          Density_rolling_avg_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          DO_rolling_avg_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          SigmaTheta_rolling_avg_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Light_Transmission_rolling_avg_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          PAR_rolling_avg_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Surface_PAR_rolling_avg_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Salinity_rolling_avg_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Temperature_rolling_avg_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          NO23_rolling_avg_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean)) %>%
#   mutate(Chlorophyll_rolling_sd_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Density_rolling_sd_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          DO_rolling_sd_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          SigmaTheta_rolling_sd_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Light_Transmission_rolling_sd_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          PAR_rolling_sd_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Surface_PAR_rolling_sd_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Salinity_rolling_sd_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Temperature_rolling_sd_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          NO23_rolling_sd_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd)) %>%
#   mutate(Chlorophyll_rateofchange = predict(sm.spline(Depth, Chlorophyll), Depth, 1),
#          Density_rateofchange = predict(sm.spline(Depth, Density), Depth, 1),
#          DO_rateofchange = predict(sm.spline(Depth, DO), Depth, 1),
#          Light_Transmission_rateofchange = predict(sm.spline(Depth, Light_Transmission), Depth, 1),
#          SigmaTheta_rateofchange = predict(sm.spline(Depth, SigmaTheta), Depth, 1),
#          Salinity_rateofchange = predict(sm.spline(Depth, Salinity), Depth, 1),
#          Temperature_rateofchange = predict(sm.spline(Depth, Temperature), Depth, 1),
#          NO23_rateofchange = predict(sm.spline(Depth, NO23), Depth, 1)) %>%
#   # Moving average of absolute value of the rate of change
#   mutate(Chlorophyll_rocMA = rollapply(abs(Chlorophyll_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T),
#          Density_rocMA = rollapply(abs(Density_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T),
#          DO_rocMA = rollapply(abs(DO_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T),
#          Light_Transmission_rocMA = rollapply(abs(Light_Transmission_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T),
#          SigmaTheta_rocMA = rollapply(abs(SigmaTheta_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T),
#          Salinity_rocMA = rollapply(abs(Salinity_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T),
#          Temperature_rocMA = rollapply(abs(Temperature_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T),
#          NO23_rocMA = rollapply(abs(NO23_rateofchange), 15, mean, na.rm = T, fill = NA, partial = T)) %>%
#   # New test version of the sd multiplier - now a function of the rate of change moving average. 
#   mutate(Chlorophyll_sd_multiplier = ifelse(Chlorophyll_rolling_sd_50 < sd_thresholds$Chlorophyll, 5, 2),
#          Density_sd_multiplier = ifelse(Density_rolling_sd_50 < sd_thresholds$Density, 4, 2),
#          DO_sd_multiplier = ifelse(DO_rolling_sd_50 < sd_thresholds$DO, 4, 2),
#          Light_Transmission_sd_multiplier = ifelse(Light_Transmission_rolling_sd_50 < sd_thresholds$Light_Transmission, 4, 2),
#          SigmaTheta_sd_multiplier = ifelse(SigmaTheta_rolling_sd_50 < sd_thresholds$SigmaTheta, 4, 2),
#          Salinity_sd_multiplier = ifelse(Salinity_rolling_sd_50 < sd_thresholds$Salinity, 4, 2),
#          Temperature_sd_multiplier = ifelse(Temperature_rolling_sd_50 < sd_thresholds$Temperature, 4, 2),
#          NO23_sd_multiplier = ifelse(NO23_rolling_sd_50 < sd_thresholds$NO23, 8, 2)) %>%
#   # mutate(Chlorophyll_sd_multiplier = ifelse(Chlorophyll_rolling_sd_50 < sd_thresholds$Chlorophyll, 5, 2),
#   #        Density_sd_multiplier = ifelse(Density_rolling_sd_50 < sd_thresholds$Density, 4, 2),
#   #        DO_sd_multiplier = ifelse(DO_rolling_sd_50 < sd_thresholds$DO, 4, 2),
#   #        Light_Transmission_sd_multiplier = ifelse(Light_Transmission_rolling_sd_50 < sd_thresholds$Light_Transmission, 4, 2),
#   #        SigmaTheta_sd_multiplier = ifelse(SigmaTheta_rolling_sd_50 < sd_thresholds$SigmaTheta, 4, 2),
#   #        Salinity_sd_multiplier = ifelse(Salinity_rolling_sd_50 < sd_thresholds$Salinity, 4, 2),
#   #        Temperature_sd_multiplier = ifelse(Temperature_rolling_sd_50 < sd_thresholds$Temperature, 4, 2),
#   #        NO23_sd_multiplier = ifelse(NO23_rolling_sd_50 < sd_thresholds$NO23, 8, 2)) %>%
#   mutate(Chlorophyll_Qual_Auto = case_when(Chlorophyll < (Chlorophyll_rolling_avg_50 - (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50)) ~ "q2",
#                                            Chlorophyll > (Chlorophyll_rolling_avg_50 + (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50)) ~ "q2"),
#          Density_Qual_Auto = case_when(Density < (Density_rolling_avg_50 - (Density_sd_multiplier * Density_rolling_sd_50)) ~ "q2",
#                                        Density > (Density_rolling_avg_50 + (Density_sd_multiplier * Density_rolling_sd_50)) ~ "q2"),
#          DO_Qual_Auto = case_when(DO < (DO_rolling_avg_50 - (DO_sd_multiplier * DO_rolling_sd_50)) ~ "q2",
#                                   DO > (DO_rolling_avg_50 + (DO_sd_multiplier * DO_rolling_sd_50)) ~ "q2"),
#          Light_Transmission_Qual_Auto = case_when(Light_Transmission < (Light_Transmission_rolling_avg_50 - (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd_50)) ~ "q2",
#                                                   Light_Transmission > (Light_Transmission_rolling_avg_50 + (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd_50)) ~ "q2"),
#          SigmaTheta_Qual_Auto = case_when(SigmaTheta < (SigmaTheta_rolling_avg_50 - (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd_50)) ~ "q2",
#                                           SigmaTheta > (SigmaTheta_rolling_avg_50 + (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd_50)) ~ "q2"),
#          Salinity_Qual_Auto = case_when(Salinity < (Salinity_rolling_avg_50 - (Salinity_sd_multiplier * Salinity_rolling_sd_50)) ~ "q2",
#                                         Salinity > (Salinity_rolling_avg_50 + (Salinity_sd_multiplier * Salinity_rolling_sd_50)) ~ "q2"),
#          Temperature_Qual_Auto = case_when(Temperature < (Temperature_rolling_avg_50 - (Temperature_sd_multiplier * Temperature_rolling_sd_50)) ~ "q2",
#                                            Temperature > (Temperature_rolling_avg_50 + (Temperature_sd_multiplier * Temperature_rolling_sd_50)) ~ "q2"),
#          NO23_Qual_Auto = case_when(NO23 < (NO23_rolling_avg_50 - (NO23_sd_multiplier * NO23_rolling_sd_50)) ~ "q2",
#                                     NO23 > (NO23_rolling_avg_50 + (NO23_sd_multiplier * NO23_rolling_sd_50)) ~ "q2")) %>%
#   mutate_if(is.character, ~replace_na(.,"")) %>%
#   mutate(flag_reason = "",
#          flag_reason = if_else(Chlorophyll_Qual_Auto != "", paste0(flag_reason, "Chl_"), flag_reason),
#          flag_reason = if_else(Density_Qual_Auto != "", paste0(flag_reason, "Density_"), flag_reason),
#          flag_reason = if_else(DO_Qual_Auto != "", paste0(flag_reason, "DO_"), flag_reason),
#          flag_reason = if_else(SigmaTheta_Qual_Auto != "", paste0(flag_reason, "SigmaT_"), flag_reason),
#          flag_reason = if_else(Light_Transmission_Qual_Auto != "", paste0(flag_reason, "Light_"), flag_reason),
#          flag_reason = if_else(PAR_Qual_Auto != "", paste0(flag_reason, "PAR_"), flag_reason),
#          flag_reason = if_else(Surface_PAR_Qual_Auto != "", paste0(flag_reason, "SPAR_"), flag_reason),
#          flag_reason = if_else(Salinity_Qual_Auto != "", paste0(flag_reason, "Sal_"), flag_reason),
#          flag_reason = if_else(Temperature_Qual_Auto != "", paste0(flag_reason, "Temp_"), flag_reason),
#          flag_reason = if_else(NO23_Qual_Auto != "", paste0(flag_reason, "NO23_"), flag_reason)) %>%
#   mutate(flag_reason = str_sub(flag_reason, end = -2)) %>%
#   select(Locator, Depth, BinDepth, Date, contains("Qual_Auto"), flag_reason, everything())
# 

# # # Pass 3: Second rolling average and rolling standard deviation ------------------------------------------------------------------
# 
# spike_pass3 <- spike_pass2 %>%
#   mutate(Chlorophyll = ifelse(Chlorophyll_Qual_Auto %in% autoquals, NA, Chlorophyll), # Removes data flagged in pass 2 for rolling stats
#          Density = ifelse(Density_Qual_Auto %in% autoquals, NA, Density), 
#          DO = ifelse(DO_Qual_Auto %in% autoquals, NA, DO), 
#          Light_Transmission = ifelse(Light_Transmission_Qual_Auto %in% autoquals, NA, Light_Transmission), 
#          SigmaTheta = ifelse(SigmaTheta_Qual_Auto %in% autoquals, NA, SigmaTheta), 
#          Salinity = ifelse(Salinity_Qual_Auto %in% autoquals, NA, Salinity), 
#          Temperature = ifelse(Temperature_Qual_Auto %in% autoquals, NA, Temperature), 
#          NO23 = ifelse(NO23_Qual_Auto %in% autoquals, NA, NO23)) %>%
#   mutate(Chlorophyll_rolling_avg_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean), # Computes rolling avg
#          Density_rolling_avg_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          DO_rolling_avg_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          SigmaTheta_rolling_avg_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Light_Transmission_rolling_avg_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          PAR_rolling_avg_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Surface_PAR_rolling_avg_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Salinity_rolling_avg_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Temperature_rolling_avg_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          NO23_rolling_avg_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean)) %>%
#   mutate(Chlorophyll_rolling_sd_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Density_rolling_sd_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          DO_rolling_sd_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          SigmaTheta_rolling_sd_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Light_Transmission_rolling_sd_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          PAR_rolling_sd_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Surface_PAR_rolling_sd_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Salinity_rolling_sd_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Temperature_rolling_sd_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          NO23_rolling_sd_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd)) %>%
#   mutate(Chlorophyll_sd_multiplier = ifelse(Chlorophyll_rolling_sd_50 < 0.2, 4, 2),
#          Density_sd_multiplier = ifelse(Density_rolling_sd_50 < 0.07, 4, 2),
#          DO_sd_multiplier = ifelse(DO_rolling_sd_50 < 0.08, 4, 2),
#          Light_Transmission_sd_multiplier = ifelse(Light_Transmission_rolling_sd_50 < 1.13, 4, 2),
#          SigmaTheta_sd_multiplier = ifelse(SigmaTheta_rolling_sd_50 < 0.04, 4, 2),
#          Salinity_sd_multiplier = ifelse(Salinity_rolling_sd_50 < 0.05, 4, 2),
#          Temperature_sd_multiplier = ifelse(Temperature_rolling_sd_50 < 0.05, 4, 2),
#          NO23_sd_multiplier = ifelse(NO23_rolling_sd_50 < 0.2, 8, 2)) %>%
#   mutate(Chlorophyll_Qual_Auto = case_when(Chlorophyll < (Chlorophyll_rolling_avg_50 - (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50)) ~ "q2",
#                                            Chlorophyll > (Chlorophyll_rolling_avg_50 + (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50)) ~ "q2"),
#          Density_Qual_Auto = case_when(Density < (Density_rolling_avg_50 - (Density_sd_multiplier * Density_rolling_sd_50)) ~ "q2",
#                                        Density > (Density_rolling_avg_50 + (Density_sd_multiplier * Density_rolling_sd_50)) ~ "q2"),
#          DO_Qual_Auto = case_when(DO < (DO_rolling_avg_50 - (DO_sd_multiplier * DO_rolling_sd_50)) ~ "q2",
#                                   DO > (DO_rolling_avg_50 + (DO_sd_multiplier * DO_rolling_sd_50)) ~ "q2"),
#          Light_Transmission_Qual_Auto = case_when(Light_Transmission < (Light_Transmission_rolling_avg_50 - (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd_50)) ~ "q2",
#                                                   Light_Transmission > (Light_Transmission_rolling_avg_50 + (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd_50)) ~ "q2"),
#          SigmaTheta_Qual_Auto = case_when(SigmaTheta < (SigmaTheta_rolling_avg_50 - (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd_50)) ~ "q2",
#                                           SigmaTheta > (SigmaTheta_rolling_avg_50 + (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd_50)) ~ "q2"),
#          Salinity_Qual_Auto = case_when(Salinity < (Salinity_rolling_avg_50 - (Salinity_sd_multiplier * Salinity_rolling_sd_50)) ~ "q2",
#                                         Salinity > (Salinity_rolling_avg_50 + (Salinity_sd_multiplier * Salinity_rolling_sd_50)) ~ "q2"),
#          Temperature_Qual_Auto = case_when(Temperature < (Temperature_rolling_avg_50 - (Temperature_sd_multiplier * Temperature_rolling_sd_50)) ~ "q2",
#                                            Temperature > (Temperature_rolling_avg_50 + (Temperature_sd_multiplier * Temperature_rolling_sd_50)) ~ "q2"),
#          NO23_Qual_Auto = case_when(NO23 < (NO23_rolling_avg_50 - (NO23_sd_multiplier * NO23_rolling_sd_50)) ~ "q2",
#                                     NO23 > (NO23_rolling_avg_50 + (NO23_sd_multiplier * NO23_rolling_sd_50)) ~ "q2")) %>%
#   mutate_if(is.character, ~replace_na(.,"")) %>%
#   mutate(flag_reason = "",
#          flag_reason = if_else(Chlorophyll_Qual_Auto != "", paste0(flag_reason, "Chl_"), flag_reason),
#          flag_reason = if_else(Density_Qual_Auto != "", paste0(flag_reason, "Density_"), flag_reason),
#          flag_reason = if_else(DO_Qual_Auto != "", paste0(flag_reason, "DO_"), flag_reason),
#          flag_reason = if_else(SigmaTheta_Qual_Auto != "", paste0(flag_reason, "SigmaT_"), flag_reason),
#          flag_reason = if_else(Light_Transmission_Qual_Auto != "", paste0(flag_reason, "Light_"), flag_reason),
#          flag_reason = if_else(PAR_Qual_Auto != "", paste0(flag_reason, "PAR_"), flag_reason),
#          flag_reason = if_else(Surface_PAR_Qual_Auto != "", paste0(flag_reason, "SPAR_"), flag_reason),
#          flag_reason = if_else(Salinity_Qual_Auto != "", paste0(flag_reason, "Sal_"), flag_reason),
#          flag_reason = if_else(Temperature_Qual_Auto != "", paste0(flag_reason, "Temp_"), flag_reason),
#          flag_reason = if_else(NO23_Qual_Auto != "", paste0(flag_reason, "NO23_"), flag_reason)) %>%
#   mutate(flag_reason = str_sub(flag_reason, end = -2)) %>%
#   select(Locator, Depth, BinDepth, Date, contains("Qual_Auto"), flag_reason, everything())
# 
# 
# # spike_flags_removed_2 <- spike_flags_removed_1 %>%
# #   mutate(Chlorophyll = ifelse(Chlorophyll_Qual_Auto %in% autoquals, NA, Chlorophyll),
# #          Density = ifelse(Density_Qual_Auto %in% autoquals, NA, Density),
# #          DO = ifelse(DO_Qual_Auto %in% autoquals, NA, DO),
# #          Light_Transmission = ifelse(Light_Transmission_Qual_Auto %in% autoquals, NA, Light_Transmission),
# #          SigmaTheta = ifelse(SigmaTheta_Qual_Auto %in% autoquals, NA, SigmaTheta),
# #          Salinity = ifelse(Salinity_Qual_Auto %in% autoquals, NA, Salinity),
# #          Temperature = ifelse(Temperature_Qual_Auto %in% autoquals, NA, Temperature),
# #          NO23 = ifelse(NO23_Qual_Auto %in% autoquals, NA, NO23)) %>%
# #   mutate(Chlorophyll_rolling_avg_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          Density_rolling_avg_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          DO_rolling_avg_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          SigmaTheta_rolling_avg_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          Light_Transmission_rolling_avg_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          PAR_rolling_avg_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          Surface_PAR_rolling_avg_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          Salinity_rolling_avg_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          Temperature_rolling_avg_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
# #          NO23_rolling_avg_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean)) %>%
# #   mutate(Chlorophyll_rolling_sd_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          Density_rolling_sd_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          DO_rolling_sd_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          SigmaTheta_rolling_sd_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          Light_Transmission_rolling_sd_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          PAR_rolling_sd_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          Surface_PAR_rolling_sd_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          Salinity_rolling_sd_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          Temperature_rolling_sd_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
# #          NO23_rolling_sd_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd)) %>%
# #   mutate(LT_qual_35 = case_when(Light_Transmission < (LT_rolling_avg_35 - (2*LT_rolling_sd_35)) ~ "q1",
# #                                 Light_Transmission > (LT_rolling_avg_35 + (2*LT_rolling_sd_35)) ~ "q1"),)
# #
# # %>%
# #   mutate(Chlorophyll_Qual_Auto = case_when(
# #     BinDepth > 15 & Chlorophyll_perc_diff > 50 ~ "q"),
# #     Density_Qual_Auto = case_when(
# #       BinDepth > 15 & Density_perc_diff > 50 ~ "q"),
# #     DO_Qual_Auto = case_when(
# #       BinDepth > 15 & DO_perc_diff > 50 ~ "q"),
# #     SigmaTheta_Qual_Auto = case_when(
# #       BinDepth > 15 & SigmaTheta_perc_diff > 50 ~ "q"),
# #     Light_Transmission_Qual_Auto = case_when(
# #       BinDepth > 15 & Light_Transmission_perc_diff > 50 ~ "q"),
# #     PAR_Qual_Auto = case_when(
# #       BinDepth > 15 & PAR_perc_diff > 100 ~ "q"),
# #     Surface_PAR_Qual_Auto = case_when(
# #       BinDepth > 15 & Surface_PAR_perc_diff > 100 ~ "q"),
# #     Salinity_Qual_Auto = case_when(
# #       BinDepth > 15 & Salinity_perc_diff > 50 ~ "q"),
# #     Temperature_Qual_Auto = case_when(
# #       BinDepth > 15 & Temperature_perc_diff > 50 ~ "q"),
# #     NO23_Qual_Auto = case_when(
# #       BinDepth > 15 & NO23_perc_diff > 60 ~ "q")) %>%
# #   mutate(flag_reason = "",
# #          flag_reason = if_else(!is.na(Chlorophyll_Qual_Auto), paste0(flag_reason, "Chl_"), flag_reason),
# #          flag_reason = if_else(!is.na(Density_Qual_Auto), paste0(flag_reason, "Density_"), flag_reason),
# #          flag_reason = if_else(!is.na(DO_Qual_Auto), paste0(flag_reason, "DO_"), flag_reason),
# #          flag_reason = if_else(!is.na(SigmaTheta_Qual_Auto), paste0(flag_reason, "SigmaT_"), flag_reason),
# #          flag_reason = if_else(!is.na(Light_Transmission_Qual_Auto), paste0(flag_reason, "Light_"), flag_reason),
# #          flag_reason = if_else(!is.na(PAR_Qual_Auto), paste0(flag_reason, "PAR_"), flag_reason),
# #          flag_reason = if_else(!is.na(Surface_PAR_Qual_Auto), paste0(flag_reason, "SPAR_"), flag_reason),
# #          flag_reason = if_else(!is.na(Salinity_Qual_Auto), paste0(flag_reason, "Sal_"), flag_reason),
# #          flag_reason = if_else(!is.na(Temperature_Qual_Auto), paste0(flag_reason, "Temp_"), flag_reason),
# #          flag_reason = if_else(!is.na(Turbidity_Qual_Auto), paste0(flag_reason, "Turb_"), flag_reason),
# #          flag_reason = if_else(!is.na(NO23_Qual_Auto), paste0(flag_reason, "NO23_"), flag_reason)) %>%
# #   mutate_if(is.character, ~replace_na(.,"")) %>%
# #   mutate(flag_reason = str_sub(flag_reason, end = -2))
# #
# 
# 
# 
# # Scratchpad --------------------------------------------------------------

# # Averaging depth values to calc sd ---------------------------------------
# 
# depth_averaged <- CTDdata_flagged %>%
#   mutate(Chlorophyll_rolling_sd_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Density_rolling_sd_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          DO_rolling_sd_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          SigmaTheta_rolling_sd_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Light_Transmission_rolling_sd_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Salinity_rolling_sd_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Temperature_rolling_sd_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          NO23_rolling_sd_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd)) %>%
#   group_by(Month, Depth) %>%
#   summarize(Chlorophyll = mean(Chlorophyll),
#             Density = mean(Density),
#             DO = mean(DO),
#             Light_Transmission = mean(Light_Transmission),
#             SigmaTheta = mean(SigmaTheta),
#             Salinity = mean(Salinity),
#             Temperature = mean(Temperature),
#             NO23 = mean(NO23),
#             Chlorophyll_rolling_sd_50 = mean(Chlorophyll_rolling_sd_50),
#             Density_rolling_sd_50 = mean(Density_rolling_sd_50),
#             DO_rolling_sd_50 = mean(DO_rolling_sd_50),
#             Light_Transmission_rolling_sd_50 = mean(Light_Transmission_rolling_sd_50),
#             SigmaTheta_rolling_sd_50 = mean(SigmaTheta_rolling_sd_50),
#             Salinity_rolling_sd_50 = mean(Salinity_rolling_sd_50),
#             Temperature_rolling_sd_50 = mean(Temperature_rolling_sd_50),
#             NO23_rolling_sd_50 = mean(NO23_rolling_sd_50)) 
# 
# for(parm in parmlist){
#   
#   sd_50 <- paste0(parm, "_rolling_sd_50")
#   
#   meanval <- mean(depth_averaged[[sd_50]], na.rm = TRUE)
#   print(parm)
#   print(meanval)
#   
#   print(
#     ggplot(depth_averaged)+
#       geom_point(aes(x = Depth,
#                      y = .data[[sd_50]],
#                      color = as.factor(Month))) +
#       geom_hline(aes(yintercept = meanval),
#                  linewidth = 1.2,
#                  alpha = 0.5) +
#       geom_path(aes(x = Depth,
#                     y = .data[[sd_50]],
#                     color = as.factor(Month)),
#                 alpha = 0.3) +
#       labs(title = paste0(parm)) + 
#       scale_x_reverse() + 
#       coord_flip()
#   )
#   ggsave(paste0(parm, "_averagesd.png"),
#          height = 8,
#          width = 10)
# }
# 
# sink("sd_ranges_2.txt")
# for(parm in parmlist){
#   sd_50 <- paste0(parm, "_rolling_sd_50")
#   print(parm)
#   print(summary(depth_averaged[[sd_50]]))
# }
# 
# sink()
# 
# 
# # Applying minimum values for std dev example
# # modifies the "VVVVV_sd_multiplier" value based on the rolling sd value (increases when it is very low)
# 
# mult_std_dev_test <- spike_pass1 %>%
#   mutate(Chlorophyll = ifelse(Chlorophyll_Qual_Auto %in% autoquals, NA, Chlorophyll), # Removes data flagged in pass 1 for rolling stats
#          Density = ifelse(Density_Qual_Auto %in% autoquals, NA, Density), 
#          DO = ifelse(DO_Qual_Auto %in% autoquals, NA, DO), 
#          Light_Transmission = ifelse(Light_Transmission_Qual_Auto %in% autoquals, NA, Light_Transmission), 
#          SigmaTheta = ifelse(SigmaTheta_Qual_Auto %in% autoquals, NA, SigmaTheta), 
#          Salinity = ifelse(Salinity_Qual_Auto %in% autoquals, NA, Salinity), 
#          Temperature = ifelse(Temperature_Qual_Auto %in% autoquals, NA, Temperature), 
#          NO23 = ifelse(NO23_Qual_Auto %in% autoquals, NA, NO23)) %>%
#   mutate(Chlorophyll_rolling_avg_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean), # Computes rolling avg
#          Density_rolling_avg_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          DO_rolling_avg_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          SigmaTheta_rolling_avg_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Light_Transmission_rolling_avg_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          PAR_rolling_avg_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Surface_PAR_rolling_avg_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Salinity_rolling_avg_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          Temperature_rolling_avg_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean),
#          NO23_rolling_avg_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = mean)) %>%
#   mutate(Chlorophyll_rolling_sd_50 = rollapply(Chlorophyll, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Density_rolling_sd_50 = rollapply(Density, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          DO_rolling_sd_50 = rollapply(DO, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          SigmaTheta_rolling_sd_50 = rollapply(SigmaTheta, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Light_Transmission_rolling_sd_50 = rollapply(Light_Transmission, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          PAR_rolling_sd_50 = rollapply(PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Surface_PAR_rolling_sd_50 = rollapply(Surface_PAR, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Salinity_rolling_sd_50 = rollapply(Salinity, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          Temperature_rolling_sd_50 = rollapply(Temperature, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd),
#          NO23_rolling_sd_50 = rollapply(NO23, width = 50, fill = NA, partial = T, na.rm = TRUE, FUN = sd)) %>%
#   mutate(Chlorophyll_sd_multiplier = ifelse(Chlorophyll_rolling_sd_50 < 0.2, 8, 4),
#          Density_sd_multiplier = ifelse(Density_rolling_sd_50 < 0.2, 8, 4),
#          DO_sd_multiplier = ifelse(DO_rolling_sd_50 < 0.2, 8, 4),
#          Light_Transmission_sd_multiplier = ifelse(Light_Transmission_rolling_sd_50 < 0.2, 8, 4),
#          SigmaTheta_sd_multiplier = ifelse(SigmaTheta_rolling_sd_50 < 0.2, 8, 4),
#          Salinity_sd_multiplier = ifelse(Salinity_rolling_sd_50 < 0.2, 8, 4),
#          Temperature_sd_multiplier = ifelse(Temperature_rolling_sd_50 < 0.2, 8, 4),
#          NO23_sd_multiplier = ifelse(NO23_rolling_sd_50 < 0.2, 8, 4)) %>%
#   mutate(Chlorophyll_Qual_Auto = case_when(Chlorophyll < (Chlorophyll_rolling_avg_50 - (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50)) ~ "q2",
#                                            Chlorophyll > (Chlorophyll_rolling_avg_50 + (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50)) ~ "q2"),
#          Density_Qual_Auto = case_when(Density < (Density_rolling_avg_50 - (Density_sd_multiplier * Density_rolling_sd_50)) ~ "q2",
#                                        Density > (Density_rolling_avg_50 + (Density_sd_multiplier * Density_rolling_sd_50)) ~ "q2"),
#          DO_Qual_Auto = case_when(DO < (DO_rolling_avg_50 - (DO_sd_multiplier * DO_rolling_sd_50)) ~ "q2",
#                                   DO > (DO_rolling_avg_50 + (DO_sd_multiplier * DO_rolling_sd_50)) ~ "q2"),
#          Light_Transmission_Qual_Auto = case_when(Light_Transmission < (Light_Transmission_rolling_avg_50 - (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd_50)) ~ "q2",
#                                                   Light_Transmission > (Light_Transmission_rolling_avg_50 + (Light_Transmission_sd_multiplier * Light_Transmission_rolling_sd_50)) ~ "q2"),
#          SigmaTheta_Qual_Auto = case_when(SigmaTheta < (SigmaTheta_rolling_avg_50 - (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd_50)) ~ "q2",
#                                           SigmaTheta > (SigmaTheta_rolling_avg_50 + (SigmaTheta_sd_multiplier * SigmaTheta_rolling_sd_50)) ~ "q2"),
#          Salinity_Qual_Auto = case_when(Salinity < (Salinity_rolling_avg_50 - (Salinity_sd_multiplier * Salinity_rolling_sd_50)) ~ "q2",
#                                         Salinity > (Salinity_rolling_avg_50 + (Salinity_sd_multiplier * Salinity_rolling_sd_50)) ~ "q2"),
#          Temperature_Qual_Auto = case_when(Temperature < (Temperature_rolling_avg_50 - (Temperature_sd_multiplier * Temperature_rolling_sd_50)) ~ "q2",
#                                            Temperature > (Temperature_rolling_avg_50 + (Temperature_sd_multiplier * Temperature_rolling_sd_50)) ~ "q2"),
#          NO23_Qual_Auto = case_when(NO23 < (NO23_rolling_avg_50 - (NO23_sd_multiplier * NO23_rolling_sd_50)) ~ "q2",
#                                     NO23 > (NO23_rolling_avg_50 + (NO23_sd_multiplier * NO23_rolling_sd_50)) ~ "q2"))
# 
# 
# for(profiledate in profile_dates){
#   df <- mult_std_dev_test %>%
#     filter(Date == ymd(profiledate))
#   
#   print(ggplot(df)+
#           geom_point(aes(x = Depth,
#                          y = Chlorophyll,
#                          color = Chlorophyll_Qual_Auto),
#                      alpha = 0.8)+
#           geom_point(aes(x = Depth,
#                          y = Chlorophyll,
#                          color = Chlorophyll_Qual_Auto),
#                      alpha = 0.8)+
#           geom_point(aes(x = Depth,
#                          y = Chlorophyll_rolling_avg_50,
#                          color = Chlorophyll_sd_multiplier),
#                      alpha = 0.8) +
#           geom_ribbon(aes(x = Depth,
#                           y = Chlorophyll_rolling_avg_50,
#                           ymin = Chlorophyll_rolling_avg_50 - (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50),
#                           ymax = Chlorophyll_rolling_avg_50 + (Chlorophyll_sd_multiplier * Chlorophyll_rolling_sd_50),
#                           group = Updown),
#                       alpha = 0.2,
#                       fill = "blue")+
#           scale_x_reverse() +
#           coord_flip() +
#           theme(legend.position = "none") +
#           labs(title = paste0(profiledate)))
#   
#   print(profiledate)
# }
# 
# 
# test <- c("Chlorophyll")
# 
# for(profiledate in profile_dates){
#   df <- min_std_dev_test %>%
#     filter(Date == ymd(profiledate))
#   for(parm in test){
#     xval <- paste0(parm)
#     yval <- "Depth"
#     colval <- paste0(parm, "_Qual_Auto")
#     avgval <- paste0(parm, "_rolling_avg_50")
#     sdval <- paste0(parm, "_rolling_sd_50")
#     multval <- paste0(parm, "_sd_multiplier")
#     
#     print(xval)
#     print(yval)
#     print(avgval)
#     print(sdval)
#     print(multval)
#     
#     
#     print(ggplot(df)+
#             geom_point(aes(x = .data[[yval]],
#                            y = .data[[xval]],
#                            color = .data[[colval]]),
#                        alpha = 0.8)+
#             geom_point(aes(x = .data[[yval]],
#                            y = .data[[avgval]]),
#                        alpha = 0.8,
#                        color = "lightgreen") +
#             geom_ribbon(aes(x = .data[[yval]],
#                             y = .data[[avgval]],
#                             ymin = .data[[avgval]] - (.data[[multval]] * .data[[sdval]]),
#                             ymax = .data[[avgval]] + (.data[[multval]] * .data[[sdval]]),
#                             group = Updown),
#                         alpha = 0.2,
#                         fill = "blue")+
#             scale_x_reverse() +
#             coord_flip() +
#             theme(legend.position = "none"))
#   }
# }
# 


