rm(list = ls())
library(tidyverse)
library(viridis)
library(lubridate)
library(kcmarine)
library(readxl)
library(metR)
library(cmocean)
library(RColorBrewer)
library(here)
library(plotly)
source(here("contour_functions.R"))

starttime <- Sys.time()

# Setup -------------------------------------------------------------------

## 
## This is currently only running on a selection of data from KSBP01 (or whichever file is referenced in "test_data"). 
## This has not been vetted for other stations yet!
## May need to program specific cutoff values and standard deviation multipliers for each station
## I hope not. 
##

# What station do you want figures from?
station <- "KSBP01"

# # Where do you want to save output?
save_folder <- here("output")
# folder <- paste0("C:\\Users\\gikeda\\R\\Offshore_CTD_QC\\",
#                  station, "\\")
# fname <- list.files(folder, pattern = "_qcd.csv")

test_data <- here("data", "KSBP01_Example.csv")

# Load data and calculate baseline/Standard deviations --------------------------

badquals <- c("E", "TA, E", "E, Rej", "E,Rej", "E, rej", "R, E", "E, TA",
              "rej", "Rej", "REJ", "R", "Rej, E, TA", "E, TA, rej", "R, TA", "Rej, E",
              "TA, Q", "Q")

goodqual <- "TA"

bin_width <- 0.5

# CTDdata <- import_CTD(paste0(folder, fname))
CTDdata <- import_CTD(test_data) %>% 
  mutate(Year = year(Sampledate), 
         Month = month(Sampledate), 
         Day = day(Sampledate), 
         YearDay = yday(Sampledate),
         Date = as.Date(Sampledate), 
         Bin = depth_bin(Depth, bin_width), 
         BinDepth = sapply(Bin, get_bin_depth))
tz(CTDdata$Sampledate) <- "America/Los_Angeles"

CTDdata_flagged <- CTDdata %>% 
  mutate(Chlorophyll = ifelse(Chlorophyll_Qual %in% badquals, NA, Chlorophyll), 
         Density = ifelse(Density_Qual %in% badquals, NA, Density), 
         DO = ifelse(DO_Qual %in% badquals, NA, DO), 
         SigmaT = ifelse(SigmaTheta_Qual %in% badquals, NA, SigmaTheta), 
         Salinity = ifelse(Salinity_Qual %in% badquals, NA, Salinity), 
         Temperature = ifelse(Temperature_Qual %in% badquals, NA, Temperature), 
         NO23 = ifelse(NO23_Qual %in% badquals, NA, NO23))

# Creates a baseline from the first sample in the dataset to the prior year (e.g. 1998 - 2022 for a profile in 2023)
base_start <- min(CTDdata_flagged$Year)
base_end <- max(CTDdata_flagged$Year) - 1

# Similar to contour script - 
# creates baseline data, calculates baseline data, then adds those columns to the working df "working_data"
baseline_data <- CTDdata_flagged %>% 
  select(Sampledate,
         Depth,
         Density,
         Density_Qual,
         DO,
         DO_Qual,
         SigmaTheta,
         SigmaTheta_Qual,
         Salinity,
         Salinity_Qual,
         Temperature,
         Temperature_Qual,
         BinDepth,
         Chlorophyll,
         Chlorophyll_Qual,
         Light_Transmission,
         Light_Qual,
         PAR,
         PAR_Qual,
         Surface_PAR,
         Surface_PAR_Qual,
         Turbidity,
         Turbidity_Qual,
         NO23,
         NO23_Qual,
         Month) %>% 
  mutate(Year = year(Sampledate)) %>% 
  filter(Year >= base_start, 
         Year <= base_end)

baseline <- baseline_data %>% 
  group_by(Month, BinDepth) %>% 
  summarize(BaselineDensity = mean(Density, na.rm = T), 
            BaselineDO = mean(DO, na.rm = T), 
            BaselineSigmaT = mean(SigmaTheta, na.rm = T), 
            BaselineSalinity = mean(Salinity, na.rm = T), 
            BaselineTemperature = mean(Temperature, na.rm = T),
            BaselineChlorophyll = mean(Chlorophyll, na.rm = T),
            BaselineSigmaTheta = mean(SigmaTheta, na.rm = T),
            BaselineLight_Transmission = mean(Light_Transmission, na.rm = T),
            BaselinePAR = mean(PAR, na.rm = T),
            BaselineSurface_PAR = mean(Surface_PAR, na.rm = T),
            BaselineTurbidity = mean(Turbidity, na.rm = T),
            BaselineNO23 = mean(NO23, na.rm = T),
            SD_Density = sd(Density, na.rm = T), 
            SD_DO = sd(DO, na.rm = T), 
            SD_SigmaT = sd(SigmaTheta, na.rm = T), 
            SD_Salinity = sd(Salinity, na.rm = T), 
            SD_Temperature = sd(Temperature, na.rm = T),
            SD_Chlorophyll = sd(Chlorophyll, na.rm = T),
            SD_SigmaTheta = sd(SigmaTheta, na.rm = T),
            SD_Light_Transmission = sd(Light_Transmission, na.rm = T),
            SD_PAR = sd(PAR, na.rm = T),
            SD_Surface_PAR = sd(Surface_PAR, na.rm = T),
            SD_Turbidity = sd(Turbidity, na.rm = T),
            SD_NO23 = sd(NO23, na.rm = T),
            n_samples = n())

# This df has all of the data that we will be working with - baselines, standard deviations, flags
working_data <- full_join(CTDdata, baseline)

# Flagging based on extreme values ----------------------------------------

extreme_df <- working_data %>%
  mutate(
    Depth_Qual_Auto = case_when(
      Depth < 0 ~ "Rej",
      Depth > 500 ~ "Rej"),
    Chlorophyll_Qual_Auto = case_when(
      Chlorophyll < 0 ~ "Rej",
      Chlorophyll > 200 ~ "Rej"),
    Density_Qual_Auto = case_when(
      Density < 500 ~ "Rej",
      Density > 1100 ~ "Rej"),
    DO_Qual_Auto = case_when(
      DO < 0 ~ "Rej",
      DO > 15 ~ "Rej"),
    SigmaTheta_Qual_Auto = case_when(
      SigmaTheta < 0 ~ "Rej",
      SigmaTheta > 35 ~ "Rej"),
    Light_Transmission_Qual_Auto = case_when(
      Light_Transmission < 0 ~ "Rej",
      Light_Transmission > 100 ~ "Rej"),
    PAR_Qual_Auto = case_when(
      PAR < 0 ~ "Rej",
      PAR > 10000 ~ "Rej"),
    Surface_PAR_Qual_Auto = case_when(
      Surface_PAR < 0 ~ "Rej",
      Surface_PAR > 5000 ~ "Rej"),
    Salinity_Qual_Auto = case_when(
      Salinity < 0 ~ "Rej",
      Salinity > 40 ~ "Rej"),
    Temperature_Qual_Auto = case_when(
      Temperature < 0 ~ "Rej",
      Temperature > 25 ~ "Rej"),
    Turbidity_Qual_Auto = case_when(
      Turbidity < 0 ~ "Rej",
      Turbidity > 125 ~ "Rej"),
    NO23_Qual_Auto = case_when(
      NO23 < 0 ~ "Rej",
      NO23 > 5 ~ "Rej")) %>%
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
  filter(flag_reason != "")

# Makes a barplot to see the most common culprits
ggplotly(ggplot(extreme_df)+
           geom_bar(aes(x = flag_reason))+
           theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

write_csv(extreme_df, paste0(save_folder, "\\Extreme_values.csv"))

# Flagging based on standard deviation range ------------------------------

stnddev_df <- working_data %>%
  mutate(
    Chlorophyll_Qual_Auto = case_when(
      Chlorophyll < (BaselineChlorophyll - 2*SD_Chlorophyll) ~ "q",
      Chlorophyll > (BaselineChlorophyll + 2*SD_Chlorophyll) ~ "q"),
    Density_Qual_Auto = case_when(
      Density < (BaselineDensity - 2*SD_Density) ~ "q",
      Density > (BaselineDensity + 2*SD_Density) ~ "q"),
    DO_Qual_Auto = case_when(
      DO < (BaselineDO - 2*SD_DO) ~ "q",
      DO > (BaselineDO + 2*SD_DO) ~ "q"),
    # SigmaTheta_Qual_Auto = case_when(
    #   DO < (BaselineSigmaTheta - 2*SD_SigmaTheta) ~ "q",
    #   DO > (BaselineSigmaTheta + 2*SD_SigmaTheta) ~ "q"),
    Light_Transmission_Qual_Auto = case_when(
      Light_Transmission < (BaselineLight_Transmission - 2*SD_Light_Transmission) ~ "q",
      Light_Transmission > (BaselineLight_Transmission + 2*SD_Light_Transmission) ~ "q"),
    PAR_Qual_Auto = case_when(
      PAR < (BaselinePAR - 2*SD_PAR) ~ "q",
      PAR > (BaselinePAR + 2*SD_PAR) ~ "q"),
    Surface_PAR_Qual_Auto = case_when(
      Surface_PAR < (BaselineSurface_PAR - 2*SD_Surface_PAR) ~ "q",
      Surface_PAR > (BaselineSurface_PAR + 2*SD_Surface_PAR) ~ "q"),
    Salinity_Qual_Auto = case_when(
      Salinity < (BaselineSalinity - 2*SD_Salinity) ~ "q",
      Salinity > (BaselineSalinity + 2*SD_Salinity) ~ "q"),
    Temperature_Qual_Auto = case_when(
      Temperature < (BaselineTemperature - 2*SD_Temperature) ~ "q",
      Temperature > (BaselineTemperature + 2*SD_Temperature) ~ "q"),
    Turbidity_Qual_Auto = case_when(
      Turbidity < (BaselineTurbidity - 2*SD_Turbidity) ~ "q",
      Turbidity > (BaselineTurbidity + 2*SD_Turbidity) ~ "q"),
    NO23_Qual_Auto = case_when(
      NO23 < (BaselineNO23 - 2*SD_NO23) ~ "q",
      NO23 > (BaselineNO23 + 2*SD_NO23) ~ "q"),
    SigmaT_Qual_Auto = case_when(
      SigmaT < (BaselineSigmaT - 5*SD_SigmaT) ~ "q",
      SigmaT > (BaselineSigmaT + 5*SD_SigmaT) ~ "q")) %>%
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
  select(flag_reason, everything())

# Creates a barplot of flag_reason, to see what the main culprits are
ggplotly(ggplot(stnddev_df)+
  geom_bar(aes(x = flag_reason))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

write_csv(stnddev_df, paste0(save_folder, "/standard_deviation_values.csv"))
shell.exec(save_folder)


# Plotting baseline +- standard dev, with shading (DOESN'T WORK YET!) --------

test <- baseline %>%
  filter(Month == 1)

test_plot <- ggplot(test)+
  geom_path(aes(y = BinDepth,
                x = BaselineDO))+
  scale_y_reverse()+
  geom_path(aes(y = BinDepth,
                x = BaselineDO + SD_DO),
            color = "blue")+
  geom_path(aes(y = BinDepth,
                x = BaselineDO - SD_DO),
            color = "blue")+
  geom_polygon(aes(y = BinDepth,
                   x = BaselineDO - SD_DO))

plotly::ggplotly(test_plot)


endtime <- Sys.time()

(timetorun <- endtime - starttime)

# Scratchpad --------------------------------------------------------------

test_plot <- ggplot(test)+
  geom_path(aes(y = BinDepth,
                x = BaselineDO))+
  scale_y_reverse()

test_plot2 <- ggplot(test)+
  geom_line(aes(x = BinDepth,
                y = BaselineDO))+
  scale_x_reverse()+
  coord_flip()+
  geom_polygon(aes(x = BinDepth,
                   y = BaselineDO))


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
