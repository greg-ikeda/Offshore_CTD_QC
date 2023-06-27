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
         depth_bin(Depth, bin_width))
tz(CTDdata$Sampledate) <- "America/Los_Angeles"

# Create CTDdata_flagged for baseline calculation WITHOUT previously-flagged bad data
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
  select(Date,
         Month, 
         Year, 
         Depth,
         BinDepth,
         Density,
         DO,
         SigmaTheta,
         Salinity,
         Temperature,
         Chlorophyll,
         Light_Transmission,
         PAR,
         Surface_PAR,
         Turbidity,
         NO23,
         Density_Qual,
         DO_Qual,
         SigmaTheta_Qual,
         Salinity_Qual,
         Temperature_Qual,
         Chlorophyll_Qual,
         Light_Qual,
         PAR_Qual,
         Surface_PAR_Qual,
         Turbidity_Qual,
         NO23_Qual) %>% 
  filter(Year >= base_start, 
         Year <= base_end)

baseline <- baseline_data %>% 
  group_by(Month, BinDepth) %>% 
  summarize(across(Density:NO23, 
                   list(mean = ~ mean(.x, na.rm = TRUE), 
                        sd = ~ sd(.x, na.rm = TRUE), 
                        n = ~ n())))

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
  filter(flag_reason != "") %>%
  mutate(flag_reason = str_sub(flag_reason, end = -2))

# Makes a barplot to see the most common culprits
ggplotly(ggplot(extreme_df)+
           geom_bar(aes(x = flag_reason))+
           theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

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
    # SigmaTheta_Qual_Auto = case_when(
    #   DO < (BaselineSigmaTheta - 2*SD_SigmaTheta) ~ "q",
    #   DO > (BaselineSigmaTheta + 2*SD_SigmaTheta) ~ "q"),
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
                x = DO_mean))+
  scale_y_reverse()+
  geom_path(aes(y = BinDepth,
                x = DO_mean + DO_sd),
            color = "blue")+
  geom_path(aes(y = BinDepth,
                x = DO_mean - DO_sd),
            color = "blue")+
  geom_polygon(aes(y = BinDepth,
                   x = DO_mean - DO_sd))

plotly::ggplotly(test_plot)


endtime <- Sys.time()

(timetorun <- endtime - starttime)

# Scratchpad --------------------------------------------------------------

test_plot <- ggplot(test)+
  geom_path(aes(y = BinDepth,
                x = DO_mean))+
  scale_y_reverse()

test_plot2 <- ggplot(test)+
  geom_ribbon(aes(x = BinDepth,
                   y = DO_mean))+
  geom_line(aes(x = BinDepth,
                y = DO_mean))+
  scale_x_reverse()+
  coord_flip()+

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
