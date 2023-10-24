# This script loads the necessary data for the offshore tests
# Offshore data for the specified station and date (working_data)
# Monthly baseline data for offshore data (baseline_data)
# Discrete bottle data for the specified station and date (bottle_data) 
# Sets up acceptable ranges and sd multipliers (rv)
# Load Libraries ----------------------------------------------------------

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
library(zoo)
library(svDialogs)
source(here("contour_functions.R"))

# Setup -------------------------------------------------------------------

# What station do you want figures from?
station <- dlgInput("Enter your station", "KSBP01")$res
date_begin <- dlgInput("Enter the date of the first profile to QC (YYYY-MM-DD)", "2023-01-01")$res
date_end <- dlgInput("Enter the date of the last profile to QC (YYYY-MM-DD)", Sys.Date())$res

# # Where do you want to save output?
save_folder <- here("output")

folder <- paste0("//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/", station, "/")
fname <- list.files(folder, pattern = "_qcd.csv")

# Load data and calculate baseline/Standard deviations --------------------------

badquals <- c("E", "TA, E", "E, Rej", "E,Rej", "E, rej", "R, E", "E, TA",
              "rej", "Rej", "REJ", "R", "Rej, E, TA", "E, TA, rej", "R, TA", "Rej, E",
              "TA, Q", "Q")
goodqual <- "TA"

bin_width <- 0.5

CTDdata <- import_CTD(paste0(folder, fname)) %>% 
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

# Create baseline data, calculates baseline data, then adds those columns to the working df "working_data"
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
working_data <- full_join(CTDdata, baseline) %>%
  filter(Sampledate > ymd(date_begin),
         Sampledate < ymd(date_end))

# Create a list of all profile dates
profile_dates <- paste0(unique(date(working_data$Sampledate)))

# Establish the acceptable ranges for each parameter, for each station -----------------------------------------
  # The order is [1]extreme_min, [2]extreme_max, [3]standard deviation multiplier 
if(station == "KSBP01"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "CK200P"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "HNFD01"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "JSUR01"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2.5),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2.5),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 3),
    SigmaT = c(0, 35, 2))
} else if(station == "KSSK02"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LSEP01"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LSKQ06"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LSNT01"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LSVV01"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LTBC43"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LTED04"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LTKE03"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "LTUM03"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "MSJN02"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "NSEX01"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PENNCOVECW"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PENNCOVEENT"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PENNCOVEPNN001"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PENNCOVEWEST"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PossDO-2"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PSUSANBUOY"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PSUSANENT"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PSUSANKP"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "PTWILLBUOY"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "SARATOGACH"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "SARATOGAOP"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "SARATOGARP"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
} else if(station == "SEAQYSI"){
  rv <- list(
    Depth = c(0, 500, 2),
    Chlorophyll = c(0, 200, 2),
    Density = c(500, 1100, 2),
    DO = c(0, 20, 2),
    SigmaTheta = c(0, 35, 2),
    Light_Transmission = c(0, 100, 2),
    PAR = c(-100, 10000, 2),
    Surface_PAR = c(0, 5000, 2),
    Salinity = c(0, 40, 2),
    Temperature = c(0, 25, 2),
    Turbidity = c(0, 125, 2),
    NO23 = c(0, 5, 2),
    SigmaT = c(0, 35, 2))
}

# Download discrete data for your station --------------------------------------------------

good_quals_discrete <- c(0, 1, 2)
ParmID <- c(1, 5, 14, 18, 22)
ParmName <- c("ChlorophyllA", "DO", "NNN", "Salinity", "Temperature")
DisplayName <- c("Chlorophyll a", 
                 "Dissolved Oxygen", 
                 "Nitrite + Nitrate Nitrogen", 
                 "Salinity", 
                 "Temperature")
discrete_parms <- tibble(ParmID, ParmName, DisplayName)

parms_to_dl <- c("DO", "Salinity", "NNN", "ChlorophyllA", "Temperature")
dl_name <- "temporary_download.csv"
bottle_data_full <- download_discrete(station, parms_to_dl, dl_name)

bottle_data <- bottle_data_full %>% filter(CollectDate >= date_begin, 
                                           CollectDate <= date_end)
if (nrow(bottle_data) == 0) {
  discrete <- F
  message("No discrete values in specified date range")
}


# Functions ---------------------------------------------------------------

# Moves png files from here("output") to the appropriate folder for the given QC test
# Helps with file organization
move_png_files <- function(save_dir, QC_test_type, locator){
  png_output <- list.files(save_dir, pattern = ".png")[which(str_detect(list.files(save_folder, pattern = ".png"), QC_test_type))]
  print(paste0("Files moved to ", paste0(save_dir,"/",QC_test_type, "/", locator),":"))
  if(length(png_output) == 0){
    print(paste("None - no files to move from", QC_test_type))
  } else{
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
  
  png(paste0(save_folder, "/", station, "_", date_to_plot, "_", QC_test, ".png"), width = 1500, height = 1500)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols = 3)
  dev.off()
}

