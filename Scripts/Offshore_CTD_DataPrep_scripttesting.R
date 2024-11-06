# This script loads the necessary data for the offshore tests in script form
# The file Offshore_CTD_DataPrep.R is for use with Quarto docs, and doesn't load everything.


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
library(patchwork)
source(here("contour_functions.R"))
source(here("Scripts", "QC_functions.R"))

# Setup -------------------------------------------------------------------

# This is a version for use with R scripts instead of within Quarto docs. 
# The version referenced in the quarto doc has a bunch of stuff removed so that it doesn't work with scripts. 

# What station do you want figures from?
station <- dlgInput("Enter your station", "KSBP01")$res
date_begin <- dlgInput("Enter the date of the first profile to QC (YYYY-MM-DD)", "2024-01-01")$res
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
  mutate(Sampledate = ymd_hms(Sampledate),
         Year = year(Sampledate), 
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

# Calculate baseline mean and standard deviation 
baseline <- baseline_data %>% 
  group_by(Month, BinDepth) %>% 
  summarize(across(Density:NO23, 
                   list(mean = ~ mean(.x, na.rm = TRUE), 
                        sd = ~ sd(.x, na.rm = TRUE), 
                        n = ~ n())))

# Create final working df (working_data) -------------------------------------------------
# This df has all of the data that we will be working with - 
  # baselines, standard deviations, flags
  # it is also filtered to the time range of interest

working_data <- full_join(CTDdata, baseline) %>%
  filter(Sampledate > ymd(date_begin),
         Sampledate < ymd(date_end)) %>%
  mutate(Sampledate = ymd_hms(Sampledate),
         cast_date = date(Sampledate),
         num_date_ctd = as.numeric(Sampledate))

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

dl_new_discrete <- dlgInput("Do you need to download new discrete data? y/n", "y")$res

if(dl_new_discrete %in% c("y", "Y", "yes", "YES")){
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
  dl_name <- "discrete_bottle_temporary_download.csv"
  bottle_data_full <- download_discrete(station, parms_to_dl, dl_name)
  
  bottle_data <- bottle_data_full %>% filter(CollectDate >= date_begin, 
                                             CollectDate <= date_end)
  if (nrow(bottle_data) == 0) {
    discrete <- F
    message("No discrete values in specified date range")
  }
} else{
  good_quals_discrete <- c(0, 1, 2)
  bottle_data_full <- read_csv(here("discrete_bottle_temporary_download.csv"))
  bottle_data <- bottle_data_full %>% filter(CollectDate >= date_begin, 
                                             CollectDate <= date_end)
}

