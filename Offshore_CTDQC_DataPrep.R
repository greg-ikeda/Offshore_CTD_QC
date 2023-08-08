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

# Create individual dfs for each profile of interest ----------------------
  # Each individual profile is appended as an element of the "profiles" list

dates_QC <- paste0(unique(working_data$Date)) # Creates a vector of all the profile dates in the range of interest
profiles <- list()
count <- 0
# Creates a list element containing the data for each unique profile
for(castdate in dates_QC){
  count <- count + 1
  profiles[[count]] <- working_data %>%
    filter(ymd(working_data$Date) == ymd(castdate))
}


# Establish the acceptable ranges for each parameter -----------------------------------------
  # The order is [1]extreme_min, [2]extreme_max, [3]standard deviation multiplier 

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
