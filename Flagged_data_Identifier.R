# This script will parse a QC'd CTD data file and pull out only the profiles with data flags
# This is to compare the data from the manual QC to the auto QC, for calibration purposes.

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

# # Where do you want to save output?
save_folder <- here("output")

folder <- paste0("//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/", station, "/")
fname <- list.files(folder, pattern = "_qcd.csv")

shell.exec(here("output"))


# # Identify all qual types -------------------------------------------------
# 
# qual_vars <- colnames(CTDdata)[which(str_detect(cols, "_Qual"))]
# 
# qual_cols <- CTDdata %>%
#   select(all_of(qual_vars)) 
# 
# qual_types <- c()
# for(i in seq(1, length(qual_cols))){
#   qual_types <- append(qual_types, unlist(unique(qual_cols[i])))
# }
# 
# unique(tibble(qual_types))

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

color_scale <- c("E" = "orange",
                 "rej" = "red",
                 "TA, E" = "yellow",
                 "TA" = "purple",
                 "E, Rej" = "red",
                 "Rej" = "red",
                 "E, Rej" = "red",
                 "REJ" = "red",
                 "E, rej" = "red",
                 "R, E" = "red",
                 "R" = "red",
                 "q" = "orange",
                 "Rej, E, TA" = "red",
                 "E, TA" = "yellow",
                 "TA, Q" = "orange",
                 "Q" = "orange",
                 "E, TA, rej" = "red",
                 "R, TA" = "red",
                 "Rej, E" = "red")

# Identify profiles with flagged T data -------------------------------------
T_data_flagged <- CTDdata %>%
  filter(!is.na(Temperature_Qual))
T_data_flagged_dates <-  unique(T_data_flagged$Date)

T_data_flagged <- CTDdata %>%
  filter(Date %in% T_data_flagged_dates)

for(profiledate in T_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- T_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = Temperature, y = Depth, 
                   color = Temperature_Qual,
                   size = is.na(Temperature_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Temperature (°C)", y = "Depth (m)")

    ggsave(here("output", "manually_flagged_data",  station, "Temperature", paste(station, date, "TemperatureError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

# Identify profiles with flagged S data -------------------------------------
S_data_flagged <- CTDdata %>%
  filter(!is.na(Salinity_Qual))
S_data_flagged_dates <-  unique(S_data_flagged$Date)

S_data_flagged <- CTDdata %>%
  filter(Date %in% S_data_flagged_dates)

for(profiledate in S_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- S_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = Salinity, y = Depth, 
                   color = Salinity_Qual,
                   size = is.na(Salinity_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Salinity (PSU)", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "Salinity", paste(station, date, "SalinityError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}


# Identify profiles with flagged DO data -------------------------------------
DO_data_flagged <- CTDdata %>%
  filter(!is.na(DO_Qual))
DO_data_flagged_dates <-  unique(DO_data_flagged$Date)

DO_data_flagged <- CTDdata %>%
  filter(Date %in% DO_data_flagged_dates)

for(profiledate in DO_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- DO_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = DO, y = Depth, 
                   color = DO_Qual,
                   size = is.na(DO_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Dissolved Oxygen (mg/L)", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "DO", paste(station, date, "DOError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

# Identify profiles with flagged Chl data -------------------------------------
Chlorophyll_data_flagged <- CTDdata %>%
  filter(!is.na(Chlorophyll_Qual))
Chlorophyll_data_flagged_dates <-  unique(Chlorophyll_data_flagged$Date)

Chlorophyll_data_flagged <- CTDdata %>%
  filter(Date %in% Chlorophyll_data_flagged_dates)

for(profiledate in Chlorophyll_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- Chlorophyll_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = Chlorophyll, y = Depth, 
                   color = Chlorophyll_Qual,
                   size = is.na(Chlorophyll_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Chlorophyll (µg/L)", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "Chlorophyll", paste(station, date, "ChlorophyllError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

# Identify profiles with flagged N data -------------------------------------
NO23_data_flagged <- CTDdata %>%
  filter(!is.na(NO23_Qual))
NO23_data_flagged_dates <-  unique(NO23_data_flagged$Date)

NO23_data_flagged <- CTDdata %>%
  filter(Date %in% NO23_data_flagged_dates)

for(profiledate in NO23_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- NO23_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = NO23, y = Depth, 
                   color = NO23_Qual,
                   size = is.na(NO23_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Nitrate + Nitrite (mgN/L)", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "NO23", paste(station, date, "NO23Error.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

# Identify profiles with flagged Density data -------------------------------------
Density_data_flagged <- CTDdata %>%
  filter(!is.na(Density_Qual))
Density_data_flagged_dates <-  unique(Density_data_flagged$Date)

Density_data_flagged <- CTDdata %>%
  filter(Date %in% Density_data_flagged_dates)

for(profiledate in Density_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- Density_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = Density, y = Depth, 
                   color = Density_Qual,
                   size = is.na(Density_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = " 	kg/m^3", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "Density", paste(station, date, "DensityError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

# Identify profiles with flagged SigmaTheta data -------------------------------------
SigmaTheta_data_flagged <- CTDdata %>%
  filter(!is.na(SigmaTheta_Qual))
SigmaTheta_data_flagged_dates <-  unique(SigmaTheta_data_flagged$Date)

SigmaTheta_data_flagged <- CTDdata %>%
  filter(Date %in% SigmaTheta_data_flagged_dates)

for(profiledate in SigmaTheta_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- SigmaTheta_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = SigmaTheta, y = Depth, 
                   color = SigmaTheta_Qual,
                   size = is.na(SigmaTheta_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Sigma-Theta Potential Density (kg/m^3)", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "SigmaTheta", paste(station, date, "SigmaThetaError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

# Identify profiles with flagged Light Transmission data -------------------------------------
Light_Transmission_data_flagged <- CTDdata %>%
  filter(!is.na(Light_Qual))
Light_Transmission_data_flagged_dates <-  unique(Light_Transmission_data_flagged$Date)

Light_Transmission_data_flagged <- CTDdata %>%
  filter(Date %in% Light_Transmission_data_flagged_dates)

for(profiledate in Light_Transmission_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- Light_Transmission_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = Light_Transmission, y = Depth, 
                   color = Light_Qual,
                   size = is.na(Light_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "Transmissivity (% Light)", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "Light_Transmission", paste(station, date, "Light_TransmissionError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

# Identify profiles with flagged PAR data -------------------------------------
PAR_data_flagged <- CTDdata %>%
  filter(!is.na(PAR_Qual))
PAR_data_flagged_dates <-  unique(PAR_data_flagged$Date)

PAR_data_flagged <- CTDdata %>%
  filter(Date %in% PAR_data_flagged_dates)

for(profiledate in PAR_data_flagged_dates){
  date <- as_date(profiledate)
  plot_data <- PAR_data_flagged %>%
    filter(Date == ymd(date))
  
  p1 <- ggplot(data = plot_data) +
    theme_bw() +
    # theme(legend.position = "none") +
    scale_color_manual(values = color_scale) +
    guides(size = FALSE) +
    geom_point(aes(x = PAR, y = Depth, 
                   color = PAR_Qual,
                   size = is.na(PAR_Qual))) + 
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2))+
    scale_y_reverse() +
    labs(x = "PAR (μmol/s/m^2)", y = "Depth (m)")
  
  ggsave(here("output", "manually_flagged_data",  station, "PAR", paste(station, date, "PARError.png", sep = "_")),
         p1, dpi = 600, width = 10, height = 10)
}

