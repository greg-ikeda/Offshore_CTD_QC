# # TO DO LIST
# - Test adding partial = T to the rollapply (maybe rollmean). This provides an envelope for the entire profile, without missing data at the top and bottom
# - Add na.rm = T to rollapply/rollmean
# - Try using % of rolling mean below ~60 m for params like chlorophyll and PAR 
# ---- Try it on the entire profile, as well as below a certain depth
# - EVENTUALLY add a line in case_when() to specify what happens with good data (aka, no flag in the _qual column).

# source(here("Offshore_CTDQC_DataPrep.R"))

QC_test <- "stnd_dev"
test_save_dir <- paste0(save_folder, "/", QC_test)

# Libraries and such ------------------------------------------------------

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
source(here("contour_functions.R"))

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
         profile_date = ymd(paste(Year, Month, Day, sep = "-")),
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

working_data <- CTDdata


# TEST DATA. Transmissivity profile on 5/2/2022 ------------------------------------
# Note - the rollapply function can be used too:
# LT_rolling_avg_25 = rollapply(Light_Transmission, width = 25, fill = NA, FUN = mean) This is equivalent to rollmean().
# Testing only on the spiky 

# Run 1 of 2. Running a first pass to remove egregious anomalies. This is unlikely to register small spikes.
LT_spikes1 <- CTDdata %>%
  filter(profile_date == ymd("2022/5/2")) %>%
  mutate(LT_rolling_avg_5 = rollmean(Light_Transmission, k = 5, fill = NA),
         LT_rolling_avg_15 = rollmean(Light_Transmission, k = 15, fill = NA),
         LT_rolling_avg_25 = rollmean(Light_Transmission, k = 25, fill = NA),
         LT_rolling_avg_35 = rollmean(Light_Transmission, k = 35, fill = NA),
         LT_rolling_sd_5 = rollapply(Light_Transmission, width = 5, fill = NA, FUN = sd),
         LT_rolling_sd_15 = rollapply(Light_Transmission, width = 15, fill = NA, FUN = sd),
         LT_rolling_sd_25 = rollapply(Light_Transmission, width = 25, fill = NA, FUN = sd),
         LT_rolling_sd_35 = rollapply(Light_Transmission, width = 35, fill = NA, FUN = sd)) %>%
  mutate(LT_qual_5 = case_when(Light_Transmission < (LT_rolling_avg_5 - (2*LT_rolling_sd_5)) ~ "q1",
                               Light_Transmission > (LT_rolling_avg_5 + (2*LT_rolling_sd_5)) ~ "q1"),
         LT_qual_15 = case_when(Light_Transmission < (LT_rolling_avg_15 - (2*LT_rolling_sd_15)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_15 + (2*LT_rolling_sd_15)) ~ "q1"),
         LT_qual_25 = case_when(Light_Transmission < (LT_rolling_avg_25 - (2*LT_rolling_sd_25)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_25 + (2*LT_rolling_sd_25)) ~ "q1"),
         LT_qual_35 = case_when(Light_Transmission < (LT_rolling_avg_35 - (2*LT_rolling_sd_35)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_35 + (2*LT_rolling_sd_35)) ~ "q1"),
         LT_qual_35_3sd = case_when(Light_Transmission < (LT_rolling_avg_35 - (3*LT_rolling_sd_35)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_35 + (3*LT_rolling_sd_35)) ~ "q1"),)

# TEST DATA. Run 2 of 2. Flagged data from pass 1 are removed to tighten the rolling sd range
LT_spikes2 <- LT_spikes1 %>%
  filter(is.na(LT_qual_35_3sd)) %>%
  mutate(LT_rolling_avg_5 = rollmean(Light_Transmission, k = 5, fill = NA),
         LT_rolling_avg_15 = rollmean(Light_Transmission, k = 15, fill = NA),
         LT_rolling_avg_25 = rollmean(Light_Transmission, k = 25, fill = NA),
         LT_rolling_avg_35 = rollmean(Light_Transmission, k = 35, fill = NA),
         LT_rolling_sd_5 = rollapply(Light_Transmission, width = 5, fill = NA, FUN = sd),
         LT_rolling_sd_15 = rollapply(Light_Transmission, width = 15, fill = NA, FUN = sd),
         LT_rolling_sd_25 = rollapply(Light_Transmission, width = 25, fill = NA, FUN = sd),
         LT_rolling_sd_35 = rollapply(Light_Transmission, width = 35, fill = NA, FUN = sd)) %>%
  mutate(LT_qual_5 = case_when(Light_Transmission < (LT_rolling_avg_5 - (2*LT_rolling_sd_5)) ~ "q2",
                               Light_Transmission > (LT_rolling_avg_5 + (2*LT_rolling_sd_5)) ~ "q2"),
         LT_qual_15 = case_when(Light_Transmission < (LT_rolling_avg_15 - (2*LT_rolling_sd_15)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_15 + (2*LT_rolling_sd_15)) ~ "q2"),
         LT_qual_25 = case_when(Light_Transmission < (LT_rolling_avg_25 - (2*LT_rolling_sd_25)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_25 + (2*LT_rolling_sd_25)) ~ "q2"),
         LT_qual_35 = case_when(Light_Transmission < (LT_rolling_avg_35 - (2*LT_rolling_sd_35)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_35 + (2*LT_rolling_sd_35)) ~ "q2"))


# Transmissivity 5/2/2022 - Run 1 -----------------------------------------

(LT_may2_2022_5 <- ggplot(LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_5,
                    ymin = LT_rolling_avg_5 - (2*LT_rolling_sd_5),
                    ymax = LT_rolling_avg_5 + (2*LT_rolling_sd_5)),
                alpha = 0.2,
                fill = "blue")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle("Pass 1, k = 5, 2 standard devs")

(LT_may2_2022_15 <- ggplot(LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_15),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_15,
                    ymin = LT_rolling_avg_15 - (2*LT_rolling_sd_15),
                    ymax = LT_rolling_avg_15 + (2*LT_rolling_sd_15)),
                alpha = 0.2,
                fill = "green")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 15, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_15))))



(LT_may2_2022_25 <- ggplot(LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_25),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_25 - (2*LT_rolling_sd_25),
                    ymax = LT_rolling_avg_25 + (2*LT_rolling_sd_25)),
                alpha = 0.2,
                fill = "orange")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_25))))

(LT_may2_2022_35 <- ggplot(LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_35),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_35 - (2*LT_rolling_sd_35),
                    ymax = LT_rolling_avg_35 + (2*LT_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 35, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_35))))

(LT_may2_2022_35 <- ggplot(LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_35_3sd),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_35 - (3*LT_rolling_sd_35),
                    ymax = LT_rolling_avg_35 + (3*LT_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 35, 3 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_35))))


# Transmissivity 5/2/2022 - Run 2 -----------------------------------------------------------------

(LT_may2_2022_5 <- ggplot(LT_spikes2)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_5,
                    ymin = LT_rolling_avg_5 - (2*LT_rolling_sd_5),
                    ymax = LT_rolling_avg_5 + (2*LT_rolling_sd_5)),
                alpha = 0.2,
                fill = "blue")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle("Pass 2, k = 5, 2 standard devs")

(LT_may2_2022_15 <- ggplot(LT_spikes2)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_15),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_15,
                    ymin = LT_rolling_avg_15 - (2*LT_rolling_sd_15),
                    ymax = LT_rolling_avg_15 + (2*LT_rolling_sd_15)),
                alpha = 0.2,
                fill = "green")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 15, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes2$LT_qual_15))))

(LT_may2_2022_25 <- ggplot(LT_spikes2)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_25),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_25 - (2*LT_rolling_sd_25),
                    ymax = LT_rolling_avg_25 + (2*LT_rolling_sd_25)),
                alpha = 0.2,
                fill = "orange")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes2$LT_qual_25))))

(LT_may2_2022_35 <- ggplot(LT_spikes2)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_35),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_35 - (2*LT_rolling_sd_35),
                    ymax = LT_rolling_avg_35 + (2*LT_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 35, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes2$LT_qual_35))))


# TEST DATA. Chlorophyll and Transmissivity profile on 5/17/2021 ------------------------------------

# Run 1 of 3 - Both Chl and Light. 
Chl_LT_spikes1 <- CTDdata %>%
  filter(profile_date == ymd("2021/5/17")) %>%
  mutate(LT_rolling_avg_5 = rollmean(Light_Transmission, k = 5, fill = NA),
         LT_rolling_avg_15 = rollmean(Light_Transmission, k = 15, fill = NA),
         LT_rolling_avg_25 = rollmean(Light_Transmission, k = 25, fill = NA),
         LT_rolling_avg_35 = rollmean(Light_Transmission, k = 35, fill = NA),
         LT_rolling_sd_5 = rollapply(Light_Transmission, width = 5, fill = NA, FUN = sd),
         LT_rolling_sd_15 = rollapply(Light_Transmission, width = 15, fill = NA, FUN = sd),
         LT_rolling_sd_25 = rollapply(Light_Transmission, width = 25, fill = NA, FUN = sd),
         LT_rolling_sd_35 = rollapply(Light_Transmission, width = 35, fill = NA, FUN = sd),
         Chl_rolling_avg_5 = rollmean(Chlorophyll, k = 5, fill = NA),
         Chl_rolling_avg_15 = rollmean(Chlorophyll, k = 15, fill = NA),
         Chl_rolling_avg_25 = rollmean(Chlorophyll, k = 25, fill = NA),
         Chl_rolling_avg_35 = rollmean(Chlorophyll, k = 35, fill = NA),
         Chl_rolling_sd_5 = rollapply(Chlorophyll, width = 5, fill = NA, FUN = sd),
         Chl_rolling_sd_15 = rollapply(Chlorophyll, width = 15, fill = NA, FUN = sd),
         Chl_rolling_sd_25 = rollapply(Chlorophyll, width = 25, fill = NA, FUN = sd),
         Chl_rolling_sd_35 = rollapply(Chlorophyll, width = 35, fill = NA, FUN = sd)) %>%
  mutate(LT_qual_5 = case_when(Light_Transmission < (LT_rolling_avg_5 - (2*LT_rolling_sd_5)) ~ "q1",
                               Light_Transmission > (LT_rolling_avg_5 + (2*LT_rolling_sd_5)) ~ "q1"),
         LT_qual_15 = case_when(Light_Transmission < (LT_rolling_avg_15 - (2*LT_rolling_sd_15)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_15 + (2*LT_rolling_sd_15)) ~ "q1"),
         LT_qual_25 = case_when(Light_Transmission < (LT_rolling_avg_25 - (2*LT_rolling_sd_25)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_25 + (2*LT_rolling_sd_25)) ~ "q1"),
         LT_qual_35 = case_when(Light_Transmission < (LT_rolling_avg_35 - (2*LT_rolling_sd_35)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_35 + (2*LT_rolling_sd_35)) ~ "q1"),
         LT_qual_35_3sd = case_when(Light_Transmission < (LT_rolling_avg_35 - (3*LT_rolling_sd_35)) ~ "q1",
                                Light_Transmission > (LT_rolling_avg_35 + (3*LT_rolling_sd_35)) ~ "q1"),
         Chl_qual_5 = case_when(Chlorophyll < (Chl_rolling_avg_5 - (2*Chl_rolling_sd_5)) ~ "q1",
                                Chlorophyll > (Chl_rolling_avg_5 + (2*Chl_rolling_sd_5)) ~ "q1"),
         Chl_qual_15 = case_when(Chlorophyll < (Chl_rolling_avg_15 - (2*Chl_rolling_sd_15)) ~ "q1",
                                 Chlorophyll > (Chl_rolling_avg_15 + (2*Chl_rolling_sd_15)) ~ "q1"),
         Chl_qual_25 = case_when(Chlorophyll < (Chl_rolling_avg_25 - (2*Chl_rolling_sd_25)) ~ "q1",
                                 Chlorophyll > (Chl_rolling_avg_25 + (2*Chl_rolling_sd_25)) ~ "q1"),
         Chl_qual_35 = case_when(Chlorophyll < (Chl_rolling_avg_35 - (2*Chl_rolling_sd_35)) ~ "q1",
                                 Chlorophyll > (Chl_rolling_avg_35 + (2*Chl_rolling_sd_35)) ~ "q1"),
         Chl_qual_35_3sd = case_when(Chlorophyll < (Chl_rolling_avg_35 - (3*Chl_rolling_sd_35)) ~ "q1",
                                 Chlorophyll > (Chl_rolling_avg_35 + (3*Chl_rolling_sd_35)) ~ "q1"),
         Chl_qual_35_4sd = case_when(Chlorophyll < (Chl_rolling_avg_35 - (4*Chl_rolling_sd_35)) ~ "q1",
                                     Chlorophyll > (Chl_rolling_avg_35 + (4*Chl_rolling_sd_35)) ~ "q1"))

# Run 2 of 3. Only filtering out 3sd spikes in LT from run 1
LT_spikes2_2021 <- Chl_LT_spikes1 %>%
  filter(is.na(LT_qual_35_3sd)) %>%
  mutate(LT_rolling_avg_5 = rollmean(Light_Transmission, k = 5, fill = NA),
         LT_rolling_avg_15 = rollmean(Light_Transmission, k = 15, fill = NA),
         LT_rolling_avg_25 = rollmean(Light_Transmission, k = 25, fill = NA),
         LT_rolling_avg_35 = rollmean(Light_Transmission, k = 35, fill = NA),
         LT_rolling_sd_5 = rollapply(Light_Transmission, width = 5, fill = NA, FUN = sd),
         LT_rolling_sd_15 = rollapply(Light_Transmission, width = 15, fill = NA, FUN = sd),
         LT_rolling_sd_25 = rollapply(Light_Transmission, width = 25, fill = NA, FUN = sd),
         LT_rolling_sd_35 = rollapply(Light_Transmission, width = 35, fill = NA, FUN = sd),
         Chl_rolling_avg_5 = rollmean(Chlorophyll, k = 5, fill = NA),
         Chl_rolling_avg_15 = rollmean(Chlorophyll, k = 15, fill = NA),
         Chl_rolling_avg_25 = rollmean(Chlorophyll, k = 25, fill = NA),
         Chl_rolling_avg_35 = rollmean(Chlorophyll, k = 35, fill = NA),
         Chl_rolling_sd_5 = rollapply(Chlorophyll, width = 5, fill = NA, FUN = sd),
         Chl_rolling_sd_15 = rollapply(Chlorophyll, width = 15, fill = NA, FUN = sd),
         Chl_rolling_sd_25 = rollapply(Chlorophyll, width = 25, fill = NA, FUN = sd),
         Chl_rolling_sd_35 = rollapply(Chlorophyll, width = 35, fill = NA, FUN = sd)) %>%
  mutate(LT_qual_5 = case_when(Light_Transmission < (LT_rolling_avg_5 - (2*LT_rolling_sd_5)) ~ "q2",
                               Light_Transmission > (LT_rolling_avg_5 + (2*LT_rolling_sd_5)) ~ "q2"),
         LT_qual_15 = case_when(Light_Transmission < (LT_rolling_avg_15 - (2*LT_rolling_sd_15)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_15 + (2*LT_rolling_sd_15)) ~ "q2"),
         LT_qual_25 = case_when(Light_Transmission < (LT_rolling_avg_25 - (2*LT_rolling_sd_25)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_25 + (2*LT_rolling_sd_25)) ~ "q2"),
         LT_qual_35 = case_when(Light_Transmission < (LT_rolling_avg_35 - (2*LT_rolling_sd_35)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_35 + (2*LT_rolling_sd_35)) ~ "q2"),
         Chl_qual_5 = case_when(Chlorophyll < (Chl_rolling_avg_5 - (2*Chl_rolling_sd_5)) ~ "q2",
                                Chlorophyll > (Chl_rolling_avg_5 + (2*Chl_rolling_sd_5)) ~ "q2"),
         Chl_qual_15 = case_when(Chlorophyll < (Chl_rolling_avg_15 - (2*Chl_rolling_sd_15)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_15 + (2*Chl_rolling_sd_15)) ~ "q2"),
         Chl_qual_25 = case_when(Chlorophyll < (Chl_rolling_avg_25 - (2*Chl_rolling_sd_25)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_25 + (2*Chl_rolling_sd_25)) ~ "q2"),
         Chl_qual_35 = case_when(Chlorophyll < (Chl_rolling_avg_35 - (2*Chl_rolling_sd_35)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_35 + (2*Chl_rolling_sd_35)) ~ "q2"))

# Run 3 of 3. Only filtering out 3sd spikes in Chl from run 1
Chl_spikes2_2021 <- Chl_LT_spikes1 %>%
  filter(is.na(Chl_qual_35_4sd)) %>%
  mutate(Chl_rolling_avg_5 = rollmean(Chlorophyll, k = 5, fill = NA),
         Chl_rolling_avg_15 = rollmean(Chlorophyll, k = 15, fill = NA),
         Chl_rolling_avg_25 = rollmean(Chlorophyll, k = 25, fill = NA),
         Chl_rolling_avg_35 = rollmean(Chlorophyll, k = 35, fill = NA),
         Chl_rolling_sd_5 = rollapply(Chlorophyll, width = 5, fill = NA, FUN = sd),
         Chl_rolling_sd_15 = rollapply(Chlorophyll, width = 15, fill = NA, FUN = sd),
         Chl_rolling_sd_25 = rollapply(Chlorophyll, width = 25, fill = NA, FUN = sd),
         Chl_rolling_sd_35 = rollapply(Chlorophyll, width = 35, fill = NA, FUN = sd),) %>%
  mutate(LT_qual_5 = case_when(Light_Transmission < (LT_rolling_avg_5 - (2*LT_rolling_sd_5)) ~ "q2",
                               Light_Transmission > (LT_rolling_avg_5 + (2*LT_rolling_sd_5)) ~ "q2"),
         LT_qual_15 = case_when(Light_Transmission < (LT_rolling_avg_15 - (2*LT_rolling_sd_15)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_15 + (2*LT_rolling_sd_15)) ~ "q2"),
         LT_qual_25 = case_when(Light_Transmission < (LT_rolling_avg_25 - (2*LT_rolling_sd_25)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_25 + (2*LT_rolling_sd_25)) ~ "q2"),
         LT_qual_35 = case_when(Light_Transmission < (LT_rolling_avg_35 - (2*LT_rolling_sd_35)) ~ "q2",
                                Light_Transmission > (LT_rolling_avg_35 + (2*LT_rolling_sd_35)) ~ "q2"),
         Chl_qual_5 = case_when(Chlorophyll < (Chl_rolling_avg_5 - (2*Chl_rolling_sd_5)) ~ "q2",
                                Chlorophyll > (Chl_rolling_avg_5 + (2*Chl_rolling_sd_5)) ~ "q2"),
         Chl_qual_15 = case_when(Chlorophyll < (Chl_rolling_avg_15 - (2*Chl_rolling_sd_15)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_15 + (2*Chl_rolling_sd_15)) ~ "q2"),
         Chl_qual_25 = case_when(Chlorophyll < (Chl_rolling_avg_25 - (2*Chl_rolling_sd_25)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_25 + (2*Chl_rolling_sd_25)) ~ "q2"),
         Chl_qual_35 = case_when(Chlorophyll < (Chl_rolling_avg_35 - (2*Chl_rolling_sd_35)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_35 + (2*Chl_rolling_sd_35)) ~ "q2"))

# Transmissivity 5/17/2021 - Run 1 ----------------------------------------

(LT_may2_2022_5 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_5,
                    ymin = LT_rolling_avg_5 - (2*LT_rolling_sd_5),
                    ymax = LT_rolling_avg_5 + (2*LT_rolling_sd_5)),
                alpha = 0.2,
                fill = "blue")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle("Pass 1, k = 5, 2 standard devs")

(LT_may2_2022_15 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_15),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_15,
                    ymin = LT_rolling_avg_15 - (2*LT_rolling_sd_15),
                    ymax = LT_rolling_avg_15 + (2*LT_rolling_sd_15)),
                alpha = 0.2,
                fill = "green")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 15, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_15))))



(LT_may2_2022_25 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_25),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_25 - (2*LT_rolling_sd_25),
                    ymax = LT_rolling_avg_25 + (2*LT_rolling_sd_25)),
                alpha = 0.2,
                fill = "orange")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_25))))

(LT_may2_2022_35 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_35),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_35 - (2*LT_rolling_sd_35),
                    ymax = LT_rolling_avg_35 + (2*LT_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 35, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_35))))

(LT_may2_2022_35 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_35_3sd),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_35 - (3*LT_rolling_sd_35),
                    ymax = LT_rolling_avg_35 + (3*LT_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 35, 3 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes1$LT_qual_35))))


# Transmissivity 5/17/2021 - Run 2 --------------------------------------

(LT_may2_2022_5 <- ggplot(LT_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_5,
                    ymin = LT_rolling_avg_5 - (2*LT_rolling_sd_5),
                    ymax = LT_rolling_avg_5 + (2*LT_rolling_sd_5)),
                alpha = 0.2,
                fill = "blue")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle("Pass 2, k = 5, 2 standard devs")

(LT_may2_2022_15 <- ggplot(LT_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_15),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_15,
                    ymin = LT_rolling_avg_15 - (2*LT_rolling_sd_15),
                    ymax = LT_rolling_avg_15 + (2*LT_rolling_sd_15)),
                alpha = 0.2,
                fill = "green")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 15, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes2_2021$LT_qual_15))))

(LT_may2_2022_25 <- ggplot(LT_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_25),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_25 - (2*LT_rolling_sd_25),
                    ymax = LT_rolling_avg_25 + (2*LT_rolling_sd_25)),
                alpha = 0.2,
                fill = "orange")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes2_2021$LT_qual_25))))

(LT_may2_2022_35 <- ggplot(LT_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Light_Transmission,
                   color = LT_qual_35),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = LT_rolling_avg_25,
                    ymin = LT_rolling_avg_35 - (2*LT_rolling_sd_35),
                    ymax = LT_rolling_avg_35 + (2*LT_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 35, 2 standard devs\r\n Flagged samples = ", sum(!is.na(LT_spikes2_2021$LT_qual_35))))


# Chlorophyll 5/17/2021 - Run 1 --------------------------------------

(Chl_may2_2022_5 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_5,
                    ymin = Chl_rolling_avg_5 - (2*Chl_rolling_sd_5),
                    ymax = Chl_rolling_avg_5 + (2*Chl_rolling_sd_5)),
                alpha = 0.2,
                fill = "blue")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle("Pass 1, k = 5, 2 standard devs")

(Chl_may2_2022_15 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_15),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_15,
                    ymin = Chl_rolling_avg_15 - (2*Chl_rolling_sd_15),
                    ymax = Chl_rolling_avg_15 + (2*Chl_rolling_sd_15)),
                alpha = 0.2,
                fill = "green")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 15, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_LT_spikes1$Chl_qual_15))))

(Chl_may2_2022_25 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_25),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_25,
                    ymin = Chl_rolling_avg_25 - (2*Chl_rolling_sd_25),
                    ymax = Chl_rolling_avg_25 + (2*Chl_rolling_sd_25)),
                alpha = 0.2,
                fill = "orange")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_LT_spikes1$Chl_qual_25))))

(Chl_may2_2022_35 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_35),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_35,
                    ymin = Chl_rolling_avg_35 - (2*Chl_rolling_sd_35),
                    ymax = Chl_rolling_avg_35 + (2*Chl_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 35, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_LT_spikes1$Chl_qual_35))))

(Chl_may2_2022_35 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_35_3sd),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_35,
                    ymin = Chl_rolling_avg_35 - (3*Chl_rolling_sd_35),
                    ymax = Chl_rolling_avg_35 + (3*Chl_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 35, 3 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_LT_spikes1$Chl_qual_35_3sd))))

(Chl_may2_2022_35 <- ggplot(Chl_LT_spikes1)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_35_4sd),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_35,
                    ymin = Chl_rolling_avg_35 - (4*Chl_rolling_sd_35),
                    ymax = Chl_rolling_avg_35 + (4*Chl_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 1, k = 35, 4 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_LT_spikes1$Chl_qual_35_4sd))))

# Chlorophyll 5/17/2021 - Run 2 --------------------------------------

(Chl_may2_2022_5 <- ggplot(Chl_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_5,
                    ymin = Chl_rolling_avg_5 - (2*Chl_rolling_sd_5),
                    ymax = Chl_rolling_avg_5 + (2*Chl_rolling_sd_5)),
                alpha = 0.2,
                fill = "blue")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle("Pass 2, k = 5, 2 standard devs")

(Chl_may2_2022_15 <- ggplot(Chl_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_15),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_15,
                    ymin = Chl_rolling_avg_15 - (2*Chl_rolling_sd_15),
                    ymax = Chl_rolling_avg_15 + (2*Chl_rolling_sd_15)),
                alpha = 0.2,
                fill = "green")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 15, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_spikes2_2021$Chl_qual_15))))

(Chl_may2_2022_25 <- ggplot(Chl_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_25),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_25,
                    ymin = Chl_rolling_avg_25 - (2*Chl_rolling_sd_25),
                    ymax = Chl_rolling_avg_25 + (2*Chl_rolling_sd_25)),
                alpha = 0.2,
                fill = "orange")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_spikes2_2021$Chl_qual_25))))

(Chl_may2_2022_25 <- ggplot(Chl_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_35),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_35,
                    ymin = Chl_rolling_avg_35 - (3*Chl_rolling_sd_35),
                    ymax = Chl_rolling_avg_35 + (3*Chl_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_spikes2_2021$Chl_qual_25))))


# Rolling Average of Rolling Standard Deviations --------------------------

#Test to determine an appropriate mean width.
# Best one appears to be kvalue = 50 for chlorophyll data

rolltest_1 <- Chl_LT_spikes1 %>%
  mutate(Chl_rolling_avg_35 = rollmean(Chlorophyll, k = 35, fill = NA),
         Chl_rolling_sd = rollapply(Chlorophyll, width = 5, fill = NA, FUN = sd),
         rolling_avg_roll_sd = rollmean((6 * Chl_rolling_sd), k = 50, fill = NA)) %>%
  mutate(Chl_qual_35 = case_when(Chlorophyll < (Chl_rolling_avg_35 - (2*Chl_rolling_sd_35)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_35 + (2*Chl_rolling_sd_35)) ~ "q2"),
         Chl_qual_WEIRD = case_when(Chlorophyll < (Chl_rolling_avg_35 - rolling_avg_roll_sd) ~ "q2",
                                    Chlorophyll > (Chl_rolling_avg_35 + rolling_avg_roll_sd) ~ "q2"))

rolltest_2 <- rolltest_1 %>%
  filter(is.na(Chl_qual_WEIRD)) %>%
  mutate(Chl_rolling_avg_35 = rollmean(Chlorophyll, k = 35, fill = NA),
         Chl_rolling_sd = rollapply(Chlorophyll, width = 5, fill = NA, FUN = sd),
         rolling_avg_roll_sd = rollmean((6 * Chl_rolling_sd), k = 50, fill = NA)) %>%
  mutate(Chl_qual_35 = case_when(Chlorophyll < (Chl_rolling_avg_35 - (2*Chl_rolling_sd_35)) ~ "q2",
                                 Chlorophyll > (Chl_rolling_avg_35 + (2*Chl_rolling_sd_35)) ~ "q2"),
         Chl_qual_WEIRD = case_when(Chlorophyll < (Chl_rolling_avg_35 - rolling_avg_roll_sd) ~ "q2",
                                    Chlorophyll > (Chl_rolling_avg_35 + rolling_avg_roll_sd) ~ "q2"))

# Plotting rolling avg of rolling sd plots. 
# Run 1

print(rolltest_1_plot <- ggplot(rolltest_1)+
        geom_point(aes(x = Depth,
                       y = Chlorophyll,
                       color = Chl_qual_WEIRD),
                   alpha = 0.4)+
        geom_ribbon(aes(x = Depth,
                        y = Chl_rolling_avg_5,
                        ymin = Chl_rolling_avg_35 - (rolling_avg_roll_sd),
                        ymax = Chl_rolling_avg_35 + (rolling_avg_roll_sd)),
                    alpha = 0.2,
                    fill = "green")+
        scale_x_reverse()+
        coord_flip()+
        ylim(-2, 17)+
        ggtitle(paste0("Pass 1, 2 standard devs\r\n Flagged samples = ", sum(!is.na(rolltest_1$Chl_qual_WEIRD)),
                       "   ", "sd_multiplier = ", multiplier,
                       "   ", "sd_width = ", sd_width,
                       "   ", "mean_width = ", kvalue)))

print(rolltest_1_plot <- ggplot(rolltest_1)+
        geom_point(aes(x = Depth,
                       y = Chlorophyll,
                       color = Chl_qual_WEIRD),
                   alpha = 0.4)+
        geom_ribbon(aes(x = Depth,
                        y = Chl_rolling_avg_5,
                        ymin = Chl_rolling_avg_35 - (rolling_avg_roll_sd),
                        ymax = Chl_rolling_avg_35 + (rolling_avg_roll_sd)),
                    alpha = 0.2,
                    fill = "green")+
        scale_x_reverse()+
        coord_flip()+
        ylim(-2, 17)+
        ggtitle(paste0("Pass 1, 2 standard devs\r\n Flagged samples = ", sum(!is.na(rolltest_1$Chl_qual_WEIRD)),
                       "   ", "sd_multiplier = ", multiplier,
                       "   ", "sd_width = ", sd_width,
                       "   ", "mean_width = ", kvalue)))
print(rolltest_1_plot <- ggplot(rolltest_1)+
        geom_point(aes(x = Depth,
                       y = Chlorophyll,
                       color = Chl_qual_WEIRD),
                   alpha = 0.4)+
        geom_ribbon(aes(x = Depth,
                        y = Chl_rolling_avg_5,
                        ymin = Chl_rolling_avg_35 - (rolling_avg_roll_sd),
                        ymax = Chl_rolling_avg_35 + (rolling_avg_roll_sd)),
                    alpha = 0.2,
                    fill = "green")+
        scale_x_reverse()+
        coord_flip()+
        ylim(-2, 17)+
        ggtitle(paste0("Pass 1, 2 standard devs\r\n Flagged samples = ", sum(!is.na(rolltest_1$Chl_qual_WEIRD)),
                       "   ", "sd_multiplier = ", multiplier,
                       "   ", "sd_width = ", sd_width,
                       "   ", "mean_width = ", kvalue)))
#Run 2

print(rolltest_2_plot <- ggplot(rolltest_2)+
        geom_point(aes(x = Depth,
                       y = Chlorophyll,
                       color = Chl_qual_WEIRD),
                   alpha = 0.4)+
        geom_ribbon(aes(x = Depth,
                        y = Chl_rolling_avg_5,
                        ymin = Chl_rolling_avg_35 - (rolling_avg_roll_sd),
                        ymax = Chl_rolling_avg_35 + (rolling_avg_roll_sd)),
                    alpha = 0.2,
                    fill = "blue")+
        scale_x_reverse()+
        coord_flip()+
        ylim(-2, 17)+
        ggtitle(paste0("Pass 1, 2 standard devs\r\n Flagged samples = ", sum(!is.na(rolltest_2$Chl_qual_WEIRD)),
                       "   ", "sd_multiplier = ", multiplier,
                       "   ", "sd_width = ", sd_width,
                       "   ", "mean_width = ", kvalue)))

# Comparison Plots
(Chl_may2_2022_25 <- ggplot(Chl_spikes2_2021)+
    geom_point(aes(x = Depth,
                   y = Chlorophyll,
                   color = Chl_qual_35),
               alpha = 0.4)+
    geom_ribbon(aes(x = Depth,
                    y = Chl_rolling_avg_35,
                    ymin = Chl_rolling_avg_35 - (3*Chl_rolling_sd_35),
                    ymax = Chl_rolling_avg_35 + (3*Chl_rolling_sd_35)),
                alpha = 0.2,
                fill = "purple")+
    scale_x_reverse()+
    coord_flip())+
  ggtitle(paste0("Pass 2, k = 25, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Chl_spikes2_2021$Chl_qual_25))))



# WEIRD TEST. This actually works well - rolling average of rolling standard deviations  --------
#Test to determine an appropriate mean width.
# Best one appears to be kvalue = 50 for chlorophyll data

for(i in c(50)){
  multiplier <- 6 # A large multiplier for standard deviation 
  kvalue <- i # A large number of samples for calculating the rolling average. Rolling avg is the avg of kvalue sample before/after
  sd_width <- 5 # a small number for rolling standard deviation. This is spiky on its own, but get smoothed by including a rolling average
  
  
  Weird_test_df <- Chl_LT_spikes1 %>%
    filter(is.na(Chl_qual_35_4sd)) %>%
    mutate(Chl_rolling_avg_5 = rollmean(Chlorophyll, k = 5, fill = NA),
           Chl_rolling_avg_15 = rollmean(Chlorophyll, k = 15, fill = NA),
           Chl_rolling_avg_25 = rollmean(Chlorophyll, k = 25, fill = NA),
           Chl_rolling_avg_35 = rollmean(Chlorophyll, k = 35, fill = NA),
           Chl_rolling_sd = rollapply(Chlorophyll, width = sd_width, fill = NA, FUN = sd),
           weird_test = rollmean((multiplier * Chl_rolling_sd), k = kvalue, fill = NA)) %>%
    mutate(weird_qual = case_when(Chlorophyll < (Chl_rolling_avg_35 - (weird_test)) ~ "q2",
                                  Chlorophyll > (Chl_rolling_avg_35 + (weird_test)) ~ "q2"),
           Chl_qual_5 = case_when(Chlorophyll < (Chl_rolling_avg_5 - (2*Chl_rolling_sd_5)) ~ "q2",
                                  Chlorophyll > (Chl_rolling_avg_5 + (2*Chl_rolling_sd_5)) ~ "q2"),
           Chl_qual_15 = case_when(Chlorophyll < (Chl_rolling_avg_15 - (2*Chl_rolling_sd_15)) ~ "q2",
                                   Chlorophyll > (Chl_rolling_avg_15 + (2*Chl_rolling_sd_15)) ~ "q2"),
           Chl_qual_25 = case_when(Chlorophyll < (Chl_rolling_avg_25 - (2*Chl_rolling_sd_25)) ~ "q2",
                                   Chlorophyll > (Chl_rolling_avg_25 + (2*Chl_rolling_sd_25)) ~ "q2"),
           Chl_qual_35 = case_when(Chlorophyll < (Chl_rolling_avg_35 - (2*Chl_rolling_sd_35)) ~ "q2",
                                   Chlorophyll > (Chl_rolling_avg_35 + (2*Chl_rolling_sd_35)) ~ "q2"),
           Chl_qual_WEIRD = case_when(Chlorophyll < (Chl_rolling_avg_35 - weird_test) ~ "q2",
                                      Chlorophyll > (Chl_rolling_avg_35 + weird_test) ~ "q2"),)
  
  
  weird_only <- Weird_test_df %>%
    filter(!is.na(Chl_qual_WEIRD)) %>%
    select(Depth, Chlorophyll, Chl_qual_WEIRD)
  
  print(weird_test_plot <- ggplot(Weird_test_df)+
          geom_point(aes(x = Depth,
                         y = Chlorophyll,
                         color = Chl_qual_WEIRD),
                     alpha = 0.4)+
          geom_ribbon(aes(x = Depth,
                          y = Chl_rolling_avg_5,
                          ymin = Chl_rolling_avg_35 - (weird_test),
                          ymax = Chl_rolling_avg_35 + (weird_test)),
                      alpha = 0.2,
                      fill = "green")+
          scale_x_reverse()+
          coord_flip()+
          ylim(-2, 17)+
          ggtitle(paste0("Pass 1, 2 standard devs\r\n Flagged samples = ", sum(!is.na(Weird_test_df$Chl_qual_WEIRD)),
                         "   ", "sd_multiplier = ", multiplier,
                         "   ", "sd_width = ", sd_width,
                         "   ", "mean_width = ", kvalue)))
  ggsave(path = save_folder,
         filename = paste0("rolling_test_", kvalue, ".png"))
}
