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

test_data <- here("data", "KSBP01_UpcastDowncast.csv")

percent_diff <- function(v1, v2){
  diff_perc <- 100 * abs(v1-v2)/((v1+v2)/2)
  return(diff_perc)
}

# Load data and calculate baseline/Standard deviations --------------------------

bin_width <- 0.5

# CTDdata <- import_CTD(paste0(folder, fname))
# Note that not binning&averaging may create problems in the future
# Something to be aware of!
CTDdata <- import_CTD(test_data) %>% 
  mutate(Year = year(Sampledate), 
         Month = month(Sampledate), 
         Day = day(Sampledate), 
         YearDay = yday(Sampledate),
         Date = as.Date(Sampledate), 
         profile_date = ymd(paste(Year, Month, Day, sep = "-")),
         depth_bin(Depth, bin_width)) %>% 
  select(-CastNotes, -Depth, -Sampledate) %>% 
  relocate(contains("_Qual"), .after = NO23)

CTDdata_up <- CTDdata %>%
  filter(Updown == "Up") %>% 
  rename_with(~ paste0(.x, "_up"), 
              .cols = Chlorophyll:NO23_Qual) %>% 
  select(-Updown)
  
CTDdata_down <- CTDdata %>%
  filter(Updown == "Down") %>% 
  rename_with(~ paste0(.x, "_down"), 
              .cols = Chlorophyll:NO23_Qual) %>% 
  select(-Updown)

working_data <- full_join(CTDdata_up, CTDdata_down) %>%
  mutate(Chlorophyll_perc_diff = percent_diff(Chlorophyll_up, Chlorophyll_down),
         Density_perc_diff = percent_diff(Density_up, Density_down),
         DO_perc_diff = percent_diff(DO_up, DO_down),
         SigmaTheta_perc_diff = percent_diff(SigmaTheta_up, SigmaTheta_down),
         Light_Transmission_perc_diff = percent_diff(Light_Transmission_up, Light_Transmission_down),
         PAR_perc_diff = percent_diff(PAR_up, PAR_down),
         Surface_PAR_perc_diff = percent_diff(Surface_PAR_up, Surface_PAR_down),
         Salinity_perc_diff = percent_diff(Salinity_up, Salinity_down),
         Temperature_perc_diff = percent_diff(Temperature_up, Temperature_down),
         Turbidity_perc_diff = percent_diff(Turbidity_up, Turbidity_down),
         NO23_perc_diff = percent_diff(NO23_up, NO23_down)) %>%
  select(BinDepth, 
         Date, 
         contains("perc_diff"), 
         everything())

chl_max <- working_data %>% 
  group_by(Locator, Date) %>% 
  filter(Chlorophyll_Qual_down %in% c(NA, "TA")) %>% 
  summarize(Chl_max_depth = BinDepth[which.max(Chlorophyll_down)])
working_data <- left_join(working_data, chl_max)

# Updown_df  --------------------------------------------------------------

updown_df <- working_data %>%
  mutate(
    Chlorophyll_Qual_Auto = case_when(
      BinDepth > 50 & Chlorophyll_perc_diff > 50 ~ "q"),
    Density_Qual_Auto = case_when(
      BinDepth > 50 & Density_perc_diff > 10 ~ "q"),
    DO_Qual_Auto = case_when(
      BinDepth > 50 & DO_perc_diff > 10 ~ "q"),
    SigmaTheta_Qual_Auto = case_when(
      BinDepth > 50 & SigmaTheta_perc_diff > 10 ~ "q"),
    Light_Transmission_Qual_Auto = case_when(
      BinDepth > 50 & Light_Transmission_perc_diff > 10 ~ "q"),
    PAR_Qual_Auto = case_when(
      BinDepth > 50 & PAR_perc_diff > 10 ~ "q"),
    Surface_PAR_Qual_Auto = case_when(
      BinDepth > 50 & Surface_PAR_perc_diff > 10 ~ "q"),
    Salinity_Qual_Auto = case_when(
      BinDepth > 50 & Salinity_perc_diff > 10 ~ "q"),
    Temperature_Qual_Auto = case_when(
      BinDepth > 50 & Temperature_perc_diff > 10 ~ "q"),
    Turbidity_Qual_Auto = case_when(
      BinDepth > 50 & Turbidity_perc_diff > 10 ~ "q"),
    NO23_Qual_Auto = case_when(
      BinDepth > 50 & NO23_perc_diff > 10 ~ "q")) %>%
  mutate_if(is.character, ~replace_na(.,"")) %>% # Replaces NA values with a blank string ""
  mutate(flag_reason = "",
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



# Plots -------------------------------------------------------------------

(sal <- ggplot(updown_df)+
   geom_point(aes(x = Salinity_perc_diff,
                  y = BinDepth,
              color = Salinity_Qual_Auto))+
   scale_y_reverse())

(chl <- ggplot(updown_df)+
   geom_point(aes(x = Chlorophyll_perc_diff,
                  y = BinDepth,
              color = Chlorophyll_Qual_Auto))+
   scale_y_reverse())

(den <- ggplot(updown_df)+
   geom_point(aes(x = Density_perc_diff,
                  y = BinDepth,
              color = Temperature_Qual_Auto))+
   scale_y_reverse())

(DO <- ggplot(updown_df)+
   geom_point(aes(x = DO_perc_diff,
                  y = BinDepth,
              color = DO_Qual_Auto))+
   scale_y_reverse())

(lt <- ggplot(updown_df)+
   geom_point(aes(x = Light_Transmission_perc_diff,
                  y = BinDepth,
              color = Light_Transmission_Qual_Auto))+
   scale_y_reverse())

(temp <- ggplot(updown_df)+
   geom_point(aes(x = Temperature_perc_diff,
                  y = BinDepth,
              color = Temperature_Qual_Auto))+
   scale_y_reverse())

(PAR <- ggplot(updown_df)+
   geom_point(aes(x = PAR_perc_diff,
                  y = BinDepth,
              color = PAR_Qual_Auto))+
   scale_y_reverse())

(turb <- ggplot(updown_df)+
   geom_point(aes(x = Turbidity_perc_diff,
                  y = BinDepth,
              color = Turbidity_Qual_Auto))+
   scale_y_reverse())

(no23 <- ggplot(updown_df)+
   geom_point(aes(x = NO23_perc_diff,
                  y = BinDepth,
              color = NO23_Qual_Auto))+
   scale_y_reverse())

(sigmat <- ggplot(updown_df)+
   geom_point(aes(x = SigmaTheta_perc_diff,
                  y = BinDepth,
              color = SigmaTheta_Qual_Auto))+
   scale_y_reverse())

(spar <- ggplot(updown_df)+
   geom_point(aes(x = Surface_PAR_perc_diff,
                  y = BinDepth,
              color = Surface_PAR_Qual_Auto))+
   scale_y_reverse())


