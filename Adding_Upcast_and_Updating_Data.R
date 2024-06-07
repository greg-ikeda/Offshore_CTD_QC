# Requires the following:
# 1. VPN access, to access data in "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/"
# 2. Upcast data, downloaded from the CTD intranet (http://dnrlab-web:82/mwdataset.asp)

# This is made to append upcast data to the data in the CTD data repository
# 1. Identify currently existing upcast data in the CTD file for a given station (e.g. when the downcast data was rejected)
# 2. Remove those casts from a separate file containing only upcast data
# 3. Write the flag "[USE_UPCAST]" to the CastComments field in the upcast data file
# 4. Append the upcast data to the CTD qcd_and raw file

rm(list = ls())
library(tidyverse)
library(lubridate)
library(kcmarine)
library(readxl)
library(here)
library(svDialogs)
library(plotly)

# Load Data ---------------------------------------------------------------

# Load original CTD data from the CTD data repository
station <- dlgInput("Enter your station", "KSBP01")$res
folder <- paste0("//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/", station, "/")
raw_file <- paste0(folder, list.files(folder, pattern = "_raw.csv"))
qcd_file <- paste0(folder, list.files(folder, pattern = "_qcd.csv"))

CTDdata_raw <- read_csv(raw_file) %>%
  mutate(date = date(mdy_hm(Sampledate)))
CTDdata_qcd <- read_csv(qcd_file) %>%
  mutate(date = date(mdy_hm(Sampledate)))

# Load new dowcast + upcast data, to be added to the existing data (downloaded separately)
CTD_newdata_file <- choose.files(default = "C:\\Users\\gikeda\\OneDrive - King County\\Downloads\\test", 
                                 caption = "Select the downloaded upcast-only file")
CTD_newdata <- read_delim(CTD_newdata_file, 
                          delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date = date(mdy_hms(Sampledate)))

# Check to make sure that you loaded the correct station data. 
# Script will stop if the station in the new data file does not match the current station
upcastdata_station <- unique(CTD_newdata$Locator)
if(upcastdata_station != station){
  dlgInput("YOUR UPCAST DATA DOES NOT MATCH THE CURRENT STATION!", "Whoops, I'll recheck and rerun")$res
  stop()
}


# Identify the latest downcast data already in the files ----------------

CTDdata_raw_downcast <- max(CTDdata_raw$date)
CTDdata_qcd_downcast <- max(CTDdata_qcd$date)

# Prep the new data to remove any downcast data already in the df 
CTD_newdata_downcast <- CTD_newdata %>%
  filter(Updown == "Down",
         date > ymd(CTDdata_raw_downcast))

#Append new downcast data to the originals
CTDdata_raw_downcast_updated <- bind_rows(CTDdata_raw, CTD_newdata_downcast) %>%
  arrange(by = Updown, date) %>%
  filter(Updown == "Down")
CTDdata_qcd_downcast_updated <- bind_rows(CTDdata_qcd, CTD_newdata_downcast) %>%
  arrange(by = Updown, date) %>%
  filter(Updown == "Down")

# Append [ADDED_UPCAST] to the new upcast data
CTD_newdata_upcast <- CTD_newdata %>%
  filter(Updown == "Up") %>%
  mutate(`Cast Notes` = if_else(
    is.na(`Cast Notes`), 
    "[ADDED_UPCAST]",
    paste(`Cast Notes`, "[ADDED_UPCAST]", sep = ", "))
  )

# Identify Upcast preferred dates in original CTD data --------------------

# Identify the dates where the upcast replaces the downcast in the qcd and raw files
upcast_preferred_raw <- CTDdata_raw %>%
  filter(Updown == "Up")
upcast_preferred_qcd <- CTDdata_qcd %>%
  filter(Updown == "Up")

# Create a vector of the dates that have the upcast replace the downcast
upcast_preferreddates_raw <- unique(upcast_preferred_raw$date)
upcast_preferreddates_qcd <- unique(upcast_preferred_qcd$date)

# Write [USE_UPCAST] to the data where upcasts are preferred, in the original data --------

CTDdata_upcast_preferred_raw <- CTDdata_raw %>%
  filter(Updown == "Up") %>%
  mutate(`Cast Notes` = if_else(
    is.na(`Cast Notes`), 
    "[USE_UPCAST]",
    paste(`Cast Notes`, "[USE_UPCAST]", sep = ", "))
  )

CTDdata_upcast_preferred_qcd <- CTDdata_qcd %>%
  filter(Updown == "Up") %>%
  mutate(`Cast Notes` = if_else(
    is.na(`Cast Notes`), 
    "[USE_UPCAST]",
    paste(`Cast Notes`, "[USE_UPCAST]", sep = ", "))
  )

# Remove existing upcast dates from the new data ------------------

CTD_newdata_upcast_append_raw <- CTD_newdata_upcast %>%
  filter(!date %in% upcast_preferreddates_raw) %>%
  bind_rows(CTDdata_upcast_preferred_raw)

CTD_newdata_upcast_append_qcd <- CTD_newdata_upcast %>%
  filter(!date %in% upcast_preferreddates_qcd) %>%
  bind_rows(CTDdata_upcast_preferred_qcd)

unique(CTD_newdata_upcast_append_qcd$`Cast Notes`)

# Combine all data --------------------------------------------------------

CTD_newdata_full_raw <- bind_rows(CTDdata_raw_downcast_updated, CTD_newdata_upcast_append_raw) %>%
  arrange(by = Updown, date) %>%
  mutate(Sampledate = format(parse_date_time(Sampledate, 
                                             orders = c('mdy_HMS', 'mdy_HM')),
                             "%m/%d/%Y %H:%M"))

CTD_newdata_full_qcd <- bind_rows(CTDdata_qcd_downcast_updated, CTD_newdata_upcast_append_qcd) %>%
  arrange(by = Updown, date) %>%
  mutate(Sampledate = format(parse_date_time(Sampledate, 
                                             orders = c('mdy_HMS', 'mdy_HM')),
                             "%m/%d/%Y %H:%M"))


# # Append date-appropriate and commented upcast data to CTD data -------------------------------------------------------------------------
# 
# CTD_data_raw_full <- bind_rows(CTDdata_raw, upcast_append_raw_noted) %>%
#   arrange(date) %>%
#   mutate(Sampledate = format(parse_date_time(Sampledate, 
#                                              orders = c('mdy_HMS', 'mdy_HM')),
#                              "%m/%d/%Y %H:%M"))
# 
# CTD_data_qcd_full <- bind_rows(CTDdata_qcd, upcast_append_qcd_noted) %>%
#   arrange(by = date) %>%
#   mutate(Sampledate = format(parse_date_time(Sampledate, 
#                                              orders = c('mdy_HMS', 'mdy_HM')),
#                              "%m/%d/%Y %H:%M"))


# Create filenames --------------------------------------------------------

first_date_raw <- date(min(CTD_newdata_full_raw$date, na.rm = T))
last_date_raw <- date(max(CTD_newdata_full_raw$date, na.rm = T))
first_date_qcd <- date(min(CTD_newdata_full_qcd$date, na.rm = T))
last_date_qcd <- date(max(CTD_newdata_full_qcd$date, na.rm = T))

# Remove date column for compatibility. Added "_write" suffix to differentiate
CTD_newdata_full_raw_write <- CTD_newdata_full_raw %>% 
  select(-date)
CTD_newdata_full_qcd_write <-  CTD_newdata_full_qcd %>%
  select(-date)


# Write data to csv -------------------------------------------------------

# Create filenames for the resulting csv files
filename_raw <- paste0("ctd_extract_",station, "_", first_date_raw, "_", last_date_raw, "_raw.csv")
filename_qcd <- gsub(paste0(".*",station,"/"), "", qcd_file)

write_csv(CTD_newdata_full_raw_write, 
          here("data", "upcast_added", filename_raw),
          na = "")
write_csv(CTD_newdata_full_qcd_write, 
          here("data", "upcast_added", filename_qcd),
          na = "")

sink(paste0(station, "_castnotes.txt"))
print(unique(CTD_newdata_full_qcd_write$`Cast Notes`))
sink()

shell.exec(here("data", "upcast_added"))
shell.exec(here(paste0(station, "_castnotes.txt")))
