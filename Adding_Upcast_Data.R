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


# Load upcast data (downloaded separately)
CTD_upcast_file <- choose.files(default = "C:\\Users\\gikeda\\OneDrive - King County\\Downloads\\test", 
                                caption = "Select the downloaded upcast-only file")
CTD_upcast <- read_delim(CTD_upcast_file, 
                         delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(!Updown == "Down")
upcastdata_station <- unique(CTD_upcast$Locator)
upcastdata_station <- "test"
if(upcastdata_station != station){
  dlgInput("YOUR UPCAST DATA DOES NOT MATCH THE CURRENT STATION!", "Whoops, I'll recheck and rerun")$res
  stop()
}


# Identify Upcast preferred dates in original CTD data --------------------

# Identify the dates where the upcast replaces the downcast in the qcd and raw files
upcast_preferred_raw <- CTDdata_raw %>%
  filter(Updown == "Up")
upcast_preferred_qcd <- CTDdata_qcd %>%
  filter(Updown == "Up")

# Create a vector of the dates that have the upcast replace the downcast
upcast_dates_raw <- unique(upcast_preferred_raw$date)
upcast_dates_qcd <- unique(upcast_preferred_qcd$date)

# Write [USE_UPCAST] to the data where upcasts are preferred --------

# Isolate upcast + downcast data where the upcast replaced the downcast
# Write [USE_UPCAST] to the notes of this tibble
# Remove the dowcast+upcast from the working CTD dataframe
# Replace this removed data with the [USE_UPCAST] tagged data 

upcast_preferred_raw <- CTDdata_raw %>%
  filter(date %in% upcast_dates_raw) %>%
  mutate(`Cast Notes` = if_else(
    is.na(`Cast Notes`), 
    "[ADDED_UPCAST]",
    paste(`Cast Notes`, "[ADDED_UPCAST]", sep = ", "))
  )

upcast_preferred_qcd <- CTDdata_qcd %>%
  filter(date %in% upcast_dates_qcd) %>%
  mutate(`Cast Notes` = if_else(
    is.na(`Cast Notes`), 
    "[ADDED_UPCAST]",
    paste(`Cast Notes`, "[ADDED_UPCAST]", sep = ", "))
  )



# Remove existing upcast dates from the upcast-only data ------------------

upcast_append_raw <- CTD_upcast %>%
  mutate(date = date(mdy_hms(Sampledate))) %>%
  filter(!date %in% upcast_dates_raw)

upcast_append_qcd <- CTD_upcast %>%
  mutate(date = date(mdy_hms(Sampledate))) %>%
  filter(!date %in% upcast_dates_qcd)


# Write [ADDED_UPCAST] flag to cast comments field -------------------------

upcast_append_raw_noted <- upcast_append_raw %>%
  mutate(`Cast Notes` = if_else(
    is.na(`Cast Notes`), 
    "[ADDED_UPCAST]",
    paste(`Cast Notes`, "[ADDED_UPCAST]"))
  )

upcast_append_qcd_noted <- upcast_append_qcd %>%
  mutate(`Cast Notes` = if_else(
    is.na(`Cast Notes`), 
    "[ADDED_UPCAST]", 
    paste(`Cast Notes`, "[ADDED_UPCAST]"))
  )


# Append date-appropriate and commented upcast data to CTD data -------------------------------------------------------------------------

CTD_data_raw_full <- bind_rows(CTDdata_raw, upcast_append_raw_noted) %>%
  arrange(date) %>%
  mutate(Sampledate = format(parse_date_time(Sampledate, 
                                             orders = c('mdy_HMS', 'mdy_HM')),
                             "%m/%d/%Y %H:%M"))

CTD_data_qcd_full <- bind_rows(CTDdata_qcd, upcast_append_qcd_noted) %>%
  arrange(by = date) %>%
  mutate(Sampledate = format(parse_date_time(Sampledate, 
                                             orders = c('mdy_HMS', 'mdy_HM')),
                             "%m/%d/%Y %H:%M"))


# Create filenames --------------------------------------------------------

first_date_raw <- date(min(CTD_data_raw_full$date, na.rm = T))
last_date_raw <- date(max(CTD_data_raw_full$date, na.rm = T))
first_date_qcd <- date(min(CTD_data_qcd_full$date, na.rm = T))
last_date_qcd <- date(max(CTD_data_qcd_full$date, na.rm = T))

# Remove date column for compatibility. Added "_write" suffix to differentiate
CTD_data_raw_full_write <- CTD_data_raw_full %>% 
  select(-date)
CTD_data_qcd_full_write <-  CTD_data_qcd_full %>%
  select(-date)

filename_raw <- paste0("ctd_extract_",station, "_", first_date_raw, "_", last_date_raw, "_raw.csv")
filename_qcd <- qcd_file

# Write data to csv -------------------------------------------------------

write_csv(CTD_data_raw_full_write, 
          here("data", "upcast_added", filename_raw),
          na = "")
write_csv(CTD_data_qcd_full_write, 
          here("data", "upcast_added", filename_qcd),
          na = "")

# Test importing with import_CTD()

test_kcmarine_import <- import_CTD(here("data", "upcast_added", filename_qcd))


