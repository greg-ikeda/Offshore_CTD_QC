# Appends new CTD Data to the file in the CTD Data Repository
# Download Data from the intranet (http://dnrlab-web:82/mwdataset.asp)

rm(list = ls())
library(tidyverse)
library(lubridate)
library(kcmarine)
library(readxl)
library(here)
library(svDialogs)

# Load Data ---------------------------------------------------------------

run_request <- "y"

while(run_request == "y"){
  station <- dlgInput("Enter your station", "KSBP01")$res
  folder <- paste0("//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/", station, "/")
  raw_file <- list.files(folder, pattern = "_raw.csv")
  qcd_file <- list.files(folder, pattern = "_qcd.csv")
  
  # Load original CTD data from the CTD data repository
  # CTDdata_raw_file <- choose.files("Choose the raw file",
  #                                  default = paste0(folder, raw_file))
  # CTDdata_qcd_file <- choose.files("Choose the qcd file",
  #                                  default = paste0(folder, qcd_file))
  CTDdata_raw_file <- paste0(folder, raw_file)
  CTDdata_qcd_file <- paste0(folder, qcd_file)
  
  CTDdata_raw <- read_csv(CTDdata_raw_file) %>%
    mutate(date = date(mdy_hm(Sampledate)))
  CTDdata_qcd <- read_csv(CTDdata_qcd_file) %>%
    mutate(date = date(mdy_hm(Sampledate)))
  
  print(paste("Latest date in raw file:", max(CTDdata_raw$date)))
  print(paste("Latest date in qcd file:", max(CTDdata_qcd$date)))
  
  # Load new data downloaded from intranet
  new_CTD_data_file <- choose.files("Select new data to append to the files")
  new_CTD_data <- read_delim(new_CTD_data_file, 
                             delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>%
    mutate(date = mdy_hms(Sampledate))
  
  
  # Append date-appropriate and commented upcast data to CTD data -------------------------------------------------------------------------
  
  CTD_data_raw_full <- bind_rows(CTDdata_raw, new_CTD_data) %>%
    arrange(date) %>%
    mutate(Sampledate = format(parse_date_time(Sampledate, 
                                               orders = c('mdy_HMS', 'mdy_HM')),
                               "%m/%d/%Y %H:%M"))
  
  CTD_data_qcd_full <- bind_rows(CTDdata_qcd, new_CTD_data) %>%
    arrange(by = date) %>%
    mutate(Sampledate = format(parse_date_time(Sampledate, 
                                               orders = c('mdy_HMS', 'mdy_HM')),
                               "%m/%d/%Y %H:%M"))
  
  first_date_raw <- date(min(CTD_data_raw_full$date, na.rm = T))
  last_date_raw <- date(max(CTD_data_raw_full$date, na.rm = T))
  first_date_qcd <- date(min(CTD_data_qcd_full$date, na.rm = T))
  last_date_qcd <- date(max(CTD_data_qcd_full$date, na.rm = T))
  
  # Remove date column for compatibility
  CTD_data_raw_full <- CTD_data_raw_full %>% 
    select(-date)
  CTD_data_qcd_full <-  CTD_data_qcd_full %>%
    select(-date)
  
  
  # Write data to csv -------------------------------------------------------
  
  filename_raw <- paste0("ctd_extract_",station, "_", first_date_raw, "_", last_date_raw, "_raw.csv")
  
  appended_save_folder <- here("data", "updated_CTD_files")
  
  write_csv(CTD_data_raw_full, 
            here("data", "updated_CTD_files", filename_raw),
            na = "")
  write_csv(CTD_data_qcd_full, 
            here("data", "updated_CTD_files", qcd_file),
            na = "")
  
  shell.exec(appended_save_folder)
  
  
  # Move files into correct directory ---------------------------------------
  # Delete the previous auto-archived files (note: auto-archive is different from the "old files" folder)
  # Transfer the original files (CTDdata_raw_file and CTDdata_qcd_file) to the auto-archive
  # Move the newly created files to the CTD_data_repository/Station folder
  
  # IN PROGRESS!!!!
  run_request <- dlgInput("Run again? y/n", "y")$res
}


