# Plots parameters for a given station and date
# Open the resulting .html files for the paraameter in question in your browser
# Hover over any errant data points to identify the depth, value, and cast direction
# Find that value in the qcd data file and flag accordingly

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
qcd_file <- paste0(folder, list.files(folder, pattern = "_qcd.csv"))
save_folder <- paste0(here(),"/QC")

CTD_data_qcd <- read_csv(qcd_file) %>%
  mutate(date = date(mdy_hm(Sampledate))) %>%
  arrange(by = "Sampledate")


# Sorting data ------------------------------------------------------------

repeat_status <- "y"

while(repeat_status %in% c("y", "yes", "Y", "YES")){
  columns <- colnames(CTD_data_qcd)
  plot_params <- which(grepl(pattern = 'Locator|Qual|date|down|Notes|Depth', colnames(CTD_data_qcd)), arr.ind = TRUE)
  plot_params <- columns[-plot_params]
  
  QC_date <- dlgInput("Which date to QC? yyyy-mm-dd", "2023-01-01")$res
  
  df <- CTD_data_qcd %>%
    filter(date == ymd(QC_date))
  
  while(nrow(df) == 0){
    print("No data for this date. Try again")
    QC_date <- dlgInput("Which date to QC? ", "2023-01-01")$res
    df <- CTD_data_qcd %>%
      filter(date == ymd(QC_date))
  }
  
  dir.create(here("QC", paste0(station, "_", QC_date)))
  save_folder <- here("QC", paste0(station, "_", QC_date))
  shell.exec(save_folder)
  
  # Plotting data ------------------------------------------------------------
  
  for(param in plot_params){
    param <- as.name(param)
    
    t <- print(ggplot(df)+
                 geom_path(aes_string(y = 'Depth',
                                      x =  param),
                           alpha = 0.2)+
                 geom_point(aes_string(y = 'Depth',
                                       x =  param,
                                       color = "Updown"))+
                 scale_color_manual(values = c("darkblue", "purple"))+
                 scale_y_reverse()+
                 labs(title = paste0(param, "   ", QC_date)))
    # ggsave(path = save_folder,
    #        filename = paste0(gsub(",.*","",param), ".png"))
    htmlwidgets::saveWidget(ggplotly(t), 
                            file = paste0(save_folder,"/",gsub(",.*","",param), ".html"),
                            title = paste0(param))
    ggplotly(t)
  }
  repeat_status <- dlgInput("QC another date? y/n", "y")$res
} 

