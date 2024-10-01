# Setup TO BE REMOVED/MODIFIED LATER. WILL BE REPLACED BY DATA PREP SCRIPT -------------------------------------------------------------------

## NOTE 1: This is currently only running on a subset of data, as the QCd data files in the CTD data repository on the Z:/ drive only contain downcast data
#          Therefore the Data prep script isn't loading the necessary upcast+downcast data for this test, as it is pulling directly from the CTD data repository 
#          The "Setup" section here is unique to this test script FOR DEVELOPMENT PURPOSES ONLY. Other tests should utilize the data prep script for consistency.  

## NOTE 2: Some stations have upcast data from when the downcast data was rejected for various reasons
#### the script upcast_data_check.R will write a new csv file that does the following (CURRENTLY ONLY USED ON KSBP01! Might need modifications ¯\_(ツ)_/¯)
####          1. Pulls upcast data from the QC'd data file to retain Cast Comments (aka creates a df with just the casts where the upcast is the good cast) 
####          2. Removes these profile dates (downcast and upcast) from a data file that contains both upcast and downcast for the entire station's data record (e.g. downloaded from the intanet)
####          3. Writes the good upcast data to the df that contains the upcast+downcast 
####          4. Reformats stuff to be used with import_CTD


# What station do you want figures from?
station <- dlgInput("Enter your station", "KSBP01")$res
date_begin <- dlgInput("Enter the date of the first profile to QC (YYYY-MM-DD)", "2023-01-01")$res
date_end <- dlgInput("Enter the date of the last profile to QC (YYYY-MM-DD)", Sys.Date())$res

# # Where do you want to save output?
save_folder <- here("output")

folder <- paste0("//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/", station, "/")
fname <- list.files(folder, pattern = "_updown.csv")

# Load data and calculate upcast/downcast  --------------------------

bin_width <- 0.5

CTDdata <- import_CTD(paste0(folder, fname)) %>% 
  mutate(Year = year(Sampledate), 
         Month = month(Sampledate), 
         Day = day(Sampledate), 
         YearDay = yday(Sampledate),
         Date = as.Date(Sampledate), 
         depth_bin(Depth, bin_width)) %>%
  filter(Sampledate > ymd(date_begin),
         Sampledate < ymd(date_end))
tz(CTDdata$Sampledate) <- "America/Los_Angeles"

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


working_data <- left_join(CTDdata_up, CTDdata_down,
                    by = c("Date", "Depth")) %>%
  select(Sampledate.x, Sampledate.y, everything()) %>%
  mutate(Chlorophyll_perc_diff = calc_updown_percentdiff(Chlorophyll_up, Chlorophyll_down),
         Density_perc_diff = calc_updown_percentdiff(Density_up, Density_down),
         DO_perc_diff = calc_updown_percentdiff(DO_up, DO_down),
         SigmaTheta_perc_diff = calc_updown_percentdiff(SigmaTheta_up, SigmaTheta_down),
         Light_Transmission_perc_diff = calc_updown_percentdiff(Light_Transmission_up, Light_Transmission_down),
         PAR_perc_diff = calc_updown_percentdiff(PAR_up, PAR_down),
         Surface_PAR_perc_diff = calc_updown_percentdiff(Surface_PAR_up, Surface_PAR_down),
         Salinity_perc_diff = calc_updown_percentdiff(Salinity_up, Salinity_down),
         Temperature_perc_diff = calc_updown_percentdiff(Temperature_up, Temperature_down),
         Turbidity_perc_diff = calc_updown_percentdiff(Turbidity_up, Turbidity_down),
         NO23_perc_diff = calc_updown_percentdiff(NO23_up, NO23_down),
         BinDepth = BinDepth.x) %>%
  select(BinDepth, 
         Date, 
         contains("perc_diff"), 
         everything())


# Identify the depth of the chl max, pycnocline, thermocline, etc. 
# Create a logical column if it is above or below
# Use that instead of Depth_up > 50

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


