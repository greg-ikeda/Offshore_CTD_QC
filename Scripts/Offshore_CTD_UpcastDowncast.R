# This test compares the upcast and the downcast and flags data that are significantly different.

# source(here("Offshore_CTDQC_DataPrep.R"))

QC_test <- "up_down_comparison"
test_save_dir <- paste0(save_folder, "/", QC_test)

# Prep Data ---------------------------------------------------------------

CTDdata_up <- working_data %>%
  filter(Updown == "Up") %>% 
  rename_with(~ paste0(.x, "_up"), 
              .cols = Chlorophyll:NO23_Qual) %>% 
  select(-Updown)

CTDdata_down <- working_data %>%
  filter(Updown == "Down") %>% 
  rename_with(~ paste0(.x, "_down"), 
              .cols = Chlorophyll:NO23_Qual) %>% 
  select(-Updown)

updown_working_data <- left_join(CTDdata_up, CTDdata_down,
                                 by = c("Locator", "Date", "Depth")) %>%
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
  filter(BinDepth > 3) %>% # Removing large variation at the surface 3m 
  select(Locator,
         BinDepth, 
         Date, 
         contains("perc_diff"), 
         everything())

# Identify the depth of the chl max, pycnocline, thermocline, etc. 
# Create a logical column if it is above or below
# Use that instead of Depth_up > 50

chl_max <- tibble()
for(profile in profile_dates){
  profile_date <- ymd(profile)
  tempdata <- updown_working_data %>%
    filter(Date == profile_date,
           Chlorophyll_Qual_down %in% c(NA, "TA")) %>%
    group_by(Date) %>%
    summarize(chl_max_depth = BinDepth[which.max(Chlorophyll_down)])
  chl_max <- bind_rows(chl_max, tempdata)
}

chl_max_working_data <- left_join(updown_working_data, chl_max) %>%
  select(Locator, Date, chl_max_depth, everything())

# Updown_df  --------------------------------------------------------------

updown_df <- chl_max_working_data %>%
  mutate(
    Chlorophyll_Qual_Auto = case_when(
      BinDepth > 15 & Chlorophyll_perc_diff > 50 ~ "q"),
    Density_Qual_Auto = case_when(
      BinDepth > 15 & Density_perc_diff > 0.2 ~ "q"),
    DO_Qual_Auto = case_when(
      BinDepth > 15 & DO_perc_diff > 10 ~ "q"),
    SigmaTheta_Qual_Auto = case_when(
      BinDepth > 15 & SigmaTheta_perc_diff > 10 ~ "q"),
    Light_Transmission_Qual_Auto = case_when(
      BinDepth > 15 & Light_Transmission_perc_diff > 10 ~ "q"),
    PAR_Qual_Auto = case_when(
      BinDepth > 15 & PAR_perc_diff > 100 ~ "q"),
    Surface_PAR_Qual_Auto = case_when(
      BinDepth > 15 & Surface_PAR_perc_diff > 30 ~ "q"),
    Salinity_Qual_Auto = case_when(
      BinDepth > 15 & Salinity_perc_diff > 10 ~ "q"),
    Temperature_Qual_Auto = case_when(
      BinDepth > 15 & Temperature_perc_diff > 10 ~ "q"),
    Turbidity_Qual_Auto = case_when(
      BinDepth > 15 & Turbidity_perc_diff > 10 ~ "q"),
    NO23_Qual_Auto = case_when(
      BinDepth > 15 & NO23_perc_diff > 15 ~ "q")) %>%
  mutate(flag_reason = "",
         flag_reason = if_else(!is.na(Chlorophyll_Qual_Auto), paste0(flag_reason, "Chl_"), flag_reason),
         flag_reason = if_else(!is.na(Density_Qual_Auto), paste0(flag_reason, "Density_"), flag_reason),
         flag_reason = if_else(!is.na(DO_Qual_Auto), paste0(flag_reason, "DO_"), flag_reason),
         flag_reason = if_else(!is.na(SigmaTheta_Qual_Auto), paste0(flag_reason, "SigmaT_"), flag_reason),
         flag_reason = if_else(!is.na(Light_Transmission_Qual_Auto), paste0(flag_reason, "Light_"), flag_reason),
         flag_reason = if_else(!is.na(PAR_Qual_Auto), paste0(flag_reason, "PAR_"), flag_reason),
         flag_reason = if_else(!is.na(Surface_PAR_Qual_Auto), paste0(flag_reason, "SPAR_"), flag_reason),
         flag_reason = if_else(!is.na(Salinity_Qual_Auto), paste0(flag_reason, "Sal_"), flag_reason),
         flag_reason = if_else(!is.na(Temperature_Qual_Auto), paste0(flag_reason, "Temp_"), flag_reason),
         flag_reason = if_else(!is.na(Turbidity_Qual_Auto), paste0(flag_reason, "Turb_"), flag_reason),
         flag_reason = if_else(!is.na(NO23_Qual_Auto), paste0(flag_reason, "NO23_"), flag_reason)) %>%
  mutate_if(is.character, ~replace_na(.,"")) %>%
  mutate(flag_reason = str_sub(flag_reason, end = -2))


# Flagged Data plots ------------------------------------------------------

updown_flagged <- updown_df %>%
  filter(flag_reason != "")

fullvector <- c()
flagged_data <- updown_flagged$flag_reason

count <- 0
for(item in flagged_data){
  count <- count + 1
  flag_vector <- str_split_1(item, "_")
  fullvector <- c(fullvector, flag_vector)
}
vector_df <- tibble(fullvector) %>%
  filter(fullvector != "SPAR") # Removing SPAR as this test does not apply

updown_rej_plot <- ggplot(vector_df) +
  geom_bar(aes(x = fullvector)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Samples Flagged by Upcast/Downcast Test")
