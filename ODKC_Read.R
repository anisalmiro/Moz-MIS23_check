## Checking ODK CENTRAL MIS MOZAMBIQUE SURVEY QUESTIONNAIRE DATA

#---- Load libraries (each time R restarts) ----
library(tidyverse)
library(janitor)
library(readxl)
library(sf)
library(openxlsx)
library(ggrepel)
library(ruODK)
library(here)

#---- Access the ODK CENTRAL and read in the household listing data ----

# FIRST, run the separate script that contains your credentials:
source("ODKC_MOZ_MIS_MAIN_CREDENTIALS.R")

# ODK Central's OData URL contains base URL, project ID, and form ID
# ODK Central credentials can live in .Renviron
# See vignette("setup") for setup and authentication options.

# SECOND, run ruODK setup to load your credentials into ruODK:

ruODK::ru_setup(
  svc = Sys.getenv("ODKC_SVC"),
  un = Sys.getenv("ODKC_UN"),
  pw = Sys.getenv("ODKC_PW"),
  tz = "Africa/Maputo",
  verbose = TRUE
)

loc <- fs::path("raw")

# THIRD, list the repeat groups available in the submitted data:

fq_svc <- ruODK::odata_service_get()
fq_svc %>% knitr::kable(.)


# THIRD, read in all the submitted data, by each repeat group:

hhraw <- ruODK::odata_submission_get(
  table = fq_svc$name[1], 
  local_dir = loc,
  wkt=TRUE)

hhmraw <- ruODK::odata_submission_get(
  table = fq_svc$name[2], 
  local_dir = loc
)

netsraw <- ruODK::odata_submission_get(
  table = fq_svc$name[3], 
  local_dir = loc,
  wkt=TRUE)

feverraw <- ruODK::odata_submission_get(
  table = fq_svc$name[4], 
  local_dir = loc,
  wkt=TRUE)

womenraw <- ruODK::odata_submission_get(
  table = fq_svc$name[5], 
  local_dir = loc
)

birthsraw <- ruODK::odata_submission_get(
  table = fq_svc$name[6], 
  local_dir = loc,
  wkt=TRUE)

birthcheckraw <- ruODK::odata_submission_get(
  table = fq_svc$name[7], 
  local_dir = loc,
  wkt=TRUE)

biomarkersraw <- ruODK::odata_submission_get(
  table = fq_svc$name[8], 
  local_dir = loc,
  wkt=TRUE)

# We have six datasets (repeats):
  # Main household questionnaire
  # Household member repeat (age, travel)
  # Fever repeat
  # Net repeat
  # Women's questionnaire 
  # Women's births repeat (with child fever)
  # Women's birth check youngest repeat (not used for analysis)
  # Biomarker children data


## Remove columns that are empty:

hh <- hhraw %>% remove_empty(which="cols", quiet=FALSE) %>% 
  rename_with(.fn = ~ tolower(gsub("g1_id_", "", .x, fixed = TRUE)), .col = starts_with("g1_id_")) %>% 
  rename_with(.fn = ~ tolower(gsub("teaminfo_g_", "", .x, fixed = TRUE)), .col = starts_with("teaminfo_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("geoinfo_g_", "", .x, fixed = TRUE)), .col = starts_with("geoinfo_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("hhinfo_g_", "", .x, fixed = TRUE)), .col = starts_with("hhinfo_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("visitstatus_g_", "", .x, fixed = TRUE)), .col = starts_with("visitstatus_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("consent_g_", "", .x, fixed = TRUE)), .col = starts_with("consent_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("hh_members_g_", "", .x, fixed = TRUE)), .col = starts_with("hh_members_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("hh_quest_", "", .x, fixed = TRUE)), .col = starts_with("hh_quest_")) %>% 
  rename_with(.fn = ~ tolower(gsub("items_g_hh_", "", .x, fixed = TRUE)), .col = starts_with("items_g_hh_")) %>% 
  rename_with(.fn = ~ tolower(gsub("h2o_g_", "", .x, fixed = TRUE)), .col = starts_with("h2o_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("livestock_g_", "", .x, fixed = TRUE)), .col = starts_with("livestock_g_")) %>% 
  rename(indphone=items_g_ind_items_phone) %>% 
  rename_with(.fn = ~ tolower(gsub("items_g_ind_", "", .x, fixed = TRUE)), .col = starts_with("items_g_ind_")) %>% 
  select(-hh_g_count, -(contains("_cal")), -(contains("odata_navigation_link"))) %>% 
  rename_with(.fn = ~ tolower(gsub("h20source_g_", "", .x, fixed = TRUE)), .col = starts_with("h20source_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("h20location_g_", "", .x, fixed = TRUE)), .col = starts_with("h20location_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("toilettype_g_", "", .x, fixed = TRUE)), .col = starts_with("toilettype_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("toiletshare_g_", "", .x, fixed = TRUE)), .col = starts_with("toiletshare_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("agriland_g_", "", .x, fixed = TRUE)), .col = starts_with("agriland_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("items_g_", "", .x, fixed = TRUE)), .col = starts_with("items_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("spray_g_", "", .x, fixed = TRUE)), .col = starts_with("spray_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("nets_g_mosqnet_g_", "", .x, fixed = TRUE)), .col = starts_with("nets_g_mosqnet_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("nets_g_", "", .x, fixed = TRUE)), .col = starts_with("nets_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("pastnet_g_", "", .x, fixed = TRUE)), .col = starts_with("pastnet_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("materials_g_", "", .x, fixed = TRUE)), .col = starts_with("materials_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("women_g_", "", .x, fixed = TRUE)), .col = starts_with("women_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("conclusion_g_", "", .x, fixed = TRUE)), .col = starts_with("conclusion_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("biomarkers_g_", "", .x, fixed = TRUE)), .col = starts_with("biomarkers_g_")) %>% 
rename_with(.fn = ~ tolower(gsub("cookfuel_g_", "", .x, fixed = TRUE)), .col = starts_with("cookfuel_g_"))

  
hhm <- hhmraw %>% remove_empty(which="cols", quiet=FALSE) %>% 
  rename_with(.fn = ~ tolower(gsub("travel_", "", .x, fixed = TRUE)), .col = starts_with("travel_"))
  
nets <- netsraw %>% remove_empty(which="cols", quiet=FALSE) %>% 
  rename_with(.fn = ~ tolower(gsub("statecolorshape_g_", "", .x, fixed = TRUE)), .col = starts_with("statecolorshape_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("brand_g_", "", .x, fixed = TRUE)), .col = starts_with("brand_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("campaign_g_", "", .x, fixed = TRUE)), .col = starts_with("campaign_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("net_users_g_", "", .x, fixed = TRUE)), .col = starts_with("net_users_g_")) 
  
fever <- feverraw %>% remove_empty(which="cols", quiet=FALSE)

women <- womenraw %>% remove_empty(which="cols", quiet=FALSE) %>% 
  rename_with(.fn = ~ tolower(gsub("w_intro_g_w_basic_info_g_", "", .x, fixed = TRUE)), .col = starts_with("w_intro_g_w_basic_info_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("w_intro_g_visitconsent_g_", "", .x, fixed = TRUE)), .col = starts_with("w_intro_g_visitconsent_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("w_intro_g_age_w_g_", "", .x, fixed = TRUE)), .col = starts_with("w_intro_g_age_w_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("w_intro_g_schoollvl_g_", "", .x, fixed = TRUE)), .col = starts_with("w_intro_g_schoollvl_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("w_births_g_kidshh_g_", "", .x, fixed = TRUE)), .col = starts_with("w_births_g_kidshh_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("w_births_g_kidsouthh_g_", "", .x, fixed = TRUE)), .col = starts_with("w_births_g_kidsouthh_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("w_births_g_kidsdead_g_", "", .x, fixed = TRUE)), .col = starts_with("w_births_g_kidsdead_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("w_births_g_", "", .x, fixed = TRUE)), .col = starts_with("w_births_g_")) %>% 
  select(-(contains("_cal"))) %>% 
  rename_with(.fn = ~ tolower(gsub("pregnant_g_", "", .x, fixed = TRUE)), .col = starts_with("pregnant_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("preg_g_", "", .x, fixed = TRUE)), .col = starts_with("preg_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("malaria_kno_g_", "", .x, fixed = TRUE)), .col = starts_with("malaria_kno_g_")) %>% 
  # rename_with(.fn = ~ tolower(gsub("mal_affirm_", "", .x, fixed = TRUE)), .col = starts_with("mal_affirm_")) %>% 
  rename_with(.fn = ~ tolower(gsub("lang_g_", "", .x, fixed = TRUE)), .col = starts_with("lang_g_")) 
  
births <- birthsraw %>% remove_empty(which="cols", quiet=FALSE) %>% 
  rename(u5id=under5_id) %>% 
  rename_with(.fn = ~ tolower(gsub("under5_", "", .x, fixed = TRUE)), .col = starts_with("under5_")) %>% 
  rename_with(.fn = ~ tolower(gsub("sexage_", "", .x, fixed = TRUE)), .col = starts_with("sexage_")) %>% 
  rename_with(.fn = ~ tolower(gsub("fever_under5_", "", .x, fixed = TRUE)), .col = starts_with("fever_under5_"))
  
birthcheck <- birthcheckraw %>% remove_empty(which="cols", quiet=FALSE)
biomarkers <- biomarkersraw %>% remove_empty(which="cols", quiet=FALSE) %>% 
  rename_with(.fn = ~ tolower(gsub("biom_basicinfo_", "", .x, fixed = TRUE)), .col = starts_with("biom_basicinfo_")) %>% 
  rename_with(.fn = ~ tolower(gsub("age_biom_g_", "", .x, fixed = TRUE)), .col = starts_with("age_biom_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("symptoms_g_", "", .x, fixed = TRUE)), .col = starts_with("symptoms_g_"))
  

# Make a list with the df names:
dfs <- list(hh = hh, hhm = hhm, nets = nets, women = women, births = births, birthcheck = birthcheck, fever = fever, biomarkers = biomarkers)

# Make all of them dataframes instead of tibbles:
dataframe_list <- lapply(dfs, as.data.frame)

# Export csv copies of the raw data
walk2(dataframe_list, paste0("raw/", names(dataframe_list), ".csv"), write_csv)



