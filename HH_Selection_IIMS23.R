#### --------------------------------------------------------
####
#### Script to filter eligible households by cluster, 
#### confirm cluster is fully listed, 
#### drop hh outside the boundaries, 
#### and select 20 households for interview for IIMS2023
####
#### --------------------------------------------------------

#---- Download HH Listing Data from Kobo with these settings: ----
# Export type = XLS
# Value and header format = XML
# Advanced options = uncheck "include data from all Y versions"
# Click Export
# Under Exports, a new row will appear. When the Download button for that row appears, you can click it
# Save the file as "mis23listing_hk.xlsx" (update your initials) in the same folder as this script.

#---- Check Kobo data to confirm team has used the correct EA number throughout the listing process ----

#---- Install Packages (run these commands only once, then you can put a # in front of them to turn them off) ----
# install.packages("tidyverse")
# install.packages("janitor")
#install.packages("sf") 
# install.packages("sp") 
# install.packages("rgdal")
# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("ggrepel")
# options(repos = c(ropensci = 'https://ropensci.r-universe.dev',
#                   CRAN = 'https://cloud.r-project.org'))
# install.packages('ruODK')

#---- Load libraries (each time R restarts) ----
library(tidyverse)
library(janitor)
library(readxl)
library(sf)
library(sp) #SpatialPointsDataFrame
library(openxlsx)
library(ggrepel)
library(ruODK)
# library(rgdal) #writeOGR


#---- Access the ODK CENTRAL and read in the household listing data ----

# FIRST, run the separate script that contains your credentials:
source("ODKC_HH_LISTING_CREDENTIALS.R")

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

# Unprotected password setup process looks like this, but don't do it this way!:
# ruODK::ru_setup(
#   svc = "https://link_to_OData_R_option_in_ODK_Central.svc",
#   un = "email@me.com",
#   pw = "password_goes_here",
#   tz = "Africa/Maputo",
#   verbose = TRUE
# )

# Some code that we don't need at the moment:
# File attachment download location -- this is supposed to download a file to the directory you specify, but it's not working. That's ok as we don't need it.
# loc <- fs::path("raw")

# See some of the submission meta data
# fq_svc <- ruODK::odata_service_get()
# fq_svc %>% knitr::kable(.)
# 
# # Get column names and labels and display them with knitr
# fq_form_schema <- ruODK::form_schema()
# fq_form_schema %>% knitr::kable(.)

# THIRD, read in all the hh listing data and remove group prefixes from variable names:

listed <- ruODK::odata_submission_get(wkt=TRUE) %>% 
  rename_with(.fn = ~ tolower(gsub("g1_id_", "", .x, fixed = TRUE)), .col = starts_with("g1_id_")) %>% 
  rename_with(.fn = ~ tolower(gsub("g2_id_", "", .x, fixed = TRUE)), .col = starts_with("g2_id_")) %>% 
  rename_with(.fn = ~ tolower(gsub("details_g_", "", .x, fixed = TRUE)), .col = starts_with("details_g_")) %>% 
  clean_names() %>% 
  filter(start_survey>"2023-11-5") # throw out any previous records from pre-testing phase

# Quick check to see how many of each EA we have in the dataset - helpful in case someone entered EA wrongly:


# Export listed file for summary table
write.csv(listed, "raw/listed.csv", row.names = FALSE)

#listed<-read.csv("raw/listed.csv")

#---- Set cluster number and team for which you are doing the selection ----
cluster <- "110623" # enter this as a string.
# For pratica cluster numbers go from 100501 to 100514

listed %>% dplyr::filter(area==cluster) %>% count(area)
#listed %>% dplyr::filter(province==cluster[2])%>% count(area)

#---- Filter to keep only records from the AE you want, and label the visit_result variable ----


filtered <- listed %>% 
  filter(area==as.numeric(cluster)) %>% # if team enters incorrect cluster numbers, add them to the filter i.e. | area==25 | area==24
  mutate(status=case_when(visit_result==1 ~ "Presente",
                          visit_result==2 ~ "Ausente - habitado",
                          visit_result==3 ~ "Ausente/não há pessoa competente",
                          visit_result==4 ~ "Todo agregado ausente por\num período prolongado de tempo",
                          visit_result==5 ~ "Desocupada/não é residência ",
                          visit_result==6 ~ "Destruída",
                          visit_result==7 ~ "Casa em construcao",
                          visit_result==8 ~ "Casa não encontrada",
                          TRUE ~ as.character(visit_result)))

#---- Load AE boundary map layer and filter to the relevant cluster ----

ae <- st_read("shp files/LIMITE_INTERNO_DE_AE.shp") %>% 
  filter(IMRS_ID == cluster)

#---- Plot the AEs just to see ----

ggplot() +
  geom_sf(data=ae, fill="lightblue", color="red") +
  theme_classic()

#---- Load Moz map base layer (TBD) and clip to EA boundaries ----

#---- Map GPS points of listed households against boundaries, marker colors show 'present' vs abandoned etc hh ----

# Visually check map output for completeness of listing households within the boundaries
# Keep in mind that GPS accuracy of e.g. 20m may place some locations outside the boundaries, even though they may really 
# be inside the boundaries 

ggplot() +
  geom_sf(data=ae, fill="lightblue", color="red") +
  geom_point(data=filtered, aes(x=gps_longitude, y=gps_latitude, color=as.factor(surveyor_id)), alpha=0.5) +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(color="Listado por:",
       title=paste(cluster, "-", length(unique(filtered$uuid_name)), "AF listados em total"))

#---- ASK: Does the EA appear to be fully listed? If not, check whether you have all the data from
#         that team, or if the team entered incorrect EA numbers during listing, etc.

#---- Save Map into folder ----
ggsave(paste0("Maps/", cluster,"_listing_map.png"), width = 10, height = 10, units = "in", dpi = 300)

#---- Drop abandoned/not eligible hh ----
filtered_clean <- filtered %>% 
  filter(visit_result %in% c(1,2)) # include absent but inhabited households as eligible for selection

#---- Create variable for listed households outside the boundaries and drop them (TBD) ----
point.sf <- st_as_sf(filtered_clean, coords = c("gps_longitude", "gps_latitude"), crs="EPSG:4326") # convert points to sf
poly <- st_transform(ae, 4326) # convert AE polygon to sf

good_points <- st_filter(point.sf, poly) %>% 
  mutate(inbounds="yes") # use st_filter to drop points that are outside the AE boundaries; create a variable to identify these 'good points'

withgood <- filtered_clean %>% # join the 'goodpoints' back into the main dataset of points, to get the new variable ("inbounds")
  full_join(good_points) %>% 
  mutate(inbounds=case_when(is.na(inbounds) ~ "fora do AE",
                            TRUE ~ "dentro do AE"),
         inbounds=fct_relevel(inbounds, "fora do AE")) # reorder levels so that 'outside' shows up in red

# Check average precision of GPS points within the cluster
# filtered_clean %>% 
#   summarize(mean=mean(gps_precision))

elig_for_interview <- withgood %>% 
  filter(inbounds=="dentro do AE")

# count the households that are outside the boundaries
oob <- withgood %>% 
  tally(inbounds=="fora do AE")

# plot the points marked inside and outside
ggplot() +
  geom_sf(data=ae, fill="lightblue", color="red") +
  geom_point(data=withgood, aes(x=gps_longitude, y=gps_latitude, color=inbounds), alpha=0.5) +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(color="",
       title=paste(cluster, "-",oob,"out of bounds"))

ggsave(paste0("Inbounds/", cluster,"_inbounds_map.png"), width = 10, height = 10, units = "in", dpi = 300)

#---- Randomly select 25 households from the clean list ----

set.seed(23) ## do not change this; it ensures the random selection is done the same every time, even by two different people
selected <- elig_for_interview %>% 
  slice_sample(n=25)

#---- Save these 25 households as a GPX file for sending via WhatsApp and opening in OSmand ----

coords <- selected %>% 
  select(hh_id_field, hhh, gps_latitude, gps_longitude) %>% 
  rename(ID=hh_id_field,
         NameHH = hhh,
         Latitude=gps_latitude,
         Longitude=gps_longitude)

my.sf.point <- st_as_sf(x = coords, 
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

st_write(my.sf.point, paste0("Selected/",cluster,"_pontos.gpx"), layer = "waypoints", driver = "GPX", dataset_options = "GPX_USE_EXTENSIONS=yes", append = FALSE)

#---- Map of 25 households from the clean list ----
ggplot() +
  geom_sf(data=ae, fill="lightblue", color="red") +
  geom_point(data=selected, aes(x=gps_longitude, y=gps_latitude, color=as.factor(surveyor_id)), alpha=1) +
  geom_text_repel(data=selected, aes(x=gps_longitude, y=gps_latitude, label=paste(hh_nb,"-",hhh)), size=2, max.overlaps=15) +
  theme_void() +
  theme(legend.position="bottom") +
  labs(color="Listado por:",
       title=paste("AE:",cluster, "-",length(unique(selected$uuid_name)), " AF seleccionadas"))

ggsave(paste0("Selected/", cluster,"_selected_map.png"), width = 10, height = 10, units = "in", dpi = 300)

#---- Drop unneeded variables, sort, and export to Excel to transfer back to Controlador ----
selected_pt <- selected %>% 
  select(area, hh_nb, hhh, nickname, directions, observations, males, females, surveyor_id) %>% 
  arrange(hh_nb) %>% 
  rename(AE=area,
         Num_AF=hh_nb,
         Num_Inq=surveyor_id,
         Nome_Chefe=hhh,
         Nome2_Chefe=nickname,
         Localização=directions,
         Observaçoes=observations,
         homems=males,
         mulheres=females) %>% 
  mutate(AE=cluster) ## change any incorrectly-entered AE numbers to the correct AE number

#---- Save the file to Excel ----

## Create a new workbook
wb <- createWorkbook()

## Add a worksheet
addWorksheet(wb, cluster)

## set column widths
setColWidths(wb, 1, cols = c(3, 4, 5, 6), widths = c(20, 20, 35, 35))

## write the data
writeData(wb, sheet = 1, x = selected_pt)

## Set the styles and textwrap
bodyStyle <- createStyle(fontSize=12, border="TopBottomLeftRight", borderColour = "gray" , wrapText=TRUE)
headerStyle <- createStyle(fontSize=14, border="TopBottomLeftRight", borderColour = "gray", textDecoration="bold")
highlightStyle <- createStyle(fgFill="lightgreen")
addStyle(wb, sheet = 1, bodyStyle, rows = 2:25, cols = 1:9, gridExpand = TRUE)
addStyle(wb, sheet = 1, highlightStyle, rows=1:25, cols=2, gridExpand = FALSE, stack = FALSE)
addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:9, gridExpand = TRUE)

## Save workbook in the "Selected" folder
saveWorkbook(wb, paste0("Selected/",cluster,"_selected.xlsx"), overwrite = TRUE) 

#---- Manual next steps: ----

# 1. Click on the .xlsx file within R File Window, and select "View File". It will open in Excel. (Or open in your Finder)
# 2. Select the cells with information, and copy
# 3. Paste into WhatsApp conversation with the relevant Controlador in the field
# 4. Excel file can also be sent to the Controlador in the field
# 5. Map of selected households with head of household name should also be copied/pasted into WhatsApp and sent to Controlador
# 6. GPX points file should be sent to Controlador via WhatsApp

