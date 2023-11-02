
#---- Load libraries (each time R restarts) ----
library(tidyverse)
library(janitor)
library(readxl)
library(sf)
library(sp) #SpatialPointsDataFrame
library(openxlsx)
library(ggrepel)

# Merge polygons by IMRS_ID
ae <- ae %>% 
  group_by(IMRS_ID) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Select the cluster that you want to draw
cluster <- "100501"
# Create in all the AEs and filter by cluster number
ae <- st_read("shp files/LIMITE_INTERNO_DE_AE_475AE.shp") %>%
  filter(IMRS_ID == cluster)
# Convert polygon to linestring as GPX doesn't support polygones
ae <- st_cast(ae, "LINESTRING")
# Write as GPX file with cluster number as prefix
st_write(ae, paste0("AE GPX files/", cluster,"_area.gpx"), layer = "track_points", driver = "GPX", dataset_options = "GPX_USE_EXTENSIONS=yes", append = FALSE)

# Add duplicate tag
ae <- ae %>% 
  mutate(dup = duplicated(IMRS_ID) | duplicated(IMRS_ID, fromLast = TRUE))

# Create data frame with duplicates (if there are any)
ae_dup <- ae %>% filter(dup == "TRUE")

# How many unique IMRS_ID
length(unique(ae$IMRS_ID))




