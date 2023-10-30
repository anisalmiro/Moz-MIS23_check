
# Select the cluster that you want to draw
cluster <- "100501"
# Create in all the AEs and filter by cluster number
ae <- st_read("shp files/LIMITE_INTERNO_DE_AE.shp") %>%
  filter(IMRS_ID == cluster)
# Convert polygon to linestring as GPX doesn't support polygones
ae <- st_cast(ae, "LINESTRING")
# Write as GPX file with cluster number as prefix
st_write(ae, paste0("AE GPX files/", cluster,"_area.gpx"), layer = "track_points", driver = "GPX", dataset_options = "GPX_USE_EXTENSIONS=yes", append = FALSE)

