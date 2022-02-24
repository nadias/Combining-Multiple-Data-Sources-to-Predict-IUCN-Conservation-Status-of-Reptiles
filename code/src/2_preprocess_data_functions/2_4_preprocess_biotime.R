
biotime_df <- biotime_df %>%
  dplyr::select(GENUS_SPECIES, LONGITUDE, LATITUDE, DAY, MONTH, YEAR, sum.allrawdata.ABUNDANCE) %>%
  drop_na(GENUS_SPECIES, LONGITUDE, LATITUDE) %>%
  rename(taxon = GENUS_SPECIES,
         longitude = LONGITUDE,
         latitude = LATITUDE,
         value = sum.allrawdata.ABUNDANCE)

biotime_df$species <- biotime_df$taxon
biotime_df$taxon_rank <- "Species"
biotime_df$metric <- "abundance"
biotime_df$issue <- "Geodetic datum assumed WGS84"   # No information on the metadata about the datum
biotime_df$dataset <- "biotime"

biotime_df <- biotime_df %>% 
  dplyr::select(taxon, species, taxon_rank, longitude, latitude, metric, value, issue, dataset)
