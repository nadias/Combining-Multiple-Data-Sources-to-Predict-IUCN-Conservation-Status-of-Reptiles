log_debug("Remove marine species")

# Remove marine turtles
marine_turtles_names <- fread("data/raw/marine_turtles_list.csv", quote="", sep =",", header=FALSE)

marine_turtles_names$V1 <- tolower(marine_turtles_names$V1)

## Check if it the names are the same as in reptile db. If not, needs to be synonimyzed
marine_turtles_names %>% inner_join(reptile_db_df, by=c("V1" = "Species"))

occurrences_df <- occurrences_df %>%
  filter(! species %in% marine_turtles_names$V1)


# Remove sea snakes
sea_snakes_names <- fread("data/raw/sea_snakes_list.csv", quote="", sep =",", header=FALSE)

sea_snakes_names$V1 <- tolower(sea_snakes_names$V1)

## Check if it the names are the same as in reptile db. If not, needs to be synonimyzed
sea_snakes_names %>% inner_join(reptile_db_df, by=c("V1" = "Species"))


occurrences_df <- occurrences_df %>%
  filter(! species %in% sea_snakes_names$V1)



log_debug("Remove occurrences with weird latitudes")


occurrences_df <- occurrences_df %>%
  filter(! ((latitude > 70 | latitude < -70) &
            (species %in% c("Agkistrodon contortrix", "Anolis bicaorum", "Anolis sulcifrons",
                            "Aspidoscelis sexlineatus", "Carlia tutela", "Crotaphopeltis hotamboeia",
                            "Eryx somalicus", "Kentropyx pelviceps", "Morelia spilota", "Trimeresurus stejnegeri"))))


log_debug("Convert in presence/absence data")

# Convert in present / not present data
occurrences_df$present <- occurrences_df$value > 0

log_debug("Deduplicate records on the same pixel")

raster_img <- raster(paste0("data/raw/raster_layers/bioclimatic_variables/chelsa/CHELSA_bio10_01.tif"))

occurrences_df$rasterCol <- colFromX(raster_img, occurrences_df$longitude)
occurrences_df$rasterRow <- rowFromY(raster_img, occurrences_df$latitude)

occurrences_df %>% filter(is.na(rasterCol))
occurrences_df %>% filter(is.na(rasterRow))

occurrences_df <- occurrences_df %>% distinct(taxon, species, taxon_rank, year, present, rasterCol, rasterRow, .keep_all = TRUE)


saveRDS(occurrences_df, "data/processed/occurrences_df_preprocessed.rds")
