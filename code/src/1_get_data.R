############
### REPTILE-DB

# From: http://www.reptile-database.org/data/reptile_checklist_2020_12.xlsx

log_info('Loading Reptile DB data')

reptile_db_df <- read_excel("data/raw/reptile-db/reptile_checklist_2020_12.xlsx")



############
### GBIF

# From: https://www.gbif.org/occurrence/download/0090957-200613084148143

log_info('Loading GBIF data')

gbif_df <- fread("data/raw/occurrences/gbif/0090957-200613084148143.csv", quote="")



############
### PREDICTS

# From: https://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/78dac1a9-6aa0-4dcb-9750-50df04f8ca6e/package?destination=https%3A%2F%2Fdata.nhm.ac.uk%2Fdataset%2Fthe-2016-release-of-the-predicts-database&resource_url=https%3A%2F%2Fdata.nhm.ac.uk%2Fdataset%2F902f084d-ce3f-429f-a6a5-23162c73fdf7%2Fresource%2F78dac1a9-6aa0-4dcb-9750-50df04f8ca6e%2Fdownload%2Fdatabase.zip&package_type=dataset&id=the-2016-release-of-the-predicts-database

log_info('Loading Predicts data')

predicts_df <- fread("data/raw/occurrences/predicts/PREDICTS_database.csv")



############
### BIOTIME

# From: http://biotime.st-andrews.ac.uk/downloadFull.php

log_info('Loading BioTIME data')

biotime_df <- fread("data/raw/occurrences/biotime/BioTIMEQuery02_04_2018.csv")



############
### LIVING PLANET INDEX

# From: https://livingplanetindex.org/

log_info('Loading Living Planet Index data')

living_Crocodylia <- read.csv("data/raw/occurrences/living_planet_index/population_record_for_multiple_1604099876222088.8_Crocodylia.csv", header=TRUE, quote = "\"")
living_Rhynchocephalia <- read.csv("data/raw/occurrences/living_planet_index/population_record_for_multiple_1604099949802744.5_Rhynchocephalia.csv", header=TRUE, quote = "\"")
living_Squamata <- read.csv("data/raw/occurrences/living_planet_index/population_record_for_multiple_1604099994422842.2_Squamata.csv", header=TRUE, quote = "\"")
living_Testudines <- read.csv("data/raw/occurrences/living_planet_index/population_record_for_multiple_1604100062220978.2_Testudines.csv", header=TRUE, quote = "\"")

log_debug('Merging Living Planet Index data')

lpi_df <- bind_rows(living_Crocodylia, living_Rhynchocephalia, living_Squamata, living_Testudines)

log_debug('Removing Living Planet Index intermediate datasets')

rm(living_Crocodylia, living_Rhynchocephalia, living_Squamata, living_Testudines)



############
#  Range maps data

# From: https://datadryad.org/stash/dataset/doi:10.5061/dryad.83s7k

log_info('Loading Range Maps data')

range_maps <- readOGR("data/raw/range_maps/GARD1.1_dissolved_ranges", "modeled_reptiles")

#range_maps_iucn <- readOGR("data/raw/range_maps/GARD1.1_dissolved_ranges", "modeled_reptiles")
