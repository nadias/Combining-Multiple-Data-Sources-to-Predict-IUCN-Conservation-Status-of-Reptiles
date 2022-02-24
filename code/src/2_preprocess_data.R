
log_debug("Getting synonyms data")
#source("src/2_preprocess_data_functions/2_0_get_synonyms.R")
synonyms_df <- readRDS("data/processed/old_bkup/synonyms_df.rds")
log_debug("Storing synonyms")
saveRDS(synonyms_df, "data/processed/synonyms_df.rds")


log_debug("Preprocessing Reptile DB data")
source("src/2_preprocess_data_functions/2_1_preprocess_reptile_db.R")
log_debug("Storing Reptile DB data")
saveRDS(reptile_db_df, "data/processed/reptile_db_df.rds")

log_debug("Preprocessing GBIF data")
source("src/2_preprocess_data_functions/2_2_preprocess_gbif.R")
log_debug("Storing GBIF data")
saveRDS(gbif_df, "data/processed/gbif_df.rds")

log_debug("Preprocessing Predicts data")
source("src/2_preprocess_data_functions/2_3_preprocess_predicts.R")
log_debug("Storing Predicts data")
saveRDS(predicts_df, "data/processed/predicts_df.rds")

log_debug("Preprocessing BioTIME data")
source("src/2_preprocess_data_functions/2_4_preprocess_biotime.R")
log_debug("Storing BioTIME data")
saveRDS(biotime_df, "data/processed/biotime_df.rds")

log_debug("Preprocessing Living Planet Index data")
source("src/2_preprocess_data_functions/2_5_preprocess_lpi.R")
log_debug("Storing Living Planet Index data")
saveRDS(lpi_df, "data/processed/lpi_df.rds")

