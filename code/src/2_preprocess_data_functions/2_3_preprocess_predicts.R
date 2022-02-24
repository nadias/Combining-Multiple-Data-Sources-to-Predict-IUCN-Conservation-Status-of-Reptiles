predicts_df <- predicts_df %>%
  filter(Class == 'Reptilia') %>%
  filter(Rank == 'Species' | Rank == 'Infraspecies') %>%
  dplyr::select(Taxon, Best_guess_binomial, Rank, Longitude, Latitude, Coordinates_method, Sampling_method,
                Sample_start_earliest, Sample_end_latest, Sample_midpoint, Diversity_metric, Diversity_metric_unit,
                Diversity_metric_type, Measurement) %>%
  drop_na(Taxon, Best_guess_binomial, Rank, Longitude, Latitude) %>%
  rename(taxon = Taxon,
         species = Best_guess_binomial,
         taxon_rank = Rank,
         longitude = Longitude,
         latitude = Latitude,
         coordinates_method = Coordinates_method,
         sampling_method = Sampling_method,
         sample_start_earliest = Sample_start_earliest,
         sample_end_latest = Sample_end_latest,
         sample_midpoint = Sample_midpoint,
         metric = Diversity_metric,
         diversity_metric_unit = Diversity_metric_unit,
         diversity_metric_type = Diversity_metric_type,
         value = Measurement)

predicts_df$issue <- "Geodetic datum assumed WGS84"   # No information on the metadata about the datum
predicts_df$dataset <- "predicts"


predicts_df$year <- as.integer(format(as.Date(predicts_df$sample_midpoint), "%Y"))
predicts_df$month <- as.integer(format(as.Date(predicts_df$sample_midpoint), "%m"))
predicts_df$day <- as.integer(format(as.Date(predicts_df$sample_midpoint), "%d"))
