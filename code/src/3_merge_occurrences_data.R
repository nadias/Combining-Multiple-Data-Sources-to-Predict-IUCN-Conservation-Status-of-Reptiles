
log_debug("Binding dataset rows")
occurrences_df <- bind_rows(gbif_df, predicts_df, biotime_df, lpi_df)



log_debug("Use the same value for all species and subspecies taxon ranks")

# Combine SPECIES and Species into Species
occurrences_df[occurrences_df$taxon_rank == "SPECIES",] <- occurrences_df %>%
  filter(taxon_rank == "SPECIES") %>%
  mutate(taxon_rank = "Species")

# Combine infraspecies, subspecies and variety into Subspecies
occurrences_df[occurrences_df$taxon_rank == "SUBSPECIES" |
                 occurrences_df$taxon_rank == "VARIETY" |
                 occurrences_df$taxon_rank == "Infraspecies",] <- occurrences_df %>%
  filter(taxon_rank == "SUBSPECIES" | taxon_rank == "VARIETY" | taxon_rank == "Infraspecies") %>%
  mutate(taxon_rank = "Subspecies")


occurrences_df <- occurrences_df %>% mutate(species = tolower(species))

log_debug("Storing occurrences data")
saveRDS(occurrences_df, "data/processed/occurrences_df.rds")

log_debug("Dropping intermediate datasets")
rm(gbif_df, predicts_df, biotime_df, lpi_df)