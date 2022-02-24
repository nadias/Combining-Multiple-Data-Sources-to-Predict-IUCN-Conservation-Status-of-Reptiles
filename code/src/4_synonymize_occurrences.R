

# Join with species in reptile_db_df
occurrences_df <- occurrences_df %>%
  left_join(reptile_db_df, by = c("species" = "Species")) %>%
  mutate(match_reptile_db = !is.na(Author)) %>%
  dplyr::select(-c("type_species", "Author", "Subspecies", "Common_name", "Familyetc", "sp#", "changes"))


# Join with species in synonyms_df
occurrences_df <- occurrences_df %>%
  left_join(synonyms_df, by = c("species" = "synonyms"))

occurrences_df[occurrences_df$match_reptile_db==FALSE & !is.na(occurrences_df$species.y),] <- occurrences_df %>%
  filter(match_reptile_db == FALSE) %>%
  filter(!is.na(species.y)) %>%
  mutate(species = species.y)


# Remove species that cannot be fixed
occurrences_df <- occurrences_df %>%
  filter(occurrences_df$match_reptile_db==TRUE | (occurrences_df$match_reptile_db==FALSE & !is.na(occurrences_df$species.y)))


saveRDS(occurrences_df, "data/processed/occurrences_df_synonimized.rds")


# Checks
occurrences_df %>% left_join(reptile_db_df, by = c("species" = "Species")) %>% filter(is.na(Author))
#  All occurrences match reptile db


occurrences_df %>% right_join(reptile_db_df, by = c("species" = "Species")) %>% filter(is.na(taxon))
#  2491 species do not exist in the occurences dataset
#  11440 - 8949 = 2491

