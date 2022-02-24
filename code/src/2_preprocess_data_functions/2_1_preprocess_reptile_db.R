# !diagnostics off

group_list <- strsplit(reptile_db_df$Familyetc, split="[(]")

reptile_db_df$group <- NA
for (row_i in 1:length(group_list)){
  reptile_db_df$group[row_i] <- group_list[[row_i]][length(group_list[[row_i]])]
}

reptile_db_df$group <- str_extract(string = reptile_db_df$group, pattern=".*(?=\\))")


reptile_db_df[reptile_db_df$Familyetc == "Sphenodontidae, Rhynchocephalia",] <- reptile_db_df %>%
  filter(Familyetc == "Sphenodontidae, Rhynchocephalia") %>%
  mutate(group = "tuataras")


reptile_db_df[reptile_db_df$Familyetc == "Amphisbaenidae, Amphisbaenia, Lacertoidea, Squamata" |
                reptile_db_df$Familyetc == "Bipedidae, Amphisbaenia, Lacertoidea, Squamata" |
                reptile_db_df$Familyetc == "Blanidae, Amphisbaenia, Lacertoidea, Squamata" |
                reptile_db_df$Familyetc == "Cadeidae, Amphisbaenia, Lacertoidea, Squamata" |
                reptile_db_df$Familyetc == "Rhineuridae, Amphisbaenia, Lacertoidea, Squamata" |
                reptile_db_df$Familyetc == "Trogonophidae, Amphisbaenia, Lacertoidea, Squamata",] <- reptile_db_df %>%
  filter(Familyetc == "Amphisbaenidae, Amphisbaenia, Lacertoidea, Squamata" |
           Familyetc == "Bipedidae, Amphisbaenia, Lacertoidea, Squamata" |
           Familyetc == "Blanidae, Amphisbaenia, Lacertoidea, Squamata" |
           Familyetc == "Cadeidae, Amphisbaenia, Lacertoidea, Squamata" |
           Familyetc == "Rhineuridae, Amphisbaenia, Lacertoidea, Squamata" |
           Familyetc == "Trogonophidae, Amphisbaenia, Lacertoidea, Squamata") %>%
  mutate(group = "amphisbaenians")


reptile_db_df %>% distinct(group)

# Unify crocodiles
reptile_db_df %>% filter(group == "alligators")
reptile_db_df %>% filter(group == "Crocodylia, crocodiles")
reptile_db_df %>% filter(group == "crocodiles")

reptile_db_df[reptile_db_df$group == "alligators" |
                reptile_db_df$group == "Crocodylia, crocodiles",] <- reptile_db_df %>%
  filter(group == "alligators" |
           group == "Crocodylia, crocodiles") %>%
  mutate(group = "crocodiles")

# Unify lizards
reptile_db_df[reptile_db_df$group == "lizards: geckos",] <- reptile_db_df %>%
  filter(group == "lizards: geckos") %>%
  mutate(group = "lizards")



# Add species identifier
species_list <- strsplit(reptile_db_df$Species, split=" ")

reptile_db_df$identifier <- NA

index <- 1
for (row_i in 1:length(species_list)){
  reptile_db_df$identifier[row_i] <- paste0(substr(species_list[[row_i]][[1]], start = 1, stop = 2),
                                            str_to_title(substr(species_list[[row_i]][[2]], start = 1, stop = 2)),
                                            index)
  if (index < 999) {
    index <- index+1
  } else {
    index <- 1
  }
}


reptile_db_df %>% distinct(identifier)

reptile_db_df <- reptile_db_df %>% mutate(Species = tolower(Species))


saveRDS(reptile_db_df, "data/processed/reptile_db_df.rds")
