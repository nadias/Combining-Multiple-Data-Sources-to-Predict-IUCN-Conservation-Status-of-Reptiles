
# Aux functions
html_text_collapse <- function(x, trim = FALSE, collapse = "\n"){
  UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = "\n"){
  vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}

dir.create(file.path("data", "tmp"))

# Web scrapping
synonyms_df = data.frame(species=character(), synonyms=character())
counter = 0
for (i in strsplit(reptile_db_df$Species, "\\s+")) {
  print(i)
  url_string <- url(paste0("https://reptile-database.reptarium.cz/species?genus=", i[[1]], "&species=", i[[2]]), "rb")
  html_string <- read_html(url_string)
  close(url_string)
  
  synonyms <- html_string %>%
    rvest::html_nodes(xpath='/html/body//*[@class="species"]/tr[4]/td[2]') %>%
    html_text_collapse()
  
  for (j in strsplit(synonyms, "\n")[[1]]) {
    s <- unlist(strsplit(j, " —"))[1]
    s <- unlist(strsplit(s, " \\["))[1]
    s <- str_replace_all(s, "[^a-zA-Z0-9]", " ")
    s <- unlist(strsplit(s, "\\b[A-Z]+\\s+"))[1]
    s <- gsub("[[:digit:]]+", "", s)
    s <- gsub("\\b[A-Z]+\\s+", "", s)
    s <- gsub("amp;", "", s)
    s <- gsub("[[:punct:] ]+", " ",s)
    synonym <- gsub("^\\s+|\\s+$", "", s)
    
    if (tolower(paste0(i[[1]], " ", i[[2]])) != tolower(synonym)) {
      df <- data.frame(paste0(i[[1]], " ", i[[2]]), synonym)
      names(df) <- c("species", "synonyms")
      df <- df %>% distinct()
      synonyms_df <- rbind(synonyms_df, df)
    }
  }

  counter <- counter+1
  if (counter %% 1000 == 0) {
    saveRDS(synonyms_df, paste0("data/tmp/synonyms_df_",counter, ".rds"))    # Store intermediate files in case it fails midway
  }
}

saveRDS(synonyms_df, "data/tmp/synonyms_df_11341.rds")

rm(counter, i, j, s, synonym, synonyms, df, html_string)
rm(html_text_collapse, html_text_collapse.xml_nodeset, html_text_collapse.xml_node)

# Mutate both columns to lower case
synonyms_df <- synonyms_df %>%
  mutate(species = tolower(species),
         synonyms = tolower(synonyms))

# Remove duplicate rows
synonyms_df <- synonyms_df %>%
  distinct()

# Remove rows where the synonym field is empty
synonyms_df <- synonyms_df %>% filter(synonyms != "")

# Some synonyms appear in more than one species. Remove those.
synonyms_df %>% 
  group_by(synonyms) %>% 
  filter(n()>1) %>%
  summarize(n=n())

synonyms_df <- synonyms_df %>% 
  group_by(synonyms) %>% 
  filter(n() == 1)

saveRDS(synonyms_df, paste0("data/processed/synonyms_df.rds"))
