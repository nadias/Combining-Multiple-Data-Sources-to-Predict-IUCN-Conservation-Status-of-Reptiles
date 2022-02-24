lpi_df <- lpi_df %>%
  gather(year, value, X1970:X2014, na.rm = TRUE, convert = FALSE) %>%
  dplyr::select(-X)


lpi_df$species <- with(lpi_df, paste(Genus, Species, sep = " "))

lpi_df$taxon <- with(lpi_df, paste(Genus, Species, Sub.species, sep = " "))
lpi_df$taxon <- lpi_df$taxon %>% str_trim(side = c("right"))

lpi_df$taxon_rank <- "Species"
index <- lpi_df$taxon != lpi_df$species
lpi_df$taxon_rank[index] <- "Infraspecies"

lpi_df$year <- as.integer(substring(lpi_df$year, 2))

lpi_df$metric <- "abundance"
lpi_df$issue <- "Geodetic datum assumed WGS84"   # No information on the metadata about the datum
lpi_df$dataset <- "lpi"

lpi_df <- lpi_df %>%
  rename(longitude = Decimal.Longitude,
         latitude = Decimal.Latitude,
         sampling_method = Sampling.method,
         diversity_metric_unit = Units) %>%
  dplyr::select(taxon, species, taxon_rank, longitude, latitude, sampling_method, year, metric, diversity_metric_unit,
                value, issue, dataset)
