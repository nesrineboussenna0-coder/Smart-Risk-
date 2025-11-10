
library(sf)
library(dplyr)
library(stringr)
library(stringi)
library(leaflet)

# === Chargement GeoJSON France ===
url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"
France_map <- st_read(url)

France_map$nom <- tolower(trimws(France_map$nom))
France_map$nom <- stri_trans_general(France_map$nom, "Latin-ASCII")

region_claims <- data_joined %>%
  group_by(Region) %>%
  summarise(Total_Claim = sum(ClaimNb, na.rm = TRUE)) %>%
  mutate(
    Region = tolower(trimws(Region)),
    Region = stri_trans_general(Region, "Latin-ASCII"),
    Region = str_replace_all(Region, '\"', '')
  )

region_mapping <- c(
  "aquitaine" = "nouvelle-aquitaine",
  "basse-normandie" = "normandie",
  "haute-normandie" = "normandie",
  "centre" = "centre-val de loire",
  "limousin" = "nouvelle-aquitaine",
  "nord-pas-de-calais" = "hauts-de-france",
  "pays-de-la-loire" = "pays de la loire",
  "poitou-charentes" = "nouvelle-aquitaine"
)
region_claims$Region <- recode(region_claims$Region, !!!region_mapping)

France_map <- France_map %>%
  left_join(region_claims, by = c("nom" = "Region")) %>%
  mutate(Total_Claim = replace_na(Total_Claim, 0))

