# Lecture packages ----
library(tidyverse)
library(sf)
library(readr)

# Import data -----
oison_taxon <- read.csv("raw_data/oison_taxon.csv")

taxons_inpn <-
  read.csv("raw_data/referentiel_taxons_INPN_oison.csv")

geom_oison <- read_sf("raw_data/oison_geometry.gpkg")

# Traitement des données ----
## Sélection ----
oison_taxon %>%
  dplyr::select(nom_vernaculaire,
                nom_scientifique,
                date,
                nom) %>%
  view()

oison_taxon %>%
  dplyr::select(observation_id:date,
                nom_vernaculaire) %>%
  dplyr::select(-email) %>% # exclusion
  view()

oison_taxon %>%
  dplyr::select(!observation_id:date) %>% # exclusion
  view()

oison_taxon %>%
  dplyr::select(!c(email, heure)) %>% # exclusion some colonnes
  view()

### Autres fonctions de sélection ----
oison_taxon %>%
  dplyr::select(starts_with("nom")) %>% # sélection intitulés champs
  view()

# pareil pour la fin avec ends_with()

oison_taxon %>%
  dplyr::select(contains("recherche")) %>% # sélection intérieur champs
  view()

# avec conditions ('|' pour 'ou')

# on peut créer un vecteur 'nom colonnes' et sélectionner avec ça

## Filtre ----
oison_taxon %>%
  dplyr::filter(nom_scientifique == 'Canis lupus') %>%
  dplyr::select(observation_id:nom_vernaculaire) %>%
  view()

oison_taxon %>%
  dplyr::filter(email == 'camille.riviere@ofb.gouv.fr' |
                  email == 'didier.pujo@ofb.gouv.fr') %>%
  dplyr::select(observation_id:nom_vernaculaire) %>%
  view()

# filtre sur 2 colonnes
oison_taxon %>%
  dplyr::filter(presence == 'Absent' & !is.na(nombre_individu)) %>%
  dplyr::select(observation_id,
                date,
                nom_scientifique,
                presence,
                nombre_individu,
                nom_vernaculaire) %>%
  filter(nombre_individu > 0) %>%
  view()

## Modification & ajout de data ----
oison_taxon %>%
  dplyr::select(observation_id, nom:email) %>%
  mutate (
    nom_complet_obs = paste(nom, prenom),
    # addition texte
    addition = 1 + 10,
    # addition chiffres
    racine_carree = sqrt(observation_id)
  ) %>%
  view()

## Calcul de colonnes ----
oison_taxon %>%
  dplyr::select(nom_scientifique, nombre_individu) %>%
  dplyr::filter(nom_scientifique == 'Bufo bufo' |
                  nom_scientifique == 'Elona quimperiana') %>%
  group_by(nom_scientifique) %>%
  summarise(
    nb_total_oison = sum(nombre_individu, na.rm = T),
    nb_max_oison = max(nombre_individu, na.rm = T)
  ) %>%
  view()

## Tri ----
oison_taxon %>%
  dplyr::select(observation_id, date, nom_vernaculaire) %>%
  mutate(date = as.Date(date)) %>% # définition des dates en 'vraies' dates
  arrange(date) %>% # tri par date
  view()

oison_taxon %>%
  dplyr::select(observation_id, date, nom_vernaculaire) %>%
  mutate(date = as.Date(date)) %>% # définition des dates en 'vraies' dates
  arrange((date), nom_vernaculaire) %>% # tri par date et par nom
  view()

## Extraction de valeurs ----
corine_chouette <- oison_taxon %>%
  dplyr::select(observation_id, date, nom_vernaculaire, corine_label) %>%
  filter(nom_vernaculaire == 'Chouette hulotte') %>%
  pull(corine_label) # extraction ici

corine_chouette[!is.na(corine_chouette)]

corine_chouette %>% unique()

corine_chouette[duplicated(corine_chouette)] %>% unique()

## Renommage colonnes ----
oison_taxon %>% 
  dplyr::select(observation_id, nom_vernaculaire) %>% 
  rename(taxon_nom_commun = nom_vernaculaire) %>% 
  head() %>% # 6 premières lignes du tableau
  view()

oison_taxon %>% 
  dplyr::select(observation_id, nom_vernaculaire) %>% 
  rename_with(toupper) %>% # tout en majuscule ou minuscule (tolower)
  view()

## Relocalisation
oison_taxon %>% 
  dplyr::select(observation_id, date, nom_vernaculaire, uuid) %>% 
  relocate(uuid, .after = observation_id) %>% # par défaut en 1er
  view()

