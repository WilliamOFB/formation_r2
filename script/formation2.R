# Lecture packages ----
library(tidyverse)
library(sf)
library(readr)

# Import data -----
oison_taxon <- read.csv("raw_data/oison_taxon.csv")

taxons_inpn <- read.csv("raw_data/referentiel_taxons_INPN_oison.csv")

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
  dplyr::filter(email == 'camille.riviere@ofb.gouv.fr' | email == 'didier.pujo@ofb.gouv.fr') %>% 
  dplyr::select(observation_id:nom_vernaculaire) %>% 
  view()
