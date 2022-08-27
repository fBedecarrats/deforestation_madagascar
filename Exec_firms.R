# # Le package est en cours de développement, toujours installer la version en cours
remotes::install_github("mapme-initiative/mapme.biodiversity", 
                        upgrade = "always")

librairies_requises <- c( # On liste les librairies dont on a besoin
  "dplyr", # Pour faciliter la manipulation de données tabulaires
  "tidyr", # Pour reformater les données (pivots...)
  "sf", # Pour faciliter la manipulation de données géographiques
  "wdpar", # Pour télécharger simplement la base d'aires protégées WDPA
  "tmap", # Pour produire de jolies carte
  "geodata", # Pour télécharger simplement les frontières administratives
  "tidygeocoder", # pour obtenir les coordo GPS d'un point à partir de son nom
  "maptiles", # Pour télécharger des fonds de carte 
  "purrr", # Pour utiliser des formes fonctionnelles de programmation (ex. map)
  "mapme.biodiversity") # Acquisition et traitement des données du projet

# On regarde parmi ces librairies lesquelles ne sont pas installées
manquantes <- !(librairies_requises %in% installed.packages())
# On installe celles qui manquent
if(any(manquantes)) install.packages(librairies_requises[manquantes])
# On charge toutes les librairies requises
invisible(lapply(librairies_requises, require, character.only= TRUE))

# Chargement des données
load("data_s3/grille_mada_donnees_raster.rds")

# Données de feux
grille_mada <- get_resources(x = grille_mada, resource = "nasa_firms",
                             instrument = "MODIS")
# Indicateurs d'incendies
grille_mada <- calc_indicators(x = grille_mada,
                               "active_fire_counts")
grille_mada <- calc_indicators(x = grille_mada,
                               "active_fire_properties")
grille_mada_init <- grille_mada

save(grille_mada, file = "data_s3/grille_mada_donnees_raster.rds")

# Sauvegarde sur le serveur distant pour éviter de télécharger à chaque fois
aws.s3::s3sync(path = "data_s3",
               bucket = "fbedecarrats",
               prefix = "diffusion/deforestation_madagascar/data_s3/",
               create = FALSE,
               region = "",
               verbose = FALSE)
