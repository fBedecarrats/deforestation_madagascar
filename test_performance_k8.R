# Install libs
remotes::install_github("mapme-initiative/mapme.biodiversity", force = TRUE,
                        upgrade = "always")
required_libs <- c( "dplyr", "tidyr", "sf", "wdpar", "tmap", "geodata", 
                    "tidygeocoder", "maptiles", "purrr", "mapme.biodiversity") 
missing_libs <- !(required_libs %in% installed.packages())
if(any(missing_libs)) install.packages(required_libs[missing_libs])
lapply(required_libs, require, character.only= TRUE)

# Get protected areas
PA_mada <- wdpa_fetch("Madagascar", wait = TRUE,
                      download_dir = "data_s3/WDPA") %>%
  st_transform(crs = "EPSG:29739") %>%
  filter(STATUS != "Proposed") %>%
  filter(DESIG != "Locally Managed Marine Area", DESIG != "Marine Park")

# honeycomb with hexagons of 5km2
PA_mada_box = st_as_sf(st_as_sfc(st_bbox(PA_mada)))
area_cell <- 5 * (1e+6)
cell_size <- 2 * sqrt(area_cell / ((3 * sqrt(3) / 2))) * sqrt(3) / 2
grid_mada <- st_make_grid(x = PA_mada_box,
                            cellsize = cell_size,
                            square = FALSE)

terrestrial_mada <- gadm(country = "Madagascar", resolution = 1, level = 0,
                         path = "data_s3/GADM") %>%
  st_as_sf() %>% 
  st_transform(crs = "EPSG:29739")

terrestrial_cells <- st_intersects(terrestrial_mada, grid_mada) %>%
  unlist()
grid_mada <- grid_mada[sort(terrestrial_cells)] %>%
  st_sf()

# set portfolio
dir.create("tmp")
grid_mada <- init_portfolio(x = grid_mada, 
                            years = 2000:2020,
                            outdir = "data_s3/mapme",
                            cores = 24,
                            add_resources = TRUE,
                            verbose = TRUE)

# Fetch and compute distance
grid_mada <-  get_resources(x = grid_mada, resource = "nelson_et_al",  
                              range_traveltime = "5k_110mio")
grid_mada <- calc_indicators(x = grid_mada,
                               "traveltime",  stats_accessibility = "mean",
                               engine = "extract")
# Fetch and compute soil properties
grid_mada <-  get_resources(x = grid_mada,
                            resources = "soilgrids",  layers = "clay", 
                            depths = "5-15cm", stats = "mean")
grid_mada <- calc_indicators(x = grid_mada,
                             "soilproperties", stats_soil = "mean", 
                             engine = "extract")
# Fetch and compute forest indicators
grid_mada <- get_resources(x = grid_mada, 
                           resources = c("gfw_treecover", "gfw_lossyear", 
                                         "gfw_emissions"))
grid_mada <- calc_indicators(x = grid_mada,
                               indicators = "treecover_area_and_emissions", 
                               min_cover = 10, min_size = 1)



