outdir = "data_s3/mapme"
resource <- "gfw_emissions"
rundir <- paste(outdir, resource, sep = "/")
tindex_file <- file.path(rundir, paste0("tileindex_", resource, ".gpkg"))

downloaded_files <- list.files(rundir, full.names = TRUE)
footprints <- lapply(downloaded_files, function(file) {
  tmp <- rast(file)
  footprint <- st_as_sf(st_as_sfc(st_bbox(tmp)))
  st_geometry(footprint) <- "geom"
  footprint$location <- sources(tmp)
  footprint
})
footprints <- do.call(rbind, footprints)
write_sf(footprints, dsn = tindex_file)