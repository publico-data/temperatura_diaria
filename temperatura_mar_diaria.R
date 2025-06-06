library(ncdf4)
library(lubridate)
library(glue)
library(tidyverse)
library(sf)

# Get credentials from command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript process_temperatura_mar.R <password> <username>")
}

pwd <- args[1]
user <- args[2]

# ----------------------
# 1. Call Python script to download data
# ----------------------
print("Calling Python script to download data...")

# Check if Python script exists
if (!file.exists("copernicus_download.py")) {
  stop("Error: copernicus_download.py not found in the current directory!")
}

# Run the Python script with credentials
result <- system2(
  "python",
  args = c("copernicus_download.py", pwd, user),
  stdout = TRUE,
  stderr = TRUE
)

# Check if the Python script ran successfully
if (
  !file.exists("data/continente.nc") ||
    !file.exists("data/madeira.nc") ||
    !file.exists("data/acores.nc")
) {
  cat("Python script output:\n")
  cat(result, sep = "\n")
  stop(
    "Error: Python script did not create all expected files. Check the output above."
  )
}

print("Data download completed successfully!")

# ----------------------
# 2. Process downloaded data
# ----------------------
print("Processing downloaded data...")

# Function to read and process NetCDF file
process_nc_file <- function(filepath, region_name) {
  nc_df <- nc_open(filepath)

  dim_lon <- ncvar_get(nc_df, "longitude")
  dim_lat <- ncvar_get(nc_df, "latitude")
  dim_time <- ncvar_get(nc_df, "time")

  t_units <- ncatt_get(nc_df, "time", "units")
  t_ustr <- strsplit(t_units$value, " ")
  t_dstr <- strsplit(unlist(t_ustr)[3], "-")
  date <- ymd(t_dstr) + dhours(dim_time)

  coords <- as.matrix(expand.grid(dim_lon, dim_lat, date))

  temperatura <- ncvar_get(nc_df, "thetao", collapse_degen = FALSE)

  nc_close(nc_df)

  df <- data.frame(cbind(coords, temperatura))
  names(df) <- c("lon", "lat", "time", "thetao")

  df <- df %>%
    distinct(lon, lat, time, .keep_all = TRUE) %>%
    filter(!is.na(thetao))

  return(df)
}

# Process each region
continente <- process_nc_file("data/continente.nc", "continente")
madeira <- process_nc_file("data/madeira.nc", "madeira")
acores <- process_nc_file("data/acores.nc", "acores")

# Combine all regions and filter for 8:00 AM today
pais_inteiro <- bind_rows(continente, madeira, acores) %>%
  filter(time == ymd_hms(glue("{Sys.Date()} 08:00:00")))

pais_inteiro$lat <- as.numeric(pais_inteiro$lat)
pais_inteiro$lon <- as.numeric(pais_inteiro$lon)
pais_inteiro$thetao <- as.numeric(pais_inteiro$thetao)

# Load beach data
heatspots <- read_rds("coordenadas_heatspots.rds")
heatspots <- heatspots %>%
  rename(geometry = coordenadas_praia) %>%
  mutate(
    lat = st_coordinates(geometry)[, 2],
    lon = st_coordinates(geometry)[, 1]
  ) %>%
  select(-geometry)

praias_com_concelhos <- read_rds("praias_com_concelhos.rds")
praias_com_concelhos <- praias_com_concelhos %>%
  mutate(
    lat = st_coordinates(geometry)[, 2],
    lon = st_coordinates(geometry)[, 1]
  )

# Join beaches with hotspots
praias_com_heatspots <- left_join(praias_com_concelhos, heatspots)
praias_com_heatspots <- praias_com_heatspots %>%
  select(-lat, -lon) %>%
  mutate(
    lat = st_coordinates(corrdenadas_heatspot)[, 2],
    lon = st_coordinates(corrdenadas_heatspot)[, 1]
  )

so_praias_com_heatspots <- left_join(praias_com_heatspots, pais_inteiro) %>%
  select(-thetao, -time)

# Get all hours data
todas_as_horas <- bind_rows(continente, madeira, acores)
todas_as_horas$lat <- as.numeric(todas_as_horas$lat)
todas_as_horas$lon <- as.numeric(todas_as_horas$lon)

# Join beaches with hourly data
praias_heatspots_horas <- left_join(so_praias_com_heatspots, todas_as_horas) %>%
  mutate(time = ymd_hms(time)) %>%
  select(-corrdenadas_heatspot, -lat, -lon)

praias_heatspots_horas <- praias_heatspots_horas %>%
  mutate(
    lat = st_coordinates(geometry)[, 2],
    lon = st_coordinates(geometry)[, 1]
  ) %>%
  select(-geometry)

# Filter for hours between 8 AM and 8 PM
praias_heatspots_horas <- praias_heatspots_horas %>%
  mutate(hours = hour(time)) %>%
  filter(hours >= 8 & hours <= 20) %>%
  select(-hours)

# Round time to nearest hour
praias_heatspots_horas <- praias_heatspots_horas %>%
  mutate(time = round_date(time, "hour"))

# Load historical data and calculate thresholds
historico <- read_rds("historico.rds")

tectos <- historico %>%
  ungroup() %>%
  filter(mes == month(Sys.Date())) %>%
  filter(day == day(Sys.Date())) %>%
  mutate(
    min = mean - std,
    max = mean + std,
    min2 = mean - (2 * std),
    max2 = mean + (2 * std)
  ) %>%
  select(-mes, -day, -mean, -std) %>%
  rename("Concelho" = "concelho")

# Load final beach names
praias_nomes_finais <- read_delim("praias_nomes_finais.csv", delim = ",")

tectos <- tectos %>%
  left_join(praias_nomes_finais) %>%
  select(nome_praia_final, Concelho, min, max, min2, max2)

# Final processing
praias_heatspots_horas <- praias_heatspots_horas %>%
  st_drop_geometry() %>%
  left_join(praias_nomes_finais) %>%
  select(-nome_praia, -geometry) %>%
  rename("nome_praia" = "nome_praia_final")

tectos <- tectos %>%
  rename("nome_praia" = "nome_praia_final")

praias_heatspots_horas <- praias_heatspots_horas %>%
  left_join(tectos) %>%
  replace_na(list(nome_praia = "Corvo/Areia (Corvo)"))

# Save results
write_rds(praias_heatspots_horas, "praias_completas.rds")
write_csv(praias_heatspots_horas, "praias_completas.csv")

print("Processing completed! Files saved:")
print("- praias_completas.rds")
print("- praias_completas.csv")
