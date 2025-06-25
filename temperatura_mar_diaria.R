library(ncdf4)
library(lubridate)
library(glue)
library(tidyverse)
library(sf)

# Get credentials
# For local testing - hardcoded
# pwd <- "QTX*mkz*jbh5hgj@xqp"
# user <- "jpinto3"

# Uncomment below to use command-line arguments instead
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

# Run the Python script (no arguments needed for local testing)
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

# Get all hours data
todas_as_horas <- bind_rows(continente, madeira, acores)
todas_as_horas$lat <- as.numeric(todas_as_horas$lat)
todas_as_horas$lon <- as.numeric(todas_as_horas$lon)
todas_as_horas$thetao <- as.numeric(todas_as_horas$thetao)

# Load beach data
heatspots <- read_rds("coordenadas_heatspots.rds")
heatspots <- heatspots %>%
  rename(geometry = coordenadas_praia) %>%
  mutate(
    heatspot_lat = st_coordinates(geometry)[, 2],
    heatspot_lon = st_coordinates(geometry)[, 1]
  ) %>%
  select(-geometry)

praias_com_concelhos <- read_rds("praias_com_concelhos.rds")

# Join beaches with hotspots
praias_com_heatspots <- left_join(praias_com_concelhos, heatspots) %>%
  mutate(
    beach_lat = st_coordinates(corrdenadas_heatspot)[, 2],
    beach_lon = st_coordinates(corrdenadas_heatspot)[, 1]
  )

# Function to find nearest temperature point
find_nearest_temperature <- function(beaches_df, temp_df) {
  # For each unique beach location
  unique_beaches <- beaches_df %>%
    distinct(nome_praia, Concelho, beach_lat, beach_lon)

  # For each beach, find nearest temperature point at each time
  results <- list()

  for (i in 1:nrow(unique_beaches)) {
    beach <- unique_beaches[i, ]

    # Get unique times
    unique_times <- unique(temp_df$time)

    beach_temps <- data.frame()

    for (t in unique_times) {
      # Get temperature data for this time
      temp_at_time <- temp_df %>% filter(time == t)

      # Calculate distances
      distances <- sqrt(
        (temp_at_time$lat - beach$beach_lat)^2 +
          (temp_at_time$lon - beach$beach_lon)^2
      )

      # Find nearest point
      nearest_idx <- which.min(distances)

      if (length(nearest_idx) > 0) {
        temp_point <- temp_at_time[nearest_idx, ]
        beach_temps <- rbind(beach_temps, temp_point)
      }
    }

    # Combine beach info with temperature data
    if (nrow(beach_temps) > 0) {
      beach_data <- beach %>%
        select(-beach_lat, -beach_lon) %>%
        crossing(beach_temps)
      results[[i]] <- beach_data
    }
  }

  # Combine all results
  bind_rows(results)
}

print("Finding nearest temperature points for each beach...")
praias_heatspots_horas <- find_nearest_temperature(
  praias_com_heatspots,
  todas_as_horas
)

# Add back the full beach information
praias_heatspots_horas <- praias_heatspots_horas %>%
  left_join(
    praias_com_heatspots %>%
      select(-beach_lat, -beach_lon) %>%
      st_drop_geometry()
  )

# Convert time and filter hours
praias_heatspots_horas <- praias_heatspots_horas %>%
  mutate(time = ymd_hms(time)) %>%
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
  left_join(praias_nomes_finais) %>%
  select(-nome_praia, -corrdenadas_heatspot, -heatspot_lat, -heatspot_lon) %>%
  rename("nome_praia" = "nome_praia_final")

tectos <- tectos %>%
  rename("nome_praia" = "nome_praia_final")

praias_heatspots_horas <- praias_heatspots_horas %>%
  left_join(tectos) %>%
  replace_na(list(nome_praia = "Corvo/Areia (Corvo)"))

# Check results
print(paste(
  "Total beaches with temperature data:",
  n_distinct(praias_heatspots_horas$nome_praia)
))
print("Sample of beaches by region:")
print(
  praias_heatspots_horas %>%
    distinct(nome_praia, Concelho) %>%
    group_by(substr(Concelho, 1, 3)) %>%
    slice(1:3)
)

# Save results
write_rds(praias_heatspots_horas, "praias_completas.rds")
write_csv(praias_heatspots_horas, "praias_completas.csv")

print("Processing completed! Files saved:")
print("- praias_completas.rds")
print("- praias_completas.csv")
