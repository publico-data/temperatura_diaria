library(reticulate)
library(CopernicusMarine)
library(ncdf4)
library(lubridate)
library(glue)
library(tidyverse)
library(sf)
library(httr2)

original_dir <- getwd()

#Código com as novas funções do Copernicus

# Get the secret key from command-line arguments
# args <- commandArgs(trailingOnly = TRUE)

# 
# options(CopernicusMarine_uid = user)
# options(CopernicusMarine_pwd = pwd)
# 
# 
# #Portugal Continental
# start_date <- ymd_hms(glue("{Sys.Date()} 08:00:00"))               # start_date
# end_date <- ymd_hms(glue("{Sys.Date()+1} 20:00:00"))               # end_date
# 
# cms_download_subset_ultimate(
#     destination   = glue("data/continente.nc"),
#     product       = "IBI_ANALYSISFORECAST_PHY_005_001",
#     layer         = "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
#     variable      = "thetao",
#     region        = c(-9.646719069971319, 36.7143279919028, -7.364793639893833, 41.884404655537004),
#     timerange     = c(start_date, end_date),
#     overwrite     = TRUE
#   )
#   
#   
#   #Madeira
# cms_download_subset(
#     destination   = glue("data/madeira.nc"),
#     product       = "IBI_ANALYSISFORECAST_PHY_005_001",
#     layer         = "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
#     variable      = "thetao",
#     region        = c(-17.31389417651776, 32.37276288911421, -16.23795594074014,  33.14961360086701),
#     timerange     = c(start_date, end_date),
#     overwrite     = TRUE
#   )
#   
#   #Açores
# cms_download_subset(
#     destination   = glue("data/acores.nc"),
#     product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
#     layer         = "cmems_mod_glo_phy_anfc_0.083deg_PT1H-m",
#     variable      = "thetao",
#     region        = c(-31.38975950285971, 36.822334920430556, -24.94078489348473, 39.827095662618056),
#     verticalrange = c(0.494024991989134,0.494024991989136),
#     timerange     = c(start_date, end_date),
#     overwrite     = TRUE
#   )
# 
#   

args <- commandArgs(trailingOnly = TRUE)

pwd <- args[1]
user <- args[2]



install_python() 

virtualenv_create(envname = "praias")
virtualenv_install("praias", packages = c("copernicusmarine"))

use_virtualenv("praias", required = TRUE)
py_install("copernicusmarine")

cm <- import("copernicusmarine")
os <- import("os")

# Define the desired output directory relative to the R project root
output_directory <- "data/"

# Use the absolute path for the output directory based on R's getwd()
absolute_output_directory <- os$path$join(os$getcwd(), output_directory)

# Check if the current directory is not already 'data/' and change if necessary
if (os$getcwd() != absolute_output_directory) {
  if (!os$path$exists(absolute_output_directory)) {
    os$makedirs(absolute_output_directory)
  }
  os$chdir(absolute_output_directory)
}


# Check and delete existing credentials file if it exists
credentials_file <- os$path$expanduser("~/.copernicusmarine/.copernicusmarine-credentials")
if (os$path$exists(credentials_file)) {
  os$remove(credentials_file)
}


cm$login(user, pwd)  # Replace with your credentials


#Portugal Continental
start_date <- format(ymd_hms(glue("{Sys.Date()} 08:00:00")), "%Y-%m-%d %H:%M:%S")
end_date <- format(ymd_hms(glue("{Sys.Date() + 1} 20:00:00")), "%Y-%m-%d %H:%M:%S")


result <- cm$subset(
  dataset_id = "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
  dataset_version = "202211",
  start_datetime = start_date,
  end_datetime = end_date,
  variables = list("thetao"),
  minimum_longitude = -9.64671906997131,
  maximum_longitude = 7.364793639893833,
  minimum_latitude = 36.7143279919028,
  maximum_latitude = 41.884404655537004,
  force_download = TRUE,
)

# Rename the downloaded files

#get the file inside data that starts with "cmems_mod"
original_file_name <- list.files(pattern = "cmems_mod")
new_file_name <- "continente.nc"  # This is the new name you want to give the file
os$rename(original_file_name, new_file_name)



#Madeira

result <- cm$subset(
  dataset_id = "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
  dataset_version = "202211",
  start_datetime = start_date,
  end_datetime = end_date,
  variables = list("thetao"),
  minimum_longitude = -17.31389417651776,
  maximum_longitude = -16.23795594074014,
  minimum_latitude = 32.37276288911421,
  maximum_latitude = 33.14961360086701,
  force_download = TRUE,
)

# Rename the downloaded file

#get the file inside data that starts with "cmems_mod"
original_file_name <- list.files(pattern = "cmems_mod")
new_file_name <- "madeira.nc"  # This is the new name you want to give the file
os$rename(original_file_name, new_file_name)



#Açores

result <- cm$subset(
  dataset_id = "cmems_mod_glo_phy_anfc_0.083deg_PT1H-m",
  dataset_version = "202211",
  start_datetime = start_date,
  end_datetime = end_date,
  variables = list("thetao"),
  minimum_longitude = -31.38975950285971,
  maximum_longitude = -24.94078489348473,
  minimum_latitude = 36.822334920430556,
  maximum_latitude = 33.14961360086701,
  force_download = TRUE,
)

# Rename the downloaded file

#get the file inside data that starts with "cmems_mod"
original_file_name <- list.files(pattern = "cmems_mod")
new_file_name <- "acores.nc"  # This is the new name you want to give the file
os$rename(original_file_name, new_file_name)

setwd(original_dir)

nc_df <- nc_open("data/continente.nc")
  
  
      dim_lon <- ncvar_get(nc_df, "longitude")
      dim_lat <- ncvar_get(nc_df, "latitude")
      dim_time <- ncvar_get(nc_df, "time")
      
      t_units <- ncatt_get(nc_df, "time", "units")
      t_ustr <- strsplit(t_units$value, " ")
      t_dstr <- strsplit(unlist(t_ustr)[3], "-")
      date <- ymd(t_dstr) + dhours(dim_time)
      
      coords <- as.matrix(expand.grid(dim_lon, dim_lat, date))
      
      temperatura <- ncvar_get(nc_df, "thetao", collapse_degen=FALSE)
      
      continente <- data.frame(cbind(coords, temperatura))
      names(continente) <- c("lon", "lat", "time", "thetao")
      
      continente <- continente %>% 
        distinct(lon, lat, time, .keep_all = TRUE) %>% 
        filter(!is.na(thetao))
    
  #Madeira    
  nc_df <- nc_open("data/madeira.nc")
      
      
      dim_lon <- ncvar_get(nc_df, "longitude")
      dim_lat <- ncvar_get(nc_df, "latitude")
      dim_time <- ncvar_get(nc_df, "time")
      
      t_units <- ncatt_get(nc_df, "time", "units")
      t_ustr <- strsplit(t_units$value, " ")
      t_dstr <- strsplit(unlist(t_ustr)[3], "-")
      date <- ymd(t_dstr) + dhours(dim_time)
      
      coords <- as.matrix(expand.grid(dim_lon, dim_lat, date))
      
      temperatura <- ncvar_get(nc_df, "thetao", collapse_degen=FALSE)
      
      madeira <- data.frame(cbind(coords, temperatura))
      names(madeira) <- c("lon", "lat", "time", "thetao")
      madeira <- madeira %>% 
        distinct(lon, lat, time, .keep_all = TRUE) %>% 
        filter(!is.na(thetao))
      
  #Açores
      nc_df <- nc_open("data/acores.nc")
      
      
      dim_lon <- ncvar_get(nc_df, "longitude")
      dim_lat <- ncvar_get(nc_df, "latitude")
      dim_time <- ncvar_get(nc_df, "time")
      
      t_units <- ncatt_get(nc_df, "time", "units")
      t_ustr <- strsplit(t_units$value, " ")
      t_dstr <- strsplit(unlist(t_ustr)[3], "-")
      date <- ymd(t_dstr) + dhours(dim_time)
      
      coords <- as.matrix(expand.grid(dim_lon, dim_lat, date))
      
      temperatura <- ncvar_get(nc_df, "thetao", collapse_degen=FALSE)
      
      acores <- data.frame(cbind(coords, temperatura))
      names(acores) <- c("lon", "lat", "time", "thetao")
      acores <- acores %>% 
        distinct(lon, lat, time, .keep_all = TRUE) %>% 
        filter(!is.na(thetao))
      
      
pais_inteiro <- bind_rows(continente, madeira, acores) %>% 
  filter(time==ymd_hms(glue("{Sys.Date()} 08:00:00")))
pais_inteiro$lat <- as.numeric(pais_inteiro$lat)
pais_inteiro$lon <- as.numeric(pais_inteiro$lon)
pais_inteiro$thetao <- as.numeric(pais_inteiro$thetao)

heatspots <- read_rds("coordenadas_heatspots.rds")
heatspots <- heatspots %>% 
  rename(geometry = coordenadas_praia) %>% 
  mutate(lat = (st_coordinates(geometry)[,2]), lon = (st_coordinates(geometry)[,1])) %>%
  select(-geometry)

praias_com_concelhos <- read_rds("praias_com_concelhos.rds")
praias_com_concelhos <- praias_com_concelhos %>% 
  mutate(lat = (st_coordinates(geometry)[,2]), lon = (st_coordinates(geometry)[,1]))
  
#juntar tudo
praias_com_heatspots <- left_join(praias_com_concelhos,heatspots)
praias_com_heatspots <- praias_com_heatspots %>% select(-lat,-lon) %>% 
  mutate(lat = (st_coordinates(corrdenadas_heatspot)[,2]), lon = (st_coordinates(corrdenadas_heatspot)[,1]))


so_praias_com_heatspots <- left_join(praias_com_heatspots,pais_inteiro) %>% 
  select(-thetao,-time)
# %>% select(-corrdenadas_heatspot,-lat,-lon) %>% 
# mutate(lat = (st_coordinates(geometry)[,2]), lon = (st_coordinates(geometry)[,1])) %>% 
# select(-geometry)

#Ir buscar todas as horas para juntar às praias com heatspots
todas_as_horas <- bind_rows(continente, madeira, acores)

todas_as_horas$lat <- as.numeric(todas_as_horas$lat)
todas_as_horas$lon <- as.numeric(todas_as_horas$lon)

praias_heatspots_horas <- left_join(so_praias_com_heatspots,todas_as_horas) %>%
  mutate(time=ymd_hms(time))%>%
  select(-corrdenadas_heatspot, -lat, -lon)

praias_heatspots_horas <- praias_heatspots_horas %>% 
  mutate(lat = (st_coordinates(geometry)[,2]), lon = (st_coordinates(geometry)[,1])) %>% 
  select(-geometry)


praias_heatspots_horas <- praias_heatspots_horas %>% 
  mutate(
    hours = hour(time)
  ) %>% 
  filter(
    hours >= 8 & hours <= 20
  ) %>% 
  select(-hours)
  
historico <- read_rds("historico.rds")

tectos <- historico %>% 
  ungroup() %>% 
  filter(mes==month(Sys.Date())) %>% 
  filter(day==day(Sys.Date())) %>% 
  mutate(min=mean-std) %>% 
  mutate(max=mean+std) %>% 
  mutate(min2=mean-(2*std)) %>% 
  mutate(max2=mean+(2*std)) %>% 
  select(-mes,-day,-mean,-std) %>% 
  rename("Concelho"="concelho")

praias_nomes_finais <- read_delim("praias_nomes_finais.csv", delim = ",")

tectos <- tectos %>% 
  left_join(praias_nomes_finais) %>% 
  select(nome_praia_final,Concelho,min,max,min2,max2)


praias_heatspots_horas <- praias_heatspots_horas %>% 
  st_drop_geometry()%>% 
  left_join(praias_nomes_finais) %>% 
  select(-nome_praia,-geometry) %>% 
  rename("nome_praia"="nome_praia_final")

tectos <- tectos %>% 
  rename("nome_praia"="nome_praia_final")

praias_heatspots_horas <- praias_heatspots_horas %>% 
  left_join(tectos) %>% 
  replace_na(list(nome_praia = "Corvo/Areia (Corvo)"))

write_rds(praias_heatspots_horas,"praias_completas.rds")
write_csv(praias_heatspots_horas,"praias_completas.csv")


# write_rds(pais_inteiro,"dia.rds")
