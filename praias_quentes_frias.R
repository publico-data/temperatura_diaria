library(reticulate)
library(CopernicusMarine)
library(ncdf4)
library(lubridate)
library(glue)
library(tidyverse)
library(sf)
library(httr2)

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

#round hour over

praias_heatspots_horas <- praias_heatspots_horas %>% 
  mutate(
    time = round_date(time, "hour")
  )

historico <- read_rds("historico.rds")

above_avg <- historico %>% 
  ungroup() %>% 
  filter(mes==month(Sys.Date())) %>% 
  filter(day==day(Sys.Date())) %>% 
  mutate(min=mean-std) %>% 
  mutate(max=mean+std) %>% 
  mutate(min2=mean-(2*std)) %>% 
  mutate(max2=mean+(2*std)) %>% 
  select(-mes,-day,-std) %>% 
  rename("Concelho"="concelho")


praias_nomes_finais <- read_delim("praias_nomes_finais.csv", delim = ",")

above_avg <- above_avg %>% 
  left_join(praias_nomes_finais) %>% 
  select(nome_praia_final,Concelho,min,max,min2,max2,mean)

praias_heatspots_horas <- praias_heatspots_horas %>% 
  st_drop_geometry()%>% 
  left_join(praias_nomes_finais) %>% 
  select(-nome_praia,-geometry) %>% 
  rename("nome_praia"="nome_praia_final")

above_avg <- above_avg %>% 
  rename("nome_praia"="nome_praia_final")

praias_above_avg <- praias_heatspots_horas %>%
  left_join(above_avg) %>% 
  replace_na(list(nome_praia = "Corvo/Areia (Corvo)")) 

praias_above_avg$thetao <- as.numeric(praias_above_avg$thetao)

praias_above_avg <- praias_above_avg %>%
  mutate(dif=thetao-mean)


#time to hms()

praias_above_avg <- praias_above_avg %>% 
  mutate(
    hours = hour(time)
  ) %>% 
  filter(
    hours == 14
  ) %>% 
  select(-hours,-lat,-lon,-Concelho)

#Top 10 com maiores diferenças

praias_above_avg_top_10 <- praias_above_avg %>% 
  filter(
    day(time) == 27
  ) %>% 
  arrange(desc(dif)) %>% 
  select(nome_praia,thetao,mean,dif) %>% 
  head(10)


praias_below_avg_top_10 <- praias_above_avg %>% 
  filter(
    day(time) == 27
  ) %>% 
  arrange(dif) %>% 
  select(nome_praia,thetao,mean,dif) %>% 
  head(10)

