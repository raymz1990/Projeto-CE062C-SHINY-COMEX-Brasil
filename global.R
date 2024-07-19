#---------------------------------------------------------------------
# Pacotes

library(tidyverse)
library(sf)
library(leaflet)
library(stringr)
library(networkD3)

#---------------------------------------------------------------------
# Carregando arquivos

# dados <- read.csv('./data/dados.csv')
# tb_city <- read.csv('./data/tb_city.csv')
# tb_country <- read.csv('./data/tb_country.csv')
# tb_products <- read.csv('./data/tb_products.csv')
# database <- read.csv('./data/database.csv')

# carregando arquivos .rds (foram convertidos neste formato para comprimir 
# o tamanho)
# dados       <- readRDS('./files/dados.rds')
tb_city     <- readRDS('./files/tb_city.rds')
tb_country  <- readRDS('./files/tb_country.rds')
# tb_products <- readRDS('./files/tb_products.rds')
database    <- readRDS('./files/database.rds')

# Remova duplicatas na coluna NO_PAIS_ING
# tb_country <- tb_country %>%
#   distinct(NO_PAIS_ING, .keep_all = TRUE)

#---------------------------------------------------------------------
# Funções Overview

# Valor das exportações
kpi_export_value <- function(tbl) {
  tbl$Valor |> sum(na.rm = TRUE)
}
 
kpi_export_value(database)

# Toneladas exportadas
kpi_export_weight <- function(tbl) {
  tbl$`Peso Líquido` |> sum(na.rm = TRUE) / 1000
}

kpi_export_weight(database)

# kg exportados
kpi_export_weight_kg <- function(tbl) {
  tbl$`Peso Líquido` |> sum(na.rm = TRUE)
}

kpi_export_weight_kg(database)

# Preço médio dos produtos
# kpi_average_price <- function(tbl) {
#   round(kpi_export_value(tbl) / kpi_export_weight_kg(tbl), 2)
# }
# 
# kpi_average_price(dados)  

database <- database %>%
  mutate(Ano = as.numeric(substr(
    `Trimestre/Ano`, nchar(`Trimestre/Ano`) - 3, nchar(`Trimestre/Ano`)
  )))

# Faturamento último ano
kpi_export_value_last_year <- function(tbl, year = 1) {
  tbl |>
    filter(Ano >= max(Ano) - year) |>
    kpi_export_value()
}

kpi_export_value_last_year(database)

# Exportação último ano
kpi_export_weight_last_year <- function(tbl, year = 1) {
  tbl |>
    filter(Ano >= max(Ano) - year) |>
    kpi_export_weight()
}

kpi_export_weight_last_year(database)

# Calcular o crescimento anual das exportações
growth_rate <- function(tbl) {
  tbl %>%
    group_by(Ano) %>%
    summarise(Total_Export_Value = sum(Valor, na.rm = TRUE)) %>%
    arrange(Ano) %>%
    mutate(Growth_Rate = (Total_Export_Value - lag(Total_Export_Value)) / lag(Total_Export_Value) * 100) %>%
    filter(!is.na(Growth_Rate)) %>%
    tail(1) %>%
    pull(Growth_Rate)
}

growth_rate2 <- function(tbl) {
  tbl %>%
    group_by(Ano) %>%
    summarise(Total_Export_Weight = sum(`Peso Líquido`, na.rm = TRUE)) %>%
    arrange(Ano) %>%
    mutate(Growth_Rate2 = (Total_Export_Weight - lag(Total_Export_Weight)) / lag(Total_Export_Weight) * 100) %>%
    filter(!is.na(Growth_Rate2)) %>%
    tail(1) %>%
    pull(Growth_Rate2)
}

growth_rate2(database)

# Função para formatar valores
format_value <- function(value) {
  if (value >= 1e12) {
    return(paste0(format(value / 1e12, digits = 3, nsmall = 2), " tri"))
  } else if (value >= 1e9) {
    return(paste0(format(value / 1e9, digits = 3, nsmall = 2), " bi"))
  } else if (value >= 1e6) {
    return(paste0(format(value / 1e6, digits = 3, nsmall = 2), " mi"))
  } else if (value >= 1e3) {
    return(paste0(format(value / 1e3, digits = 3, nsmall = 2), " mil"))
  } else {
    return(format(value, digits = 3, nsmall = 2))
  }
}

#------------------------------------------------------------------------------

# Preparar database de exportação por região
data_regioes <- database %>%
  group_by(Região, Ano) %>%
  summarise(
    Total_Export_Value = sum(Valor, na.rm = TRUE) / 1e6, # converter para milhões
    Total_Export_Weight = sum(`Peso Líquido`, na.rm = TRUE) / 1000
  )

# Preparar database de exportação por bloco econômico
data_blocos <- database %>%
  group_by(`Região Geográfica`, Ano) %>%
  summarise(
    Total_Export_Value = sum(Valor, na.rm = TRUE) / 1e6, # converter para milhões
    Total_Export_Weight = sum(`Peso Líquido`, na.rm = TRUE) / 1000
  )

#---------------------------------------------------------------------
# mapa

# Carregar shapefile dos países do mundo
world_sf <- read_sf("./maps/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world_sf <- select(world_sf, ISO3, UN, NAME, LON, LAT, geometry)


# Carregar shapefile dos estados do Brasil
brasil_shapefile <- read_sf("./maps/br_uf_shape_file/BR_UF_2022.shp")

# Unir database de exportação com tb_country para obter informações dos países
data_paises <- database %>%
  group_by(País) %>%
  summarise(Total_Export_Value = sum(Valor, na.rm = TRUE))

data_paises <- data_paises %>%
  left_join(tb_country, by = c("País" = "NO_PAIS"))




# Unir database de exportação com shapefile dos países do mundo
data_paises <- select(data_paises, "País", "CO_PAIS_ISON3", "CO_PAIS_ISOA3","Total_Export_Value", 
         "NO_BLOCO")

world_sf <- world_sf %>%
  left_join(data_paises, by = c("ISO3" = "CO_PAIS_ISOA3"))

# Separar database do Brasil dos database do resto do mundo
world_sf <- world_sf %>% filter(ISO3 != "BRA")

# Preparar database de exportação por estado
data_estados <- database %>%
  group_by(Estado) %>%
  summarise(Total_Export_Value = sum(Valor, na.rm = TRUE))

data_estados <- data_estados %>%
  left_join(tb_city, by = c("Estado" = "NO_UF"))

# Unir database de exportação com shapefile dos estados do Brasil
data_estados <- select(data_estados, "Estado", "SG_UF", "Total_Export_Value")

data_estados <- data_estados |>
  distinct()

brasil_shapefile <- brasil_shapefile %>%
  left_join(data_estados, by = c("SIGLA_UF" = "SG_UF"))



# Substituir valores faltantes por zero
# world_sf$Total_Export_Value[is.na(world_sf$Total_Export_Value)] <- 0

# Transformar valores para uma escala logarítmica
world_sf$Total_Export_Value <- log10(world_sf$Total_Export_Value + 1)
brasil_shapefile$log_Total_Export_Value <- log10(brasil_shapefile$Total_Export_Value + 1)

# Criar paleta de cores para os países
pal_world <- colorNumeric("Oranges", domain = world_sf$Total_Export_Value, na.color = "transparent")
pal_brazil <- colorNumeric("Greens", domain = brasil_shapefile$log_Total_Export_Value, na.color = "transparent")

#------------------------------------------------------------------------
# Preparar database para scatter plot e tabela de top 3 países exportadores
region_data <- select(database, "Trimestre/Ano", "Produto", "Estado", "Região", "País",
                       "Região Geográfica", "Valor", "Peso Líquido")

region_data <- region_data %>%
  mutate(Ano = as.numeric(sub(".* / ", "", `Trimestre/Ano`))) %>%
  select(-`Trimestre/Ano`)

region_data <- region_data %>%
  group_by(across(c(Produto, Estado, Região, País, `Região Geográfica`, Ano))) %>%
  summarise(Total_Valor = sum(`Valor`, na.rm = TRUE),
            Total_Peso = sum(`Peso Líquido`, na.rm = TRUE), 
            .groups = 'drop')

# Região norte
region_data_norte <- region_data |>
  filter(Região == "Norte")

# Região norte
region_data_nordeste <- region_data |>
  filter(Região == "Nordeste")

# Região norte
region_data_centro <- region_data |>
  filter(Região == "Centro Oeste")

# Região norte
region_data_sudeste <- region_data |>
  filter(Região == "Sudeste")

# Região norte
region_data_sul <- region_data |>
  filter(Região == "Sul")


  