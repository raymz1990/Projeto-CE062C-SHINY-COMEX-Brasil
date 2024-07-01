#---------------------------------------------------------------------
# Pacotes

library(tidyverse)
library(sf)
library(leaflet)
library(stringr)

#---------------------------------------------------------------------
# Tabelas

# dados <- read.csv('./data/dados.csv')
# tb_city <- read.csv('./data/tb_city.csv')
# tb_country <- read.csv('./data/tb_country.csv')
# tb_products <- read.csv('./data/tb_products.csv')
# database <- read.csv('./data/database.csv')

temp_dir <- tempdir()                         # Cria um diretório temporário
unzip("./data/data.zip", exdir = temp_dir)

# Listar arquivos extraídos (opcional, para verificação)
arquivos_extraidos <- list.files(temp_dir, full.names = TRUE)

# Exemplo de leitura de um arquivo .csv extraído
dados       <- read.csv(file.path(temp_dir, "dados.csv"))
tb_city     <- read.csv(file.path(temp_dir, "tb_city.csv"))
tb_country  <- read.csv(file.path(temp_dir, "tb_country.csv"))
tb_products <- read.csv(file.path(temp_dir, "tb_products.csv"))
database    <- read.csv(file.path(temp_dir, "database.csv"))

# Limpar diretório temporário após o uso (opcional, dependendo da necessidade)
unlink(temp_dir, recursive = TRUE)

#---------------------------------------------------------------------
# Funções Overview

# Valor das exportações
kpi_export_value <- function(tbl) {
  tbl$US_Valor |> sum(na.rm = TRUE)
}
 
kpi_export_value(dados)

# Toneladas exportadas
kpi_export_weight <- function(tbl) {
  tbl$Peso_Liquido |> sum(na.rm = TRUE) / 1000
}

kpi_export_weight(dados)

# kg exportados
kpi_export_weight_kg <- function(tbl) {
  tbl$Peso_Liquido |> sum(na.rm = TRUE)
}

kpi_export_weight_kg(dados)

# Preço médio dos produtos
# kpi_average_price <- function(tbl) {
#   round(kpi_export_value(tbl) / kpi_export_weight_kg(tbl), 2)
# }
# 
# kpi_average_price(dados)  

# Faturamento último ano
kpi_export_value_last_year <- function(tbl, year = 1) {
  tbl |>
    filter(Year >= max(Year) - year) |>
    kpi_export_value()
}

kpi_export_value_last_year(dados)

# Exportação último ano
kpi_export_weight_last_year <- function(tbl, year = 1) {
  tbl |>
    filter(Year >= max(Year) - year) |>
    kpi_export_weight()
}

kpi_export_weight_last_year(dados)

# Calcular o crescimento anual das exportações
growth_rate <- function(tbl) {
  tbl %>%
    group_by(Year) %>%
    summarise(Total_Export_Value = sum(US_Valor, na.rm = TRUE)) %>%
    arrange(Year) %>%
    mutate(Growth_Rate = (Total_Export_Value - lag(Total_Export_Value)) / lag(Total_Export_Value) * 100) %>%
    filter(!is.na(Growth_Rate)) %>%
    tail(1) %>%
    pull(Growth_Rate)
}

growth_rate2 <- function(tbl) {
  tbl %>%
    group_by(Year) %>%
    summarise(Total_Export_Weight = sum(Peso_Liquido, na.rm = TRUE)) %>%
    arrange(Year) %>%
    mutate(Growth_Rate2 = (Total_Export_Weight - lag(Total_Export_Weight)) / lag(Total_Export_Weight) * 100) %>%
    filter(!is.na(Growth_Rate2)) %>%
    tail(1) %>%
    pull(Growth_Rate2)
}

growth_rate2(dados)

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

# Preparar dados de exportação por região
dados_regioes <- dados %>%
  left_join(tb_city, by = "City") %>%
  group_by(NO_REGIAO, Year) %>%
  summarise(
    Total_Export_Value = sum(US_Valor, na.rm = TRUE) / 1e6, # converter para milhões
    Total_Export_Weight = sum(Peso_Liquido, na.rm = TRUE) / 1000
  ) %>%
  mutate(NO_REGIAO = str_replace_all(NO_REGIAO, "REGIAO ", "")) %>%
  mutate(NO_REGIAO = str_to_title(NO_REGIAO))

# Preparar dados de exportação por bloco econômico
dados_blocos <- dados %>%
  left_join(tb_country, by = c("Country" = "NO_PAIS_ING")) %>%
  group_by(NO_BLOCO, Year) %>%
  summarise(
    Total_Export_Value = sum(US_Valor, na.rm = TRUE) / 1e6, # converter para milhões
    Total_Export_Weight = sum(Peso_Liquido, na.rm = TRUE) / 1000
  )

#---------------------------------------------------------------------
# mapa

# Carregar shapefile dos países do mundo
world_sf <- read_sf("./maps/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world_sf <- select(world_sf, ISO3, UN, NAME, LON, LAT, geometry)


# Carregar shapefile dos estados do Brasil
brasil_shapefile <- read_sf("./maps/br_uf_shape_file/BR_UF_2022.shp")

# Unir dados de exportação com tb_country para obter informações dos países
dados_paises <- dados %>%
  group_by(Country) %>%
  summarise(Total_Export_Value = sum(US_Valor, na.rm = TRUE))

dados_paises <- dados_paises %>%
  left_join(tb_country, by = c("Country" = "NO_PAIS_ING"))


# Unir dados de exportação com shapefile dos países do mundo
dados_paises <- select(dados_paises, "Country", "CO_PAIS_ISON3", "CO_PAIS_ISOA3","Total_Export_Value", 
         "NO_BLOCO", "NO_PAIS")

world_sf <- world_sf %>%
  left_join(dados_paises, by = c("ISO3" = "CO_PAIS_ISOA3"))

# Separar dados do Brasil dos dados do resto do mundo
world_sf <- world_sf %>% filter(ISO3 != "BRA")

# Preparar dados de exportação por estado
dados_estados <- dados %>%
  left_join(tb_city, by = "City") %>%
  group_by(SG_UF) %>%
  summarise(Total_Export_Value = sum(US_Valor, na.rm = TRUE))

# Unir dados de exportação com shapefile dos estados do Brasil
brasil_shapefile <- brasil_shapefile %>%
  left_join(dados_estados, by = c("SIGLA_UF" = "SG_UF"))

# Substituir valores faltantes por zero
# world_sf$Total_Export_Value[is.na(world_sf$Total_Export_Value)] <- 0

# Transformar valores para uma escala logarítmica
world_sf$Total_Export_Value <- log10(world_sf$Total_Export_Value + 1)
brasil_shapefile$log_Total_Export_Value <- log10(brasil_shapefile$Total_Export_Value + 1)

# Criar paleta de cores para os países
pal_world <- colorNumeric("Oranges", domain = world_sf$Total_Export_Value, na.color = "transparent")
pal_brazil <- colorNumeric("Greens", domain = brasil_shapefile$log_Total_Export_Value, na.color = "transparent")

#------------------------------------------------------------------------
# Preparar dados para scatter plot e tabela de top 3 países exportadores
region_data <- select(database, "Mês.Ano", "Produto", "Estado", "Região", "País",
                       "Região.Geográfica", "Valor....", "Peso.Líquido")

region_data <- region_data %>%
  mutate(Ano = as.numeric(sub(".* / ", "", Mês.Ano))) %>%
  select(-Mês.Ano)

region_data <- region_data %>%
  group_by(across(c(Produto, Estado, Região, País, Região.Geográfica, Ano))) %>%
  summarise(Total_Valor = sum(`Valor....`, na.rm = TRUE),
            Total_Peso = sum(`Peso.Líquido`, na.rm = TRUE), 
            .groups = 'drop')

# Função auxiliar para filtrar dados por região
filter_region_data <- function(region) {
  region_data %>%
    filter(Região == region)
}

# Gráfico Sankey
render_sankey <- function(region) {
  output[[paste0("sankey_plot_", region)]] <- renderSankeyNetwork({
    req(input[[paste0("year_select_", region)]])
    req(input[[paste0("product_select_", region)]])
    
    filtered_data <- filter_region_data(region) %>%
      filter(Ano == input[[paste0("year_select_", region)]], Produto %in% input[[paste0("product_select_", region)]] | input[[paste0("product_select_", region)]] == "Todos")
    
    nodes <- data.frame(
      name = unique(c(filtered_data$Estado, filtered_data$País))
    )
    
    links <- filtered_data %>%
      group_by(Estado, País) %>%
      summarise(value = sum(Total_Valor, na.rm = TRUE)) %>%
      left_join(nodes, by = c("Estado" = "name")) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("País" = "name")) %>%
      rename(target = id)
    
    sankeyNetwork(
      Links = links, Nodes = nodes,
      Source = "source", Target = "target",
      Value = "value", NodeID = "name",
      sinksRight = FALSE
    )
  })
}

# Tabela de top países exportadores
render_table <- function(region) {
  output[[paste0("top_countries_table_", region)]] <- renderDataTable({
    req(input[[paste0("year_select_", region)]])
    
    filtered_data <- filter_region_data(region) %>%
      filter(Ano == input[[paste0("year_select_", region)]])
    
    top_countries <- filtered_data %>%
      group_by(Estado, País) %>%
      summarise(Total_Valor = sum(Total_Valor, na.rm = TRUE)) %>%
      arrange(desc(Total_Valor)) %>%
      group_by(Estado) %>%
      slice_max(Total_Valor, n = 3) %>%
      ungroup()
    
    datatable(top_countries, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Renderizar gráficos e tabelas para cada região
regions <- c("norte", "nordeste", "centro_oeste", "sudeste", "sul")

lapply(regions, function(region) {
  render_sankey(region)
  render_table(region)
})