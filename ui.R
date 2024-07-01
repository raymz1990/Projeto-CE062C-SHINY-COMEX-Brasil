library(shiny)
library(bs4Dash)
library(leaflet)
library(plotly)
library(DT)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Painel de Navegação",
      color = "primary",
      href = "https://www.gov.br/mdic/pt-br",
      image = "https://www.gov.br/mdic/++theme++padrao_govbr/favicons/favicon-48x48.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Geral",
               tabName = "OVERVIEW",
               icon = icon("dashboard")),
      menuItem("Por Região",
               tabName = "REGION",
               icon = icon("flag"),
               startExpanded = FALSE,
               menuSubItem(
                 text = "Região Norte",
                 tabName = "norte",
                 icon = icon("circle")
               ),
               menuSubItem(
                 text = "Região Nordeste",
                 tabName = "nordeste",
                 icon = icon("circle")
               ),
               menuSubItem(
                 text = "Região Centro-Oeste",
                 tabName = "centro-oeste",
                 icon = icon("circle")
               ),
               menuSubItem(
                 text = "Região Sudeste",
                 tabName = "sudeste",
                 icon = icon("circle")
               ),
               menuSubItem(
                 text = "Região Sul",
                 tabName = "sul",
                 icon = icon("circle")
               )
      ),
      menuItem("Produtos", tabName = "PRODUCTS",
               icon = icon("shopping-cart")),
      menuItem("Base de Dados", tabName = "DATABASE",
               icon = icon("database"))
    )
  ),
  
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        .info-box-title {
          font-size: 12px;  /* Diminui o tamanho do título */
        }
        .info-box-number {
          font-size: 160%;  /* Altera o tamanho do valor */
        }
        
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "OVERVIEW",
        h2("Comércio Internacional Brasileiro: Uma Análise das Exportações de 2011 à 2020"),
        fluidRow(
          valueBox(
            subtitle = "Exportações (em $)",
            color = "success",
            icon = icon("money-bill"),
            value = tags$div(class = "info-box-number", htmlOutput("KPI_VALUE")),
            elevation = 4
          ),
          
          valueBox(
            subtitle = "Crescimento Anual",
            color = "warning",
            icon = icon("chart-line"),
            value = tags$div(class = "info-box-number", htmlOutput("KPI_GROWTH")),
            elevation = 4
          ),
          
          valueBox(
            subtitle = "Toneladas Exportadas",
            color = "info",
            icon = icon("scale-balanced"),
            value = tags$div(class = "info-box-number", htmlOutput("KPI_WEIGHT")),
            elevation = 4
          ),
          valueBox(
            subtitle = "Crescimento Anual",
            color = "gray",
            icon = icon("chart-line"),
            value = tags$div(class = "info-box-number", htmlOutput("KPI_GROWTH2")),
            elevation = 4
          )
          
        ),
        fluidRow(
          tabBox(
            title = "Gráfico de Exportações",
            id = "tabset",
            width = 12,
            tabPanel(
              "por Região de Estado",
              value = "region",
              fluidRow(
                radioButtons(
                  "measure_select",
                  "Medida:",
                  choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                  selected = "value",
                  inline = TRUE
                )
              ),
              plotlyOutput("line_chart_region")
            ),
            tabPanel(
              "por Região Geográfica no Mundo",
              value = "economic_block",
              fluidRow(
                radioButtons(
                  "measure_select",
                  "Medida:",
                  choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                  selected = "value",
                  inline = TRUE
                )
              ),
              plotlyOutput("line_chart_block")
            )
          )
        ), 
        fluidRow(
          box(
            title = "Mapa de Exportações",
            width = 12,
            leafletOutput("map", height = 800)  # Saída do mapa
          )
        )
      ),
        
      
      
      
      tabItems(
        tabItem(
          tabName = "REGION"
        ),
        tabItem(
          tabName = "norte",
          h2("Região Norte"),
          fluidRow(
            column(
              width = 4,
              selectInput("product_select", "Selecionar Produto:",
                          choices = c("Todos", unique(region_data$Produto)),
                          selected = "Todos")
            ),
            column(
              width = 4,
              sliderInput("year_select", "Selecionar Ano:", 
                          min = min(region_data$Ano),
                          max = max(region_data$Ano),
                          value = max(region_data$Ano),
                          step = 1)
            ),
            column(
              width = 4,
              radioButtons(
                "measure_select",
                "Medida:",
                choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                selected = "value",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            box(
              title = "Gráfico Sankey",
              width = 12,
              sankeyNetworkOutput("sankey_plot")
            )
          ),
          fluidRow(
            box(
              title = "Top 3 Países Exportadores",
              width = 12,
              dataTableOutput("top_countries_table")
            )
          ),
          fluidRow(
            column(
              width = 12,
              tabBox(
                id = "norte_tabs",
                tabPanel("Gráfico Sankey", sankeyNetworkOutput("sankey_plot_norte")),
                tabPanel("Top 3 Países", dataTableOutput("top_countries_table_norte"))
              )
            )
          )
        ),,
        tabItem(
          tabName = "nordeste",
          h2("Região Nordeste"),
          fluidRow(
            column(
              width = 4,
              selectInput("product_select_nordeste", "Selecionar Produto:",
                          choices = c("Todos", unique(region_data$Produto)),
                          selected = "Todos")
            ),
            column(
              width = 4,
              sliderInput("year_select_nordeste", "Selecionar Ano:", 
                          min = min(region_data$Ano),
                          max = max(region_data$Ano),
                          value = max(region_data$Ano),
                          step = 1)
            ),
            column(
              width = 4,
              radioButtons(
                "measure_select_nordeste",
                "Medida:",
                choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                selected = "value",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            box(
              title = "Gráfico Sankey",
              width = 12,
              sankeyNetworkOutput("sankey_plot_nordeste")
            )
          ),
          fluidRow(
            box(
              title = "Top 3 Países Exportadores",
              width = 12,
              dataTableOutput("top_countries_table_nordeste")
            )
          )
        ),
        tabItem(
          tabName = "centro_oeste",
          h2("Região Centro-Oeste"),
          fluidRow(
            column(
              width = 4,
              selectInput("product_select_centro_oeste", "Selecionar Produto:",
                          choices = c("Todos", unique(region_data$Produto)),
                          selected = "Todos")
            ),
            column(
              width = 4,
              sliderInput("year_select_centro_oeste", "Selecionar Ano:", 
                          min = min(region_data$Ano),
                          max = max(region_data$Ano),
                          value = max(region_data$Ano),
                          step = 1)
            ),
            column(
              width = 4,
              radioButtons(
                "measure_select_centro_oeste",
                "Medida:",
                choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                selected = "value",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            box(
              title = "Gráfico Sankey",
              width = 12,
              sankeyNetworkOutput("sankey_plot_centro_oeste")
            )
          ),
          fluidRow(
            box(
              title = "Top 3 Países Exportadores",
              width = 12,
              dataTableOutput("top_countries_table_centro_oeste")
            )
          )
        ),
        tabItem(
          tabName = "sudeste",
          h2("Região Sudeste"),
          fluidRow(
            column(
              width = 4,
              selectInput("product_select_sudeste", "Selecionar Produto:",
                          choices = c("Todos", unique(region_data$Produto)),
                          selected = "Todos")
            ),
            column(
              width = 4,
              sliderInput("year_select_sudeste", "Selecionar Ano:", 
                          min = min(region_data$Ano),
                          max = max(region_data$Ano),
                          value = max(region_data$Ano),
                          step = 1)
            ),
            column(
              width = 4,
              radioButtons(
                "measure_select_sudeste",
                "Medida:",
                choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                selected = "value",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            box(
              title = "Gráfico Sankey",
              width = 12,
              sankeyNetworkOutput("sankey_plot_sudeste")
            )
          ),
          fluidRow(
            box(
              title = "Top 3 Países Exportadores",
              width = 12,
              dataTableOutput("top_countries_table_sudeste")
            )
          )
        ),
        tabItem(
          tabName = "sul",
          h2("Região Sul"),
          fluidRow(
            column(
              width = 4,
              selectInput("product_select_sul", "Selecionar Produto:",
                          choices = c("Todos", unique(region_data$Produto)),
                          selected = "Todos")
            ),
            column(
              width = 4,
              sliderInput("year_select_sul", "Selecionar Ano:", 
                          min = min(region_data$Ano),
                          max = max(region_data$Ano),
                          value = max(region_data$Ano),
                          step = 1)
            ),
            column(
              width = 4,
              radioButtons(
                "measure_select_sul",
                "Medida:",
                choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                selected = "value",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            box(
              title = "Gráfico Sankey",
              width = 12,
              sankeyNetworkOutput("sankey_plot_sul")
            )
          ),
          fluidRow(
            box(
              title = "Top 3 Países Exportadores",
              width = 12,
              dataTableOutput("top_countries_table_sul")
            )
          )
        ),
        tabItem(
          tabName = "PRODUCTS",
          h2("Produtos"),
          box(
            title = "Box 3",
            "Content 3"
          )
        ),
        tabItem(
          tabName = "DATABASE",
          box(
            width = 12,
            dataTableOutput("datatable")
          )
        )
      )
    )
  )
)

shinyUI(ui)