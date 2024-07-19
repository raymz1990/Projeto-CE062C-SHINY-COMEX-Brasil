library(shiny)
library(bs4Dash)
library(leaflet)
library(plotly)
library(DT)
library(networkD3)

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
                 icon = icon("globe")
               ),
               menuSubItem(
                 text = "Região Nordeste",
                 tabName = "nordeste",
                 icon = icon("globe")
               ),
               menuSubItem(
                 text = "Região Centro-Oeste",
                 tabName = "centro-oeste",
                 icon = icon("globe")
               ),
               menuSubItem(
                 text = "Região Sudeste",
                 tabName = "sudeste",
                 icon = icon("globe")
               ),
               menuSubItem(
                 text = "Região Sul",
                 tabName = "sul",
                 icon = icon("globe")
               )
      ),
      # menuItem("Produtos", tabName = "PRODUCTS",
      #          icon = icon("shopping-cart")),
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
        h2("Comércio Internacional Brasileiro: Exportações de 2011 à 2020"),
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
        
      
      
      tabItem(
        tabName = "norte",
        h2("Exportações da Região Norte"),
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "product_select_norte",
              label = "Selecionar Produto:",
              choices = c("Todos", unique(region_data_norte$Produto), decreasing = TRUE),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "year_select_norte",
              label = "Selecionar Ano:", 
              choices = unique(region_data_norte$Ano),
              selected = max(region_data_norte$Ano)
            )
          )
        ),
        fluidRow(
          box(
            title = "Destinos das Exportações",
            width = 12,
            sankeyNetworkOutput("sankey_plot_norte")
          )
        ),
        fluidRow(
          box(
            title = "Top 5 Países Exportadores",
            width = 12,
            DT::dataTableOutput("top_countries_table_norte")
          )
        )
      ),
      
      tabItem(
        tabName = "nordeste",
        h2("Exportações da Região Nordeste"),
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "product_select_nordeste",
              label = "Selecionar Produto:",
              choices = c("Todos", unique(region_data_nordeste$Produto), decreasing = TRUE),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "year_select_nordeste",
              label = "Selecionar Ano:", 
              choices = unique(region_data_nordeste$Ano),
              selected = max(region_data_nordeste$Ano)
            )
          )
        ),
        fluidRow(
          box(
            title = "Destinos das Exportações",
            width = 12,
            sankeyNetworkOutput("sankey_plot_nordeste")
          )
        ),
        fluidRow(
          box(
            title = "Top 5 Países Exportadores",
            width = 12,
            DT::dataTableOutput("top_countries_table_nordeste")
          )
        )
      ),
      
      tabItem(
        tabName = "centro",
        h2("Exportações da Região Centro Oeste"),
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "product_select_centro",
              label = "Selecionar Produto:",
              choices = c("Todos", unique(region_data_centro$Produto), decreasing = TRUE),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "year_select_centro",
              label = "Selecionar Ano:", 
              choices = unique(region_data_centro$Ano),
              selected = max(region_data_centro$Ano)
            )
          )
        ),
        fluidRow(
          box(
            title = "Destinos das Exportações",
            width = 12,
            sankeyNetworkOutput("sankey_plot_centro")
          )
        ),
        fluidRow(
          box(
            title = "Top 5 Países Exportadores",
            width = 12,
            DT::dataTableOutput("top_countries_table_centro")
          )
        )
      ),
      
      tabItem(
        tabName = "sudeste",
        h2("Exportações da Região Sudeste"),
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "product_select_sudeste",
              label = "Selecionar Produto:",
              choices = c("Todos", unique(region_data_sudeste$Produto), decreasing = TRUE),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "year_select_sudeste",
              label = "Selecionar Ano:", 
              choices = unique(region_data_sudeste$Ano),
              selected = max(region_data_sudeste$Ano)
            )
          )
        ),
        fluidRow(
          box(
            title = "Destinos das Exportações",
            width = 12,
            sankeyNetworkOutput("sankey_plot_sudeste")
          )
        ),
        fluidRow(
          box(
            title = "Top 5 Países Exportadores",
            width = 12,
            DT::dataTableOutput("top_countries_table_sudeste")
          )
        )
      ),
      
      tabItem(
        tabName = "sul",
        h2("Exportações da Região Sul"),
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "product_select_sul",
              label = "Selecionar Produto:",
              choices = c("Todos", unique(region_data_sul$Produto), decreasing = TRUE),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "year_select_sul",
              label = "Selecionar Ano:", 
              choices = unique(region_data_sul$Ano),
              selected = max(region_data_sul$Ano)
            )
          )
        ),
        fluidRow(
          box(
            title = "Destinos das Exportações",
            width = 12,
            sankeyNetworkOutput("sankey_plot_sul")
          )
        ),
        fluidRow(
          box(
            title = "Top 5 Países Exportadores",
            width = 12,
            DT::dataTableOutput("top_countries_table_sul")
          )
        )
      ),
        # tabItem(
        #   tabName = "PRODUCTS",
        #   h2("Produtos"),
        #   box(
        #     title = "Box 3",
        #     "Content 3"
        #   )
        # ),
        tabItem(
          tabName = "DATABASE",
          fluidRow(
            box(
              width = 12,
              title = "Base de Dados Exportações Brasil - 2011 - 2020",
              status = "info", solidHeader = TRUE,
              DT::dataTableOutput("datatable")
            )
          )
        )
      )
    )
)
  


shinyUI(ui)