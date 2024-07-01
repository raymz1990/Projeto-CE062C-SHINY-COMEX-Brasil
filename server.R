library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(sf)
library(plotly)
library(networkD3)

shinyServer(function(input, output) {
  
  output$KPI_VALUE <- renderUI({
    formatted_value <- format_value(kpi_export_value(dados))
    HTML(paste0('<span>', formatted_value, '</span>'))
  })
  
  output$KPI_WEIGHT <- renderUI({
    formatted_weight <- format_value(kpi_export_weight(dados))
    HTML(paste0('<span>', formatted_weight, '</span>'))
  })
  
  output$KPI_GROWTH <- renderUI({
    growth <- growth_rate(dados)
    formatted_growth <- paste0(format(growth, digits = 3, nsmall = 2), "%")
    HTML(paste0('<span>', formatted_growth, '</span>'))
  })
  
  output$KPI_GROWTH2 <- renderUI({
    growth2 <- growth_rate2(dados)
    formatted_growth2 <- paste0(format(growth2, digits = 3, nsmall = 2), "%")
    HTML(paste0('<span>', formatted_growth2, '</span>'))
  })
  
  # grafico linha exportações por região
  output$line_chart_region <- renderPlotly({
    req(input$measure_select)
    
    measure <- ifelse(input$measure_select == "value",
                      "Total_Export_Value",
                      "Total_Export_Weight")
    
    plot_ly(
      dados_regioes,
      x = ~ Year,
      y = as.formula(paste0("~", measure)),
      color = ~ NO_REGIAO,
      type = 'scatter',
      mode = 'lines',
      line = list(width = 4)
    ) %>%
      layout(
        yaxis = list(
          title = ifelse(
            input$measure_select == "value",
            "Exportações (em Milhões de Dólares)",
            "Peso (Toneladas)"
          )
        ),
        xaxis = list(title = "Ano"),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2
        )  # Legenda na parte inferior
      )
  })
  
  # grafico de linhas exportações por bloco economico
  output$line_chart_block <- renderPlotly({
    req(input$measure_select)
    
    measure <- ifelse(input$measure_select == "value",
                      "Total_Export_Value",
                      "Total_Export_Weight")
    
    plot_ly(
      dados_blocos,
      x = ~ Year,
      y = as.formula(paste0("~", measure)),
      color = ~ NO_BLOCO,
      type = 'scatter',
      mode = 'lines',
      line = list(width = 4)
    ) %>%
      layout(
        yaxis = list(
          title = ifelse(
            input$measure_select == "value",
            "Exportações (em Milhões de Dólares $)",
            "Peso (Toneladas)"
          )
        ),
        xaxis = list(title = "Ano"),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2
        )  # Legenda na parte inferior
      )
  })
  
  # Renderizar mapa coroplético
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 0, lng = -15.7801, zoom = 2) %>%
      addPolygons(
        data = brasil_shapefile,
        fillColor = ~pal_brazil(log_Total_Export_Value),
        fillOpacity = 1.0,
        stroke = FALSE) %>%
      addPolygons(
        data = world_sf,
        fillColor = ~pal_world(Total_Export_Value),
        fillOpacity = 0.9,
        stroke = FALSE
      )
  })
  
  # Função auxiliar para filtrar dados por região
  filter_region_data <- function(region) {
    region_data %>%
      filter(Região == region)
  }
  
  # Gráfico Sankey
  render_sankey <- function(region) {
    output[[paste0("sankey_plot_", region)]] <- renderSankeyNetwork({
      req(input$year_select)
      req(input$product_select)
      
      filtered_data <- filter_region_data(region) %>%
        filter(Ano == input$year_select, Produto %in% input$product_select | input$product_select == "Todos")
      
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
  
  render_table <- function(region) {
    output[[paste0("top_countries_table_", region)]] <- renderDataTable({
      req(input$table_year_select)
      
      filtered_data <- filter_region_data(region) %>%
        filter(Ano == input$table_year_select)
      
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
  
  # Datatable
  output$datatable <- renderDataTable({
      datatable(
        database,
        class = 'cell-border stripe',
        filter = 'top',
        editable = FALSE,
        options = list(
          pageLength = 50,
          searching = FALSE,
          autoWidth = TRUE
        )
      )
  })
  
})