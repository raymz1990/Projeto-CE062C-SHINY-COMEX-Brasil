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
  
  # Região Norte
  filtered_data <- reactive({
    data <- region_data_norte
    if (!("Todos" %in% input$product_select_norte)) {
      data <- data %>% filter(Produto %in% input$product_select_norte)
    }
    data <- data %>% filter(Ano == input$year_select_norte)
    data
  })
  
  output$sankey_plot_norte <- renderSankeyNetwork({
    data <- filtered_data()
    
    sankey_data <- data %>%
      group_by(Estado, Região.Geográfica) %>%
      summarise(Total_Valor = sum(Total_Valor), .groups = 'drop')
    
    nodes <- data.frame(
      name = unique(c(sankey_data$Estado, sankey_data$Região.Geográfica))
    )
    
    sankey_data$IDsource <- match(sankey_data$Estado, nodes$name) - 1
    sankey_data$IDtarget <- match(sankey_data$Região.Geográfica, nodes$name) - 1
    
    links <- data.frame(
      source = sankey_data$IDsource,
      target = sankey_data$IDtarget,
      value = sankey_data$Total_Valor / 1000  # Convertendo para milhares de reais
    )
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                  Value = "value", NodeID = "name", units = "Milhares de Reais", fontSize = 12, nodeWidth = 30)
  })
  
  output$top_countries_table_norte <- renderDataTable({
    data <- filtered_data() %>%
      group_by(Estado, País) %>%
      summarise(Total_Valor = sum(Total_Valor), Total_Peso = sum(Total_Peso), .groups = 'drop') %>%
      mutate(
        Valor_Formatado = case_when(
          Total_Valor >= 1e9 ~ paste0(round(Total_Valor / 1e9, 2), " Bilhões"),
          Total_Valor >= 1e6 ~ paste0(round(Total_Valor / 1e6, 2), " Milhões"),
          TRUE ~ paste0(round(Total_Valor / 1e3, 2), " Mil")
        )
      ) %>%
      arrange(desc(Total_Valor)) %>%
      group_by(Estado) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    datatable(
      data,
      options = list(pageLength = FALSE, server = TRUE),
      colnames = c(
        "Estado",
        "País",
        "Total Valor (R$)",
        "Total Peso (Toneladas)",
        "Valor Formatado"
      )
    )
  })
  
  # Região Nordeste
  filtered_data <- reactive({
    data <- region_data_nordeste
    if (!("Todos" %in% input$product_select_nordeste)) {
      data <- data %>% filter(Produto %in% input$product_select_nordeste)
    }
    data <- data %>% filter(Ano == input$year_select_nordeste)
    data
  })
  
  output$sankey_plot_nordeste <- renderSankeyNetwork({
    data <- filtered_data()
    
    sankey_data <- data %>%
      group_by(Estado, Região.Geográfica) %>%
      summarise(Total_Valor = sum(Total_Valor), .groups = 'drop')
    
    nodes <- data.frame(
      name = unique(c(sankey_data$Estado, sankey_data$Região.Geográfica))
    )
    
    sankey_data$IDsource <- match(sankey_data$Estado, nodes$name) - 1
    sankey_data$IDtarget <- match(sankey_data$Região.Geográfica, nodes$name) - 1
    
    links <- data.frame(
      source = sankey_data$IDsource,
      target = sankey_data$IDtarget,
      value = sankey_data$Total_Valor / 1000  # Convertendo para milhares de reais
    )
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                  Value = "value", NodeID = "name", units = "Milhares de Reais", fontSize = 12, nodeWidth = 30)
  })
  
  output$top_countries_table_nordeste <- renderDataTable({
    data <- filtered_data() %>%
      group_by(Estado, País) %>%
      summarise(Total_Valor = sum(Total_Valor), Total_Peso = sum(Total_Peso), .groups = 'drop') %>%
      mutate(
        Valor_Formatado = case_when(
          Total_Valor >= 1e9 ~ paste0(round(Total_Valor / 1e9, 2), " Bilhões"),
          Total_Valor >= 1e6 ~ paste0(round(Total_Valor / 1e6, 2), " Milhões"),
          TRUE ~ paste0(round(Total_Valor / 1e3, 2), " Mil")
        )
      ) %>%
      arrange(desc(Total_Valor)) %>%
      group_by(Estado) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    datatable(
      data,
      options = list(pageLength = FALSE, server = TRUE),
      colnames = c(
        "Estado",
        "País",
        "Total Valor (R$)",
        "Total Peso (Toneladas)",
        "Valor Formatado"
      )
    )
  })
  
  # Região Centro-Oeste
  filtered_data <- reactive({
    data <- region_data_centro
    if (!("Todos" %in% input$product_select_centro)) {
      data <- data %>% filter(Produto %in% input$product_select_centro)
    }
    data <- data %>% filter(Ano == input$year_select_centro)
    data
  })
  
  output$sankey_plot_centro <- renderSankeyNetwork({
    data <- filtered_data()
    
    sankey_data <- data %>%
      group_by(Estado, Região.Geográfica) %>%
      summarise(Total_Valor = sum(Total_Valor), .groups = 'drop')
    
    nodes <- data.frame(
      name = unique(c(sankey_data$Estado, sankey_data$Região.Geográfica))
    )
    
    sankey_data$IDsource <- match(sankey_data$Estado, nodes$name) - 1
    sankey_data$IDtarget <- match(sankey_data$Região.Geográfica, nodes$name) - 1
    
    links <- data.frame(
      source = sankey_data$IDsource,
      target = sankey_data$IDtarget,
      value = sankey_data$Total_Valor / 1000  # Convertendo para milhares de reais
    )
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                  Value = "value", NodeID = "name", units = "Milhares de Reais", fontSize = 12, nodeWidth = 30)
  })
  
  output$top_countries_table_centro <- renderDataTable({
    data <- filtered_data() %>%
      group_by(Estado, País) %>%
      summarise(Total_Valor = sum(Total_Valor), Total_Peso = sum(Total_Peso), .groups = 'drop') %>%
      mutate(
        Valor_Formatado = case_when(
          Total_Valor >= 1e9 ~ paste0(round(Total_Valor / 1e9, 2), " Bilhões"),
          Total_Valor >= 1e6 ~ paste0(round(Total_Valor / 1e6, 2), " Milhões"),
          TRUE ~ paste0(round(Total_Valor / 1e3, 2), " Mil")
        )
      ) %>%
      arrange(desc(Total_Valor)) %>%
      group_by(Estado) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    datatable(
      data,
      options = list(pageLength = FALSE, server = TRUE),
      colnames = c(
        "Estado",
        "País",
        "Total Valor (R$)",
        "Total Peso (Toneladas)",
        "Valor Formatado"
      )
    )
  })
  
  # Região Sudeste
  filtered_data <- reactive({
    data <- region_data_sudeste
    if (!("Todos" %in% input$product_select_sudeste)) {
      data <- data %>% filter(Produto %in% input$product_select_sudeste)
    }
    data <- data %>% filter(Ano == input$year_select_sudeste)
    data
  })
  
  output$sankey_plot_sudeste <- renderSankeyNetwork({
    data <- filtered_data()
    
    sankey_data <- data %>%
      group_by(Estado, Região.Geográfica) %>%
      summarise(Total_Valor = sum(Total_Valor), .groups = 'drop')
    
    nodes <- data.frame(
      name = unique(c(sankey_data$Estado, sankey_data$Região.Geográfica))
    )
    
    sankey_data$IDsource <- match(sankey_data$Estado, nodes$name) - 1
    sankey_data$IDtarget <- match(sankey_data$Região.Geográfica, nodes$name) - 1
    
    links <- data.frame(
      source = sankey_data$IDsource,
      target = sankey_data$IDtarget,
      value = sankey_data$Total_Valor / 1000  # Convertendo para milhares de reais
    )
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                  Value = "value", NodeID = "name", units = "Milhares de Reais", fontSize = 12, nodeWidth = 30)
  })
  
  output$top_countries_table_sudeste <- renderDataTable({
    data <- filtered_data() %>%
      group_by(Estado, País) %>%
      summarise(Total_Valor = sum(Total_Valor), Total_Peso = sum(Total_Peso), .groups = 'drop') %>%
      mutate(
        Valor_Formatado = case_when(
          Total_Valor >= 1e9 ~ paste0(round(Total_Valor / 1e9, 2), " Bilhões"),
          Total_Valor >= 1e6 ~ paste0(round(Total_Valor / 1e6, 2), " Milhões"),
          TRUE ~ paste0(round(Total_Valor / 1e3, 2), " Mil")
        )
      ) %>%
      arrange(desc(Total_Valor)) %>%
      group_by(Estado) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    datatable(
      data,
      options = list(pageLength = FALSE, server = TRUE),
      colnames = c(
        "Estado",
        "País",
        "Total Valor (R$)",
        "Total Peso (Toneladas)",
        "Valor Formatado"
      )
    )
  })
  
  
  # Região Sul
  filtered_data <- reactive({
    data <- region_data_sul
    if (!("Todos" %in% input$product_select_sul)) {
      data <- data %>% filter(Produto %in% input$product_select_sul)
    }
    data <- data %>% filter(Ano == input$year_select_sul)
    data
  })
  
  output$sankey_plot_sul <- renderSankeyNetwork({
    data <- filtered_data()
    
    sankey_data <- data %>%
      group_by(Estado, Região.Geográfica) %>%
      summarise(Total_Valor = sum(Total_Valor), .groups = 'drop')
    
    nodes <- data.frame(
      name = unique(c(sankey_data$Estado, sankey_data$Região.Geográfica))
    )
    
    sankey_data$IDsource <- match(sankey_data$Estado, nodes$name) - 1
    sankey_data$IDtarget <- match(sankey_data$Região.Geográfica, nodes$name) - 1
    
    links <- data.frame(
      source = sankey_data$IDsource,
      target = sankey_data$IDtarget,
      value = sankey_data$Total_Valor / 1000  # Convertendo para milhares de reais
    )
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                  Value = "value", NodeID = "name", units = "Milhares de Reais", fontSize = 12, nodeWidth = 30)
  })
  
  output$top_countries_table_sul <- renderDataTable({
    data <- filtered_data() %>%
      group_by(Estado, País) %>%
      summarise(Total_Valor = sum(Total_Valor), Total_Peso = sum(Total_Peso), .groups = 'drop') %>%
      mutate(
        Valor_Formatado = case_when(
          Total_Valor >= 1e9 ~ paste0(round(Total_Valor / 1e9, 2), " Bilhões"),
          Total_Valor >= 1e6 ~ paste0(round(Total_Valor / 1e6, 2), " Milhões"),
          TRUE ~ paste0(round(Total_Valor / 1e3, 2), " Mil")
        )
      ) %>%
      arrange(desc(Total_Valor)) %>%
      group_by(Estado) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    datatable(
      data,
      options = list(pageLength = FALSE, server = TRUE),
      colnames = c(
        "Estado",
        "País",
        "Total Valor (R$)",
        "Total Peso (Toneladas)",
        "Valor Formatado"
      )
    )
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
          autoWidth = TRUE,
          server = TRUE
        )
      )
  })
  
})