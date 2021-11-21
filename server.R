my_server <- function(input, output) {
  virusIcon <- makeIcon(
    iconUrl = "location.png",
    iconWidth = 40, iconHeight = 40
  )
  
  checkGlobalDate <- reactive({input$globaldate})
  checkCountries <- reactive({input$region})
  createGlobalActive <- reactive({
    df_active <- df_no_subregions
    
    df_active <- df_active %>%
      filter(date >= checkGlobalDate()[1] & date <= checkGlobalDate()[2])
    
    if (length(checkCountries()) != 0) {
      df_active <- df_active %>% filter(region %in% checkCountries())
    } else {
      df_active <- df_active
    }
  })
  
  output$globalsummary <- renderText({
    df_active <- createGlobalActive() %>% filter(date == max(date))
    
    paste(
      paste("On", format(max(df_active$date), format = "%B %d, %Y"), "in the selected country(s):\n"),
      paste0("\nThere were ", format(sum(df_active$confirmed, na.rm = T), big.mark = ","), " cumulative COVID-19 confirmed cases"),
      paste0("\nThere were ", format(sum(df_active$death, na.rm = T), big.mark = ","), " cumulative deaths caused by COVID-19"),
      paste0("\nThe average fatality rate was ", round(mean(parse_number(na.omit(df_active[["Fatality Rate"]]))), 2), "%")
    )
  })

  output$globalmap <- renderLeaflet({
    df_map <- createGlobalActive() %>% filter(date == max(date))
    
    if (length(checkCountries()) != 0) {
      df_map <- df_map %>% filter(region %in% checkCountries())
    } else {
      df_map <- df_map
    }

      leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      addMarkers(lng = df_map$Long, lat = df_map$Lat, 
                 clusterOptions = markerClusterOptions(),
                 popup = paste("Dependency: ", df_map$sub_region, "<br>",
                               "Country: ", df_map$region, "<br>",
                               "Confirmed Cases: ", format(df_map$confirmed, big.mark = ","), "<br>",
                               "Deaths: ", format(df_map$death, big.mark = ","), "<br>",
                               "As of ", format(df_map$date, format = "%b %d, %Y"), "<br>"),
                 icon = virusIcon) %>%
      setMaxBounds(lng1 = -200, lat1 = -90, lng2 = 200, lat2 = 90) %>%
      setView(lng = 0, lat = 35, zoom = 2)
  })
  
  output$globaltable <- renderDataTable({
    df_active <- createGlobalActive() %>%
      select(date, region, sub_region, confirmed, death, `Fatality Rate`) %>%
      rename(Date = date, Country = region, Dependency = sub_region, Confirmed = confirmed, Deaths = death) %>%
      arrange(Date)

    datatable(df_active,
              extensions = c('Buttons'),
              options = list(dom = 'Bfrtip', deferRender = TRUE, scrollY = 135, pageLength = 2000,
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
    ) %>% formatRound('Confirmed', digits = 0) %>%
      formatRound('Deaths', digits = 0)
  })
  
  output$confirmed_bar <- renderPlot({
    df_active <- createGlobalActive() %>% arrange(desc(confirmed)) %>% filter(date == max(date))
    
    barplot(
      df_active$confirmed[5:1] / 1000000,
      names.arg = df_active$region[5:1],
      main = paste0("Top 5 Countries by # of confirmed cases on ", format(max(df_active$date), format = "%B %d, %Y")),
      ylab = "# of confirmed cases (millions)",
      xlab = "Country",
      col = c("white", "beige", "yellow", "orange", "red")
    )
    
    axis(side = 2, tck = 1)
  })
  
  output$deaths_bar <- renderPlot({
    df_active <- createGlobalActive() %>% arrange(desc(death)) %>% filter(date == max(date))
    
    barplot(
      df_active$death[5:1] / 1000,
      names = df_active$region[5:1],
      main = paste0("Top 5 Countries by # of deaths on ", format(max(df_active$date), format = "%B %d, %Y")),
      ylab = "# of deaths (thousands)",
      xlab = "Country",
      col = c("white", "beige", "yellow", "orange", "red")
    )
    
    axis(side = 2, tck = 1)
  })
  
  checkNationalDate <- reactive({input$nationaldate})
  checkStates <- reactive({input$state})
  createNationalActive <- reactive({
    df_active <- df %>% filter(region == "US")
    
    df_active <- df_active %>%
      filter(date >= checkNationalDate()[1] & date <= checkNationalDate()[2])
    
    if (length(checkStates()) != 0) {
      df_active <- df_active %>% filter(sub_region %in% checkStates())
    } else {
      df_active <- df_active
    }
  })
  
  output$nationalsummary <- renderText({
    df_active <- createNationalActive() %>% na.omit(sub_region) %>% filter(date == max(date))
    
    paste(
      paste("On", format(max(df_active$date), format = "%B %d, %Y"), "in the selected territory/state(s):\n"),
      paste0("\nThere were ", format(sum(df_active["confirmed"], na.rm = T), big.mark = ","), " cumulative COVID-19 confirmed cases"),
      paste0("\nThere were ", format(sum(df_active["death"], na.rm = T), big.mark = ","), " cumulative deaths caused by COVID-19"),
      paste0("\nThe average fatality rate was ", round(mean(parse_number(na.omit(df_active[["Fatality Rate"]]))), 2), "%")
    )
  })

  output$nationalmap <- renderLeaflet({
    df_map <- df_state %>% filter(date == input$nationaldate[2])
    
    if (length(checkStates()) != 0) {
      df_map <- df_map %>% filter(state %in% checkStates())
    } else {
      df_map <- df_map
    }
    
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      addMarkers(lng = df_map$Long, lat = df_map$Lat, 
                 clusterOptions = markerClusterOptions(),
                 popup = paste("County:", df_map$county, "<br>",
                               "State/Territory:", df_map$state, "<br>",
                               "Confirmed Cases: ", format(df_map$confirmed, big.mark = ","), "<br>",
                               "Deaths: ", format(df_map$death, big.mark = ","), "<br>",
                               "As of ", format(df_map$date, format = "%b %d, %Y"), "<br>"),
                 icon = virusIcon) %>%
      setMaxBounds(lng1 = -270, lng2 = 0, lat1 = -90, lat2 = 90) %>%
      setView(lng = -175, lat = 45, zoom = 2)
  })
  
  output$nationaltable <- renderDataTable({
    df_active <- createNationalActive() %>%
      group_by(sub_region) %>%
      select(Date = date, "Territory/State" = sub_region, Confirmed = confirmed, Deaths = death, `Fatality Rate`) %>%
      arrange(Date)

    datatable(df_active,
              extensions = c('Buttons'),
              options = list(dom = 'Bfrtip', deferRender = TRUE, scrollY = 135,
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 2000)
    ) %>% formatRound('Confirmed', digits = 0) %>%
      formatRound('Deaths', digits = 0)
  })
  
  output$confirmed_plot <- renderPlot({
    df_active <- df %>% 
      filter(region == "US" & is.na(sub_region))
    
    df_active <- df_active %>% 
      filter(date <= checkNationalDate()[2] & date >= checkNationalDate()[1])
    
    plot(
      df_active$date,
      df_active$confirmed / 1000,
      main = "US confirmed cases",
      type = "n",
      xlab = paste0("Date range between ", format(min(df_active$date), format = "%b %d, %Y"), " and ", format(max(df_active$date), format = "%b %d, %Y")),
      ylab = "Confirmed cases (thousands)"
    )
    
    axis(side = 2, tck = 1)
    
    lines(
      df_active$date,
      df_active$confirmed / 1000,
      type = "o",
      col = "red"
    )
  })
  
  output$new_cases <- renderDataTable({
    df_first <- createNationalActive() %>%
      filter(date == min(date) & !(is.na(sub_region)))

    df_second <- createNationalActive() %>%
      filter(date == max(date) & !(is.na(sub_region)))

    df_active <- full_join(df_first, df_second, by = "sub_region") %>%
      mutate(new_con = confirmed.y - confirmed.x) %>%
      select("Territory/State" = sub_region, confirmed.x, confirmed.y, "New cases" = new_con, date.x, date.y)

    names(df_active)[2] <- paste("Cases from ", format(max(df_active$date.x), format = "%b %d, %Y"))
    names(df_active)[3] <- paste("Cases from ", format(max(df_active$date.y), format = "%b %d, %Y"))

    datatable(df_active[, 1:4],
              extensions = c('Scroller', 'Buttons'),
              options = list(dom = 'Bfrtip',
                             scroller = TRUE,
                             scrollY = 290,
                             deferRender = TRUE,
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
    ) %>% formatRound(paste("Cases from ", format(max(df_active$date.x), format = "%b %d, %Y")), digits = 0) %>%
      formatRound(paste("Cases from ", format(max(df_active$date.y), format = "%b %d, %Y")), digits = 0) %>%
      formatRound('New cases', digits = 0)
  })
}