## (c) Keisuke Kondo
## Date (First Version): 2020-05-05
## Date (Latest Version): 2020-12-20
## 
## - global.R
## - server.R
## - ui.R
## 

server <- function(input, output, session){
  ####################################
  ## VISUALIZE SIMULATION
  ## leaflet
  ## - output$covid19mapSimulated
  ## - output$odflowmapLine
  ## 
  ## MAKE FIGURES USING SIMULATION RESULTS
  ## Higherchart
  ## - linePlot for national total
  ## - linePlot for 47 prefectures
  ## - minilinePlot for national total
  ## - minilinePlot for 47 prefectures
  ##
  ## MAKE FIGURES USING DATA FROM GLOBAL.R
  ## leaflet
  ## - output$covid19mapObserbed
  ##
  ## Higherchart
  ## - barPlot for national total
  ## - barPlot for 47 prefectures
  ####################################
  
  #++++++++++++++++++++++++++++++++++++++
  #Radio Button
  observeEvent(input$radioCase, {
    idx <- as.integer(input$radioCase)
    output$textCase <- renderText({
      as.character(listTextR0[[idx]])
    })
  })
  
  #++++++++++++++++++++++++++++++++++++++
  #Reset Button
  observeEvent(input$buttonReset, {
    session$reload()
  })
  
  ## ++++++++++++++++++++++++++++++++++++++++++
  ## VISUALIZE SIMULATION RESULTS
  ## ++++++++++++++++++++++++++++++++++++++++++
  observeEvent(input$buttonSimulation, {
    #Parameter Setting
    # - R0: Basic reproductive ratio
    # - l_gamma: Average days for the infectious state
    # - l_epsilon: Average days for the exposed state
    # - beta: infection rate
    # - gamma: recovery rate
    # - epsilon: incubation rate

    #UPDATE DataFrame
    idx <- as.integer(input$radioCase)
    dateStartSimulation <- listDateStartSimulation[[idx]]
    dfResults1Long <- listResult1[[idx]] %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::mutate(Alpha = Beta/beta0) %>%
      dplyr::left_join(dfInfo, by = c("date", "prefCode"))
    dfResults2Long <- listResult2[[idx]] %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::mutate(Alpha = Beta/beta0) %>%
      dplyr::left_join(dfInfo, by = c("date", "prefCode"))
    
    ## ++++++++++++++++++++++++++++++++++++++++++
    ## END DO SIMULATION
    ## ++++++++++++++++++++++++++++++++++++++++++
    
    ## ++++++++++++++++++++++++++++++++++++++++++
    ## MAKE FIGURES USING SIMULATION RESULTS
    ## ++++++++++++++++++++++++++++++++++++++++++
    #++++++++++++++++++++++++++++++++++++++
    #covid19mapSimulated
    output$cavid19mapSimulated <- renderLeaflet({
      
      #Load Data
      dfTemp1 <- dfResults1Long %>%
        dplyr::filter(date == input$dateSimulationMap)
      #Make ShapeFile
      sfTemp1 <- sfPref %>%
        dplyr::left_join(dfTemp1, by = "prefCode")
      
      #Make Map
      leaflet(sfTemp1) %>%
        #Center Position
        setView(lat = 35.721370,
                lng = 146.204871,
                zoom = 5) %>%
        #Tile Layer
        addMapboxGL(
          accessToken = accessToken,
          style = styleUrlBasic,
          setView = FALSE
        ) %>%
        #Polygon Layer
        addPolygons(
          fillOpacity = 0,
          stroke = TRUE,
          weight = 0.2,
          color = "gray",
          layerId = ~ prefCode,
          label = ~ labelPref,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        #Minicharts Layer
        addMinicharts(
          lng = sfTemp1$lon,
          lat = sfTemp1$lat,
          chartdata = round(sfTemp1$I),
          showLabels = TRUE,
          labelMinSize = 8,
          labelMaxSize = 56,
          width = 64,
          popup = popupArgs(labels = paste("Number of Infectious Individuals"))
        )
    })
    
    #++++++++++++++++++++++++++++++++++++++
    #Datatable Download
    output$downloadDataSim1 <- downloadHandler(
      filename = function() {
        paste("simulation_data_from_spatial_seir_model_with_mobility_case",
              idx,
              "_",
              Sys.Date(),
              ".csv",
              sep = "")
      },
      content = function(con) {
        dfTemp <- dfResults1Long %>%
          dplyr::filter(date <= input$dateRangeEndSimulation)
        readr::write_csv(dfTemp, con)
      }
    )
    
    #++++++++++++++++++++++++++++++++++++++
    #Datatable Download
    output$downloadDataSim2 <- downloadHandler(
      filename = function() {
        paste("simulation_data_from_spatial_seir_model_without_mobility_case",
              idx,
              "_",
              Sys.Date(),
              ".csv",
              sep = "")
      },
      content = function(con) {
        dfTemp <- dfResults2Long %>%
          dplyr::filter(date <= input$dateRangeEndSimulation)
        readr::write_csv(dfTemp, con)
      }
    )

    #++++++++++++++++++++++++++++++++++++++
    #linePlot using simulated data in tabPanel Time-Series
    inputDateRangeStartSimulation <- dateStartSimulation
    inputDateRangeEndSimulation <- reactive(input$dateRangeEndSimulation)
    inputTypeOfVarSimulation <- reactive(input$typeOfVarSimulation)
    inputCase <- as.integer(input$radioCase)
    
    #National Total and Each Prefecture
    observeEvent(input$prefCodeLinePlotSim, {
      callModule(
        prefLinePlot,
        "prefLinePlotEach",
        dfResults1Long,
        dfResults2Long,
        inputCase,
        as.integer(stringr::str_sub(input$prefCodeLinePlotSim, 1, 2)),
        inputTypeOfVarSimulation,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation
      )
    })
    
    #++++++++++++++++++++++++++++++++++++++
    #Lineplot using simulated data in tabPanel Spatial Distribution
    inputDateRangeStartSimulation <- dateStartSimulation
    inputDateRangeEndSimulation <- reactive(input$dateRangeEndSimulation)
    inputDateSimulationMap <- reactive(input$dateSimulationMap)
    
    #National Total and Each Prefecture
    observeEvent(input$prefCodeCovid19mapSim, {
      callModule(
        prefMiniLinePlot,
        "prefMiniLinePlotEach",
        dfResults1Long,
        as.integer(stringr::str_sub(input$prefCodeCovid19mapSim, 1, 2)),
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputDateSimulationMap
      )
    }, ignoreNULL = FALSE)
    ## ++++++++++++++++++++++++++++++++++++++++++
    ## END MAKE FIGURES USING SIMULATION RESULTS
    ## ++++++++++++++++++++++++++++++++++++++++++
    
    ###############################
    #buttonSimulation
  }, ignoreNULL = FALSE)
  

  ## ++++++++++++++++++++++++++++++++++++++++++
  ## FIGURES USING DATA FROM GLOBAL.R
  ## ++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++
  #Barplot using observed data
  inputTypeOfVar <- reactive(input$typeOfVar)
  inputDateCovidMap <- reactive(input$dateCovidMap)
  
  #National Total and Each Prefecture
  observeEvent(input$prefCodeCovid19map, {
    callModule(prefBarPlot,
               "prefBarPlotEach",
               as.integer(stringr::str_sub(input$prefCodeCovid19map, 1, 2)),
               inputTypeOfVar,
               inputDateCovidMap)
  }, ignoreNULL = FALSE)

  #++++++++++++++++++++++++++++++++++++++
  #covid19mapObserbed
  output$covid19mapObserbed <- renderLeaflet({
    
    #Load Data
    dfTemp <- dfCovidPanel %>%
      dplyr::filter(date == input$dateCovidMap)
    #Make ShapeFile
    sfTemp <- sfPref %>%
      dplyr::left_join(dfTemp, by = "prefCode")
    
    #Choose New or Cumulative
    if (input$typeOfVar == 1) {
      sfTemp <- sfTemp %>%
        dplyr::mutate(y = newPositive)
      labelText <- "Daily number of new patients tested positive"
    }
    if (input$typeOfVar == 2) {
      sfTemp <- sfTemp %>%
        dplyr::mutate(y = cumPositive)
      labelText <- "Cumulative number of patients tested positive"
    }
    
    #Make Map
    leaflet(sfTemp) %>%
      #Center Position
      setView(lat = 35.721370,
              lng = 146.204871,
              zoom = 5) %>%
      #Tile Layer
      addMapboxGL(
        accessToken = accessToken,
        style = styleUrlBasic,
        setView = FALSE
      ) %>%
      #Polygon Layer
      addPolygons(
        fillOpacity = 0,
        stroke = TRUE,
        color = "gray",
        weight = 0.2,
        layerId = ~ prefCode,
        label = ~ labelPref,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      #Minicharts Layer
      addMinicharts(
        lng = sfTemp$lon,
        lat = sfTemp$lat,
        chartdata = sfTemp$y,
        showLabels = TRUE,
        labelMinSize = 8,
        labelMaxSize = 56,
        width = 64,
        popup = popupArgs(labels = labelText)
      )
  })  
  
  
  #++++++++++++++++++++++++++++++++++++++
  #odflowmapLine
  output$odflowmapLine <- renderLeaflet({
    
    #Outflow from Prefecture
    sfTemp1 <- sfFlow %>%
      dplyr::filter(
        month == as.integer(which(
          month.name == as.character(input$periodOfMonth)
        )) &
          periodOfDay == as.integer(input$periodOfDay) &
          prefCodeOrigin == as.integer(stringr::str_sub(input$prefCodeFlow, 1, 2))
      ) %>%
      dplyr::filter(prefCodeOrigin != prefCodeDestination)
    #Inflow into Prefecture
    sfTemp2 <- sfFlow %>%
      dplyr::filter(
        month == as.integer(which(
          month.name == as.character(input$periodOfMonth)
        )) &
          periodOfDay == as.integer(input$periodOfDay) &
          prefCodeDestination == as.integer(stringr::str_sub(input$prefCodeFlow, 1, 2))
      ) %>%
      dplyr::filter(prefCodeOrigin != prefCodeDestination)
    
    #tmap by type of flow
    if (input$typeOfFlow == 1) {
      #Layer Order
      sfTemp1 <- sfTemp1 %>% dplyr::arrange(flow)
      sfTemp2 <- sfTemp2 %>% dplyr::arrange(flow)
      #Make Map
      tmap_mode("view")
      tp <- tm_shape(sfPref) +
        tm_basemap(NULL) +
        tm_borders(lwd = 0.3) +
        tm_shape(sfTemp1) +
        tm_lines(
          col = "flow",
          lwd = "flow",
          scale = 20,
          palette = "YlOrRd",
          id = "labelId",
          title.col = paste("Outflow from Prefecture", "(persons)", sep = "<br>"),
          n = 8,
          group = "Outflow from Prefecture"
        ) +
        tm_shape(sfTemp2) +
        tm_lines(
          col = "flow",
          lwd = "flow",
          scale = 20,
          palette = "YlOrRd",
          id = "labelId",
          title.col = paste("Inflow into Prefecture", "(persons)", sep = "<br>"),
          n = 8,
          group = "Inflow into Prefecture"
        )
    }
    if (input$typeOfFlow == 2) {
      #Layer Order
      sfTemp1 <- sfTemp1 %>% dplyr::arrange(shFlow)
      sfTemp2 <- sfTemp2 %>% dplyr::arrange(shFlow)
      #Make Map
      tmap_mode("view")
      tp <- tm_shape(sfPref) +
        tm_basemap(NULL) +
        tm_borders(lwd = 0.3) +
        tm_shape(sfTemp1) +
        tm_lines(
          col = "shFlow",
          lwd = "shFlow",
          scale = 20,
          palette = "YlOrRd",
          id = "labelId",
          title.col = paste(
            "Outflow from Prefecture",
            "Share to Population",
            "in Origin Prefecture (%)",
            sep = "<br>"
          ),
          n = 5,
          group = "Outflow from Prefecture"
        ) +
        tm_shape(sfTemp2) +
        tm_lines(
          col = "shFlow",
          lwd = "shFlow",
          scale = 20,
          palette = "YlOrRd",
          id = "labelId",
          title.col = paste(
            "Inflow into Prefecture",
            "Share to Population",
            "in Origin Prefecture (%)",
            sep = "<br>"
          ),
          n = 5,
          group = "Inflow into Prefecture"
        )
    }
    
    #Leaflet from tmap
    lf <- tmap_leaflet(tp)
    lf %>%
      leaflet::setView(lat = 35.559778,
                       lng = 136.096321,
                       zoom = 5) %>%
      #Tile Layer
      addMapboxGL(
        accessToken = accessToken,
        style = styleUrlDark,
        setView = FALSE
      ) %>%
      #Layer Control
      addLayersControl(
        position = "topright",
        baseGroups = c("Outflow from Prefecture", "Inflow into Prefecture"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })  

  #++++++++++++++++++++++++++++++++++++++
  #Datatable Download
  output$downloadDataObs <- downloadHandler(
    filename = function() {
      paste("covid19_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      dfTemp <- dfCovidPanel %>%
        dplyr::filter(date <= dateEndObservedData)
      readr::write_csv(dfTemp, con)
    }
  )
  
  #++++++++++++++++++++++++++++++++++++++
  #Datatable Download
  output$downloadDataFlow <- downloadHandler(
    filename = function() {
      paste("odflow_data_from_resas_", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      readr::write_csv(dfFlow, con)
    }
  )

  #++++++++++++++++++++++++++++++++++++++
  #Update DateInput in Spatial Diffusion Map
  observe({
    updateDateInput(
      session,
      "dateSimulationMap",
      value = input$dateRangeEndSimulation,
      min = dateStartSimulation,
      max = input$dateRangeEndSimulation
    )
  })
  
  #++++++++++++++++++++++++++++++++++++++
  #Update pulldown value in Covid-19 Map
  observeEvent(input$covid19mapObserbed_shape_click, {
    updateSelectInput(session,
                      "prefCodeCovid19map",
                      selected = listPref0[[input$covid19mapObserbed_shape_click$id+1]])
  })
  
  #++++++++++++++++++++++++++++++++++++++
  #Update pulldown value in Covid-19 Map
  observeEvent(input$cavid19mapSimulated_shape_click, {
    updateSelectInput(session,
                      "prefCodeCovid19mapSim",
                      selected = listPref[[input$cavid19mapSimulated_shape_click$id]])
  })
  
}