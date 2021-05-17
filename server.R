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

    ## ++++++++++++++++++++++++++++++++++++++++++
    ## LOAD SIMULATION RESULTS
    ## ++++++++++++++++++++++++++++++++++++++++++
    #++++++++++++++++++++++++++++++++++++++
    #UPDATE DataFrame
    idx <- as.integer(input$radioCase)
    dateStartSimulation <- listDateStartSimulation[[idx]]
    dfResults1Long <- listResult1[[idx]] %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::left_join(dfInfo, by = c("date", "prefCode"))
    dfResults2Long <- listResult2[[idx]] %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::left_join(dfInfo, by = c("date", "prefCode"))

    ## ++++++++++++++++++++++++++++++++++++++++++
    ## MAKE FIGURES USING SIMULATION RESULTS
    ## ++++++++++++++++++++++++++++++++++++++++++

    #++++++++++++++++++++++++++++++++++++++
    #linePlot using simulated data in tabPanel Time-Series
    inputDateRangeStartSimulation <- dateStartSimulation
    inputDateRangeEndSimulation <- reactive(input$dateRangeEndSimulation)
    inputCase <- as.integer(input$radioCase)
    
    #Update Time-Series Line
    observeEvent(input$prefCodeLinePlotSim, {
    
      #DataFrame
      dfCovidPanelPref <- dfCovidPanel %>%
        dplyr::filter(prefCode == as.integer(stringr::str_sub(input$prefCodeLinePlotSim, 1, 2))) %>%
        dplyr::filter(date <= dateEndObservedData) %>%
        dplyr::mutate(numDays = row_number())
      dfResults1LongPref <- dfResults1Long %>%
        dplyr::filter(prefCode == as.integer(stringr::str_sub(input$prefCodeLinePlotSim, 1, 2)))
      dfResults2LongPref <- dfResults2Long %>%
        dplyr::filter(prefCode == as.integer(stringr::str_sub(input$prefCodeLinePlotSim, 1, 2)))

      #LABEL
      if(inputCase == 1){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility"
        labelLine2 <- "Simulation from the model without interregional mobility"
        inputLabelLine <- c(labelLine0, labelLine1, labelLine2)
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#f45b5b"
        colorLine2 <- "#25b086"
        inputColorLine <- c(colorLine0, colorLine1, colorLine2)
      }
      if(inputCase == 2){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility"
        labelLine2 <- "Simulation from the model without interregional mobility"
        inputLabelLine <- c(labelLine0, labelLine1, labelLine2)
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#f45b5b"
        colorLine2 <- "#25b086"
        inputColorLine <- c(colorLine0, colorLine1, colorLine2)
      }
      if(inputCase == 3){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility at 8 pm"
        labelLine2 <- "Simulation from the model with interregional mobility at 2 pm"
        inputLabelLine <- c(labelLine0, labelLine1, labelLine2)
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#483d8b"
        colorLine2 <- "#f45b5b"
        inputColorLine <- c(colorLine0, colorLine1, colorLine2)
      }
      if(inputCase == 4){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility except infectious persons"
        labelLine2 <- "Simulation from the model with interregional mobility"
        inputLabelLine <- c(labelLine0, labelLine1, labelLine2)
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#483d8b"
        colorLine2 <- "#f45b5b"
        inputColorLine <- c(colorLine0, colorLine1, colorLine2)
      }
      if(inputCase == 5){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility except Greater Tokyo area"
        labelLine2 <- "Simulation from the model with interregional mobility"
        inputLabelLine <- c(labelLine0, labelLine1, labelLine2)
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#483d8b"
        colorLine2 <- "#f45b5b"
        inputColorLine <- c(colorLine0, colorLine1, colorLine2)
      }
      if(inputCase == 6){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility except Greater Osaka area"
        labelLine2 <- "Simulation from the model with interregional mobility"
        inputLabelLine <- c(labelLine0, labelLine1, labelLine2)
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#483d8b"
        colorLine2 <- "#f45b5b"
        inputColorLine <- c(colorLine0, colorLine1, colorLine2)
      }
      if(inputCase == 7){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility except Tokyo and Osaka"
        labelLine2 <- "Simulation from the model with interregional mobility"
        inputLabelLine <- c(labelLine0, labelLine1, labelLine2)
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#483d8b"
        colorLine2 <- "#f45b5b"
        inputColorLine <- c(colorLine0, colorLine1, colorLine2)
      }
          
      #Scale Factor of Transmision Rate
      callModule(
        prefLinePlot1,
        "prefLinePlotEach1",
        dfCovidPanelPref,
        dfResults1LongPref,
        dfResults2LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputLabelLine,
        inputColorLine
      )
      
      #Number of Susceptible
      callModule(
        prefLinePlot2,
        "prefLinePlotEach2",
        dfCovidPanelPref,
        dfResults1LongPref,
        dfResults2LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputLabelLine,
        inputColorLine
      )
      
      #Number of Exposed
      callModule(
        prefLinePlot3,
        "prefLinePlotEach3",
        dfCovidPanelPref,
        dfResults1LongPref,
        dfResults2LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputLabelLine,
        inputColorLine
      )
      
      #Number of Infectious
      callModule(
        prefLinePlot4,
        "prefLinePlotEach4",
        dfCovidPanelPref,
        dfResults1LongPref,
        dfResults2LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputLabelLine,
        inputColorLine
      )
      
      #Number of Recovered
      callModule(
        prefLinePlot5,
        "prefLinePlotEach5",
        dfCovidPanelPref,
        dfResults1LongPref,
        dfResults2LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputLabelLine,
        inputColorLine
      )

      #Daily Number of New Infections
      callModule(
        prefLinePlot6,
        "prefLinePlotEach6",
        dfCovidPanelPref,
        dfResults1LongPref,
        dfResults2LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputLabelLine,
        inputColorLine
      )
      
      #Ratio of Daytime and Nighttime Force of Infection
      callModule(
        prefLinePlot7,
        "prefLinePlotEach7",
        dfCovidPanelPref,
        dfResults1LongPref,
        dfResults2LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputLabelLine,
        inputColorLine
      )
    })
    #++++++++++++++++++++++++++++++++++++++
    #Lineplot using simulated data in tabPanel Spatial Distribution
    inputDateRangeStartSimulation <- dateStartSimulation
    inputDateRangeEndSimulation <- reactive(input$dateRangeEndSimulation)
    inputDateSimulationMap <- reactive(input$dateSimulationMap)
    
    #National Total and Each Prefecture
    observeEvent(input$prefCodeCovid19mapSim, {
      #DataFrame
      dfResults1LongPref <- dfResults1Long %>%
        dplyr::filter(prefCode == as.integer(stringr::str_sub(input$prefCodeCovid19mapSim, 1, 2)))
      #Module
      callModule(
        prefMiniLinePlot,
        "prefMiniLinePlotEach",
        dfResults1LongPref,
        inputDateRangeStartSimulation,
        inputDateRangeEndSimulation,
        inputDateSimulationMap
      )
    }, ignoreNULL = FALSE)
    
    #++++++++++++++++++++++++++++++++++++++
    #covid19mapSimulated
    output$covid19mapSimulated <- renderLeaflet({
      
      #Load Data
      dfTemp1 <- dfResults1Long %>%
        dplyr::filter(prefCode != 0) %>%
        dplyr::filter(date == input$dateSimulationMap)
      #Make ShapeFile
      sfTemp1 <- sfPref %>%
        dplyr::left_join(dfTemp1, by = "prefCode") %>%
        dplyr::mutate(labelPoly = paste0(labelPref, "<br>", "n = ", round(dI)))
      
      #Color
      qpal <- colorQuantile(palette="YlOrRd", domain = dfTemp1$dI, n = 7)
      qpal_interval <- 1 / 7
      qpal_colors <- unique(qpal(sort(dfTemp1$dI))) # hex codes
      qpal_labs <- round(quantile(dfTemp1$dI, seq(0, 1, qpal_interval))) # depends on n from pal
      qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
      
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
          fillOpacity = 0.5,
          stroke = TRUE,
          weight = 0.2,
          color = ~ qpal(dI),
          layerId = ~ prefCode,
          label = ~ lapply(labelPoly, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position="topleft",
          title = paste(
            "Daily Number of", 
            "New Infections",
            sep = "<br>"
          ),
          colors = qpal_colors, 
          labels = qpal_labs)
    })
    
    
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
      dplyr::filter(prefCode != 0) %>%
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
  observeEvent(input$covid19mapSimulated_shape_click, {
    updateSelectInput(session,
                      "prefCodeCovid19mapSim",
                      selected = listPref[[input$covid19mapSimulated_shape_click$id]])
  })
  
}