## (c) Keisuke Kondo
## Date (First Version): 2020-05-05
## Date (Latest Version): 2020-12-20
## 
## - global.R
## - server.R
## - ui.R
## 

#Required Packages
if(!require(devtools)) install.packages("devtools")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(shinycssloaders)) install.packages("shinycssloaders")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(shinyjs)) install.packages("shinyjs")
if(!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(leaflet)) install.packages("leaflet")
if(!require(leaflet.minicharts)) install.packages("leaflet.minicharts")
if(!require(leaflet.mapboxgl)) devtools::install_github("rstudio/leaflet.mapboxgl")
if(!require(htmlwidgets)) install.packages("htmlwidgets")
if(!require(highcharter)) install.packages("highcharter")
if(!require(sf)) install.packages("sf")
if(!require(tmap)) install.packages("tmap")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tibble)) install.packages("tibble")
if(!require(readr)) install.packages("readr")
if(!require(lubridate)) install.packages("lubridate")

#######################################
## Mapbox
#######################################
## SET MAPBOX API

#Mapbox API--------------------------------------------
#Variables are defined on .Renviron
styleUrlBasic <- Sys.getenv("MAPBOX_STYLE_BASIC")
styleUrlDark <- Sys.getenv("MAPBOX_STYLE_DARK")
accessToken <- Sys.getenv("MAPBOX_ACCESS_TOKEN")
#Mapbox API--------------------------------------------

#######################################
## Data
#######################################
## - Prefecture Code and Label
## - Population
## - Calender 
## - Origin-Destination Flows
## - Covid-19 for 47 prefectures
## - Shapefile for 47 prefecture
#######################################
#Pull Down of Prefecture List
dfPref <- readr::read_csv("csv/input_data/CSV_list_pref.csv")
dfPref <- dfPref %>%
  dplyr::mutate(labelPref = paste0(as.character(prefCode), " ", prefNameEn))
numPref <- nrow(dfPref)

#Make Label
listPref <- as.list(dfPref$labelPref)
listPref0 <- c(list("0 National Total"), listPref)
listMonth <- as.list(month.name)

#Population by Prefecture as of Oct. 2019
dfPop <- readr::read_csv("csv/input_data/CSV_pop_by_pref_201910.csv")

#Calendar for Weekday Weekend
dfCalendar <- readr::read_csv("csv/input_data/CSV_calendar.csv") %>%
  dplyr::mutate(date = as.Date(date, "%Y-%m-%d"))

#LABEL for Case Scenario
listTextR0 <- list()
listTextR0[[1]] <- "This is the <b>rapid covergence case scenario</b> at the early stage with <i><b>time-constant transmission rate</b> &beta;<sub>t</sub></i> for each month. See Kondo (2020) for details of the parameter setting. The simulation starts from <b>April 7, 2020</b>."
listTextR0[[2]] <- "This is the <b>modest convergence case scenario</b> in which covergence and divergence repeat with <i><b>time-varying transmission rate</b> &beta;<sub>t</sub></i> for each month. See Kondo (2020) for details of the parameter setting. The simulation starts from <b>April 7, 2020</b>."
listTextR0[[3]] <- "This is the <b>modest convergence case scenario</b> in which covergence and divergence repeat with <i><b>time-varying transmission rate</b> &beta;<sub>t</sub></i> for each month. See Kondo (2020) for details of the parameter setting. The simulation starts from <b>July 1, 2020</b>."
listTextR0[[4]] <- "This is the <b>modest convergence case scenario</b> in which covergence and divergence repeat with <i><b>time-varying transmission rate</b> &beta;<sub>t</sub></i> for each month. See Kondo (2020) for details of the parameter setting. The simulation starts from <b>November 4, 2020</b>."
listTextR0[[5]] <- "This is the <b>worsening case scenario</b> in which covergence and divergence repeat with <i><b>time-varying transmission rate</b> &beta;<sub>t</sub></i> for each month. See Kondo (2020) for details of the parameter setting. The simulation starts from <b>November 4, 2020</b>."
listTextR0[[6]] <- "This is the <b>mobility restriction only for Tokyo</b> and the <b>modest convergence case scenario</b> in which covergence and divergence repeat with <i><b>time-varying transmission rate</b> &beta;<sub>t</sub></i> for each month. See Kondo (2020) for details of the parameter setting. The simulation starts from <b>November 4, 2020</b>."

#Date of Key Events for Simulation
dateEndObservedData <- as.Date("2020-11-10", "%Y-%m-%d")
listDateStartSimulation <- list()
listDateStartSimulation[[1]] <- as.Date("2020-04-07", "%Y-%m-%d")
listDateStartSimulation[[2]] <- as.Date("2020-04-07", "%Y-%m-%d")
listDateStartSimulation[[3]] <- as.Date("2020-07-01", "%Y-%m-%d")
listDateStartSimulation[[4]] <- as.Date("2020-11-04", "%Y-%m-%d")
listDateStartSimulation[[5]] <- as.Date("2020-11-04", "%Y-%m-%d")
listDateStartSimulation[[6]] <- as.Date("2020-11-04", "%Y-%m-%d")
dateStartSimulation <- listDateStartSimulation[[1]]

#COVID-19 Panel-data by Prefecture (2020-01-01 to 2020-11-15)
dfCovidPanel <- readr::read_csv("csv/input_data/CSV_covid19_panel_by_pref.csv")
dfInfo <- dfCovidPanel %>%
  dplyr::select(numDays, date, date_year, date_month, date_day, date_holiday, prefCode)

#Simulation Results (1: Model with mobility, 2: Model without mobility)
#Four case scenarios
listResult1 <- list()
listResult2 <- list()
listResult1[[1]] <- readr::read_csv("csv/case01/CSV_simulation_with_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult2[[1]] <- readr::read_csv("csv/case01/CSV_simulation_without_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult1[[2]] <- readr::read_csv("csv/case02/CSV_simulation_with_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult2[[2]] <- readr::read_csv("csv/case02/CSV_simulation_without_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult1[[3]] <- readr::read_csv("csv/case03/CSV_simulation_with_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult2[[3]] <- readr::read_csv("csv/case03/CSV_simulation_without_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult1[[4]] <- readr::read_csv("csv/case04/CSV_simulation_with_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult2[[4]] <- readr::read_csv("csv/case04/CSV_simulation_without_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult1[[5]] <- readr::read_csv("csv/case05/CSV_simulation_with_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult2[[5]] <- readr::read_csv("csv/case05/CSV_simulation_without_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult1[[6]] <- readr::read_csv("csv/case06/CSV_simulation_with_mobility_except_Tokyo.csv", col_types = list("Ratio" = "d", "Beta" = "d"))
listResult2[[6]] <- readr::read_csv("csv/case06/CSV_simulation_with_mobility.csv", col_types = list("Ratio" = "d", "Beta" = "d"))

#OD Flows across Prefectures
sfFlow <- sf::read_sf("shp/SHP_lines_od_flows_by_pref.shp")
sfFlow <- sfFlow %>%
  dplyr::rename(prefCodeOrigin = prefCdO) %>%
  dplyr::rename(prefCodeDestination = prefCdD) %>%
  dplyr::rename(prefNameOrigin = prefNmO) %>%
  dplyr::rename(prefNameDestination = prefNmD) %>%
  dplyr::rename(periodOfDay = prdOfDy) %>%
  dplyr::rename(periodOfTIme = prdOfTm) %>%
  dplyr::rename(totalPopOrigin = ttlPpOr)
dfFlow <- st_drop_geometry(sfFlow)

#Construct OD Flow Matrix
listC <- list()
#Monthly (January to December)
for(i in 1:12) {
  listC[[i]] <- list()
  #Period of Day (weekday or weekend)
  for(j in 1:2) {
    dfTemp <- dfFlow %>%
      dplyr::filter(month == i & periodOfDay == j) %>%
      dplyr::select(shFlow)
    #OD matrix
    mC <- matrix(dfTemp$shFlow/100, numPref, numPref, byrow = TRUE)
    listC[[i]][[j]] <- mC 
  }
}
rm(dfTemp, mC)

#Shape File of Japanese Prefectures
sfPref <- sf::read_sf("shp/japan_pref_lonlat.shp") %>%
  dplyr::left_join(dfPref, by = "prefCode")
rm(dfPref)

#Parameters
R0 <- 2.6
l_gamma <- 10
l_epsilon <- 5
gamma <- 1 / l_gamma
epsilon <- 1 / l_epsilon
beta0 <- R0 * gamma


#######################################
## MODULE
#######################################
## - barchart in tabPanel COVID-19 Data Viewer
## - linechart in tabPanel Time-Series
## - linechart in tabPanel Spatial Distribution
#######################################
## ++++++++++++++++++++++++++++++++++++
## Module for visualization of observed data
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefBarPlotUI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("barPlot"), height = "232px"))
}
#Module for Prefecture
prefBarPlot <-
  function(input,
           output,
           session,
           inputPrefCode,
           inputTypeOfVar,
           inputDateCovidMap) {
    #Highcharts
    output$barPlot <- renderHighchart({
      #Data
      dfTemp <- dfCovidPanel %>%
        dplyr::filter(prefCode == inputPrefCode) %>%
        dplyr::filter(date <= dateEndObservedData)
      #Type of variable
      if (inputTypeOfVar() == 1) {
        dfTemp <- dfTemp %>%
          dplyr::mutate(y = newPositive)
        ytitleText <- "Daily number of new patients tested positive"
      }
      if (inputTypeOfVar() == 2) {
        dfTemp <- dfTemp %>%
          dplyr::mutate(y = cumPositive)
        ytitleText <- "Cumulative number of patients tested positive"
      }
      #Date on Map
      dfTempFilter <- dfTemp %>%
        dplyr::filter(date == inputDateCovidMap())
      labelPoint <- paste0("Date on map", format(dfTempFilter$date, "%Y-%m-%d"))
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      hc <- highchart() %>%
        hc_add_series(
          data = dfTemp,
          hcaes(numDays-1, y),
          type = "column",
          color = "#CD5C5C",
          pointWidth = 0.9,
          borderWidth = 0,
          borderColor = "transparent",
          name = ytitleText,
          showInLegend = FALSE
        ) %>%
        hc_yAxis(title = list(text = ytitleText),
                 allowDecimals = FALSE) %>%
        hc_xAxis(categories = dfTemp$date) %>%
        hc_tooltip(valueDecimals = 0,
                   pointFormat = "Number: {point.y}") %>%
        hc_add_theme(hc_theme_gridlight())
      
      #plot
      hc
    })
  }

## ++++++++++++++++++++++++++++++++++++
## Module for visualization
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefLinePlotUI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "448px"))
}
#Module for Prefecture
prefLinePlot <-
  function(input,
           output,
           session,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputCase,
           inputPrefCode0,
           inputTypeOfVarSim,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation) {
    #Highcharts
    output$linePlot <- renderHighchart({
      #Observed Data
      dfTemp0 <- dfCovidPanel %>%
        dplyr::filter(prefCode == inputPrefCode0) %>%
        dplyr::filter(date <= dateEndObservedData) %>%
        dplyr::mutate(numDays = row_number())
      #Starting Date of Simulation
      dfTempFilter <- inputDataFrameResult1 %>%
        dplyr::filter(prefCode == inputPrefCode0) %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterBeta <- inputDataFrameResult1 %>%
        dplyr::filter(prefCode == inputPrefCode0) %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterBeta$date-1, "%Y-%m-%d"))
      
      #LABEL
      if(inputCase == 6){
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility excluding Tokyo"
        labelLine2 <- "Simulation from the model with interregional mobility"
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#20b2aa"
        colorLine2 <- "#f45b5b"
      } else {
        labelLine0 <- "Observed Data"
        labelLine1 <- "Simulation from the model with interregional mobility"
        labelLine2 <- "Simulation from the model without interregional mobility"
        colorLine0 <- "#2f7ed8"
        colorLine1 <- "#f45b5b"
        colorLine2 <- "#25b086"
      }
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      if (inputTypeOfVarSim() == 1) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(S = if_else(date < inputDataRangeStartSimulation, NA_real_, S))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(S = if_else(date < inputDataRangeStartSimulation, NA_real_, S))

        #Susceptibles, S
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp0,
            hcaes(numDays-1, (S)),
            type = "line",
            color = colorLine0,
            name = labelLine0,
            lineWidth = 5.5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp2,
            hcaes(numDays-1, (S)),
            type = "line",
            color = colorLine2,
            name = labelLine2,
            lineWidth = 5,
            lineOpacity = 0.8,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (S)),
            type = "line",
            color = colorLine1,
            name = labelLine1,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilter,
            hcaes(numDays-1, (S)),
            type = "scatter",
            color = "red",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Observed/Simulated Number of Susceptible Persons"),
            allowDecimals = FALSE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 0,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }
      if (inputTypeOfVarSim() == 2) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(E = if_else(date < inputDataRangeStartSimulation, NA_real_, E))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(E = if_else(date < inputDataRangeStartSimulation, NA_real_, E))

        #Exposed, E
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp0,
            hcaes(numDays-1, (E)),
            type = "line",
            color = colorLine0,
            name = labelLine0,
            lineWidth = 5.5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp2,
            hcaes(numDays-1, (E)),
            type = "line",
            color = colorLine2,
            name = labelLine2,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (E)),
            type = "line",
            color = colorLine1,
            name = labelLine1,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilter,
            hcaes(numDays-1, (E)),
            type = "scatter",
            color = "red",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Observed/Simulated Number of Exposed Persons"),
            allowDecimals = FALSE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 0,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }
      if (inputTypeOfVarSim() == 3) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(I = if_else(date < inputDataRangeStartSimulation, NA_real_, I))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(I = if_else(date < inputDataRangeStartSimulation, NA_real_, I))
        
        #Infectious, I
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp0,
            hcaes(numDays-1, (I)),
            type = "line",
            color = colorLine0,
            name = labelLine0,
            lineWidth = 5.5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp2,
            hcaes(numDays-1, (I)),
            type = "line",
            color = colorLine2,
            name = labelLine2,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (I)),
            type = "line",
            color = colorLine1,
            name = labelLine1,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilter,
            hcaes(numDays-1, (I)),
            type = "scatter",
            color = "red",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Observed/Simulated Number of Infectious Persons"),
            allowDecimals = FALSE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 0,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }
      if (inputTypeOfVarSim() == 4) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(R = if_else(date < inputDataRangeStartSimulation, NA_real_, R))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(R = if_else(date < inputDataRangeStartSimulation, NA_real_, R))
        
        #Recovered
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp0,
            hcaes(numDays-1, (R)),
            type = "line",
            color = colorLine0,
            name = labelLine0,
            lineWidth = 5.5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp2,
            hcaes(numDays-1, (R)),
            type = "line",
            color = colorLine2,
            name = labelLine2,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (R)),
            type = "line",
            color = colorLine1,
            name = labelLine1,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilter,
            hcaes(numDays-1, (R)),
            type = "scatter",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Observed/Simulated Number of Recoverd Persons"),
            allowDecimals = FALSE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 0,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }
      if (inputTypeOfVarSim() == 5) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(dI = if_else(date < inputDataRangeStartSimulation, NA_real_, dI))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(dI = if_else(date < inputDataRangeStartSimulation, NA_real_, dI))
        
        #Recovered
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp0,
            hcaes(numDays-1, (dI)),
            type = "line",
            color = colorLine0,
            name = labelLine0,
            lineWidth = 5.5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp2,
            hcaes(numDays-1, (dI)),
            type = "line",
            color = colorLine2,
            name = labelLine2,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (dI)),
            type = "line",
            color = colorLine1,
            name = labelLine1,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilter,
            hcaes(numDays-1, (dI)),
            type = "scatter",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Observed/Simulated Number of Newl Positive Cases"),
            allowDecimals = FALSE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 0,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }   
      if (inputTypeOfVarSim() == 6) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(Ratio = if_else(date < inputDataRangeStartSimulation, NA_real_, Ratio))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(Ratio = if_else(date < inputDataRangeStartSimulation, NA_real_, 1))
        
        #Ratio
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp2,
            hcaes(numDays-1, (Ratio)),
            type = "line",
            color = colorLine2,
            name = labelLine2,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (Ratio)),
            type = "line",
            color = colorLine1,
            name = labelLine1,
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilter,
            hcaes(numDays-1, (1)),
            type = "scatter",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Ratio of force of infection in the daytime and nighttime"),
            min = min(dfTemp1$Ratio),
            max = max(dfTemp1$Ratio),
            allowDecimals = TRUE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 3,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }
      if (inputTypeOfVarSim() == 7) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(Beta = if_else(date < inputDataRangeStartSimulation, NA_real_, Beta))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(Beta = if_else(date < inputDataRangeStartSimulation, NA_real_, Beta))

        #Beta
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (Beta)),
            type = "line",
            color = "#f45b5b",
            name = "Transmission Rate",
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilterBeta,
            hcaes(numDays-1, (Beta)),
            type = "scatter",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Transmission Rate"),
            min = 0.0,
            max = 0.3,
            allowDecimals = TRUE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 3,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }
      if (inputTypeOfVarSim() == 8) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(Alpha = if_else(date < inputDataRangeStartSimulation, NA_real_, Alpha))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(Alpha = if_else(date < inputDataRangeStartSimulation, NA_real_, Alpha))
        
        #Alpha
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (Alpha)),
            type = "line",
            color = "#f45b5b",
            name = "Scaling Factor for Transmission Rate",
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilterBeta,
            hcaes(numDays-1, (Alpha)),
            type = "scatter",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Scaling Factor for Transmission Rate"),
            min = 0.0,
            max = 1.0,
            plotLines = list(list(value = 0.4, color = "red", width = 2, dashStyle = "shortdash")),
            allowDecimals = TRUE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 3,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }   
      if (inputTypeOfVarSim() == 9) {
        #Simulated Data from Model with Mobility
        dfTemp1 <- inputDataFrameResult1 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(R0t = if_else(date < inputDataRangeStartSimulation, NA_real_, R0t))
        #Simulated Data from Model without Mobility
        dfTemp2 <- inputDataFrameResult2 %>%
          dplyr::filter(prefCode == inputPrefCode0) %>%
          dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
          dplyr::mutate(R0t = if_else(date < inputDataRangeStartSimulation, NA_real_, R0t))
        
        #Effective Reproduction Number
        hc <- highchart() %>%
          hc_add_series(
            data = dfTemp1,
            hcaes(numDays-1, (R0t)),
            type = "line",
            color = "#f45b5b",
            name = "Scaling Factor for Transmission Rate",
            lineWidth = 5,
            showInLegend = TRUE
          ) %>%
          hc_add_series(
            data = dfTempFilterBeta,
            hcaes(numDays-1, (R0t)),
            type = "scatter",
            name = labelPoint,
            showInLegend = FALSE,
            marker = list(
              radius = 10,
              symbol = "square",
              lineWidth = 5,
              lineColor = "#e0d72d",
              fillColor = "transparent"
            )
          ) %>%
          hc_yAxis(
            title = list(text = "Effective Reproduction Number"),
            min = 0.0,
            max = 2.5,
            plotLines = list(list(value = 1, color = "red", width = 2, dashStyle = "shortdash")),
            allowDecimals = TRUE
          ) %>%
          hc_xAxis(categories = dfTemp1$date) %>%
          hc_tooltip(valueDecimals = 3,
                     pointFormat = "Number: {point.y}") %>%
          hc_add_theme(hc_theme_gridlight())
      }         
      #plot
      hc
    }
    )
    
  }


## ++++++++++++++++++++++++++++++++++++
## Module for visualization of simulation results
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefMiniLinePlotUI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("miniLinePlot"), height = "280px"))
}
#Module for Prefecture
prefMiniLinePlot <-
  function(input,
           output,
           session,
           inputDataFrameResult1,
           inputPrefCode,
           inputDateRangeStartSimulation,
           inputDateRangeEndSimulation,
           inputDateSimulationMap) {
    #Highcharts
    output$miniLinePlot <- renderHighchart({
      #Observed Data
      dfTemp0 <- inputDataFrameResult1 %>%
        dplyr::filter(prefCode == inputPrefCode) %>%
        dplyr::filter(date <= inputDateRangeEndSimulation()) %>%
        dplyr::mutate(I = if_else(date > inputDateRangeStartSimulation, NA_real_, I))
      #Simulated Data from Model with Mobility
      dfTemp1 <- inputDataFrameResult1 %>%
        dplyr::filter(prefCode == inputPrefCode) %>%
        dplyr::filter(date <= inputDateRangeEndSimulation()) %>%
        dplyr::mutate(I = if_else(date < inputDateRangeStartSimulation, NA_real_, I))
      
      #Date on Map
      dfTempFilter1 <- dfTemp1 %>%
        dplyr::filter(date == inputDateSimulationMap())
      labelPointMap <- paste0("Date on map vizualization: ", format(dfTempFilter1$date, "%Y-%m-%d"))
      #Date on Simulation
      dfTempFilter2 <- dfTemp1 %>%
        dplyr::filter(date == inputDateRangeStartSimulation)
      labelPointSimulation <- paste0("Starting date of simulation: ", format(dfTempFilter2$date, "%Y-%m-%d"))

      #Highcharts
      #Adjust xAxisLabel: numDays -1
      hc <- highchart() %>%
        hc_add_series(
          data = (dfTemp0),
          hcaes(numDays-1, (I)),
          type = "line",
          name = "Number of Infectious Persons (Observed)",
          color = "black",
          showInLegend = FALSE
        ) %>%
        hc_add_series(
          data = (dfTemp1),
          hcaes(numDays-1, (I)),
          type = "line",
          name = "Number of Infectious Persons (Simulated from Model with Mobility)",
          color = "black",
          showInLegend = FALSE
        ) %>%
        hc_add_series(
          data = dfTempFilter1,
          hcaes(numDays-1, (I)),
          type = "scatter",
          color = "red",
          name = labelPointMap,
          showInLegend = FALSE,
          marker = list(
            radius = 5,
            symbol = "circle",
            fillColor = "red"
          )
        ) %>%
        hc_yAxis(title = list(text = "Number of Infectious Persons (Observed and Simulated)"),
                 allowDecimals = FALSE) %>%
        hc_xAxis(categories = dfTemp1$date,
                 plotLines = list(
                   list(color = "#FF0000",
                        width = 1,
                        value = dfTempFilter2$numDays-1)
                 )) %>%
        hc_tooltip(valueDecimals = 0,
                   pointFormat = "Number: {point.y}") %>%
        hc_add_theme(hc_theme_gridlight())
      
      #plot
      hc
    })
  }
