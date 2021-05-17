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
  dplyr::mutate(date = as.Date(date, "%Y-%m-%d", tz = "Asia/Tokyo"))

#LABEL for Case Scenario
listTextR0 <- list()
listTextR0[[1]] <- "The long-run impacts of interregional mobility on the spatial spread of COVID-19 infection based on the spatial SEIR models with and without interregional mobility are compared. The simulation starts from April 7, 2020."
listTextR0[[2]] <- "The long-run impacts of interregional mobility on the spatial spread of COVID-19 infection based on the spatial SEIR models with and without interregional mobility are compared. The simulation starts from April 23, 2021."
listTextR0[[3]] <- "The long-run impacts of interregional mobility on the spatial spread of COVID-19 infection based on the spatial SEIR models with and without interregional mobility are compared. The interregional mobility based on where they reside and where people are located at 8 pm is used. The simulation starts from April 23, 2021."
listTextR0[[4]] <- "The interregional mobility only for infectious people is restricted to remain in their residential prefectures and susceptible, exposed (infected but not yet infectious), and recovered individuals commute freely or travel across prefectures. The simulation starts from April 23, 2021."
listTextR0[[5]] <- "The interregional mobility is restricted for the Greater Tokyo area (Saitama, Chiba, Tokyo, and kanagawa). It is assumed that residents in the Greater Tokyo area are restricted to remain in each prefecture. However, residents in other prefectures are allowed to commute and travel across prefectures, except for the Greater Tokyo area. The simulation starts from April 23, 2021."
listTextR0[[6]] <- "The interregional mobility is restricted for the Greater Osaka area (Kyoto, Osaka, and Hyogo). It is assumed that residents in the Greater Osaka area are restricted to remain in each prefecture. However, residents in other prefectures are allowed to commute and travel across prefectures, except for the Greater Osaka area. The simulation starts from April 23, 2021."
listTextR0[[7]] <- "The interregional mobility of residents in Tokyo and Osaka is restricted, and residents from other prefectures are allowed to stay in Tokyo and Osaka in the daytime. The simulation starts from April 23, 2021."

#Date of Key Events for Simulation
dateEndObservedData <- as.Date("2021-04-23", "%Y-%m-%d")
listDateStartSimulation <- list()
listDateStartSimulation[[1]] <- as.Date("2020-04-07", "%Y-%m-%d")
listDateStartSimulation[[2]] <- as.Date("2021-04-23", "%Y-%m-%d")
listDateStartSimulation[[3]] <- as.Date("2021-04-23", "%Y-%m-%d")
listDateStartSimulation[[4]] <- as.Date("2021-04-23", "%Y-%m-%d")
listDateStartSimulation[[5]] <- as.Date("2021-04-23", "%Y-%m-%d")
listDateStartSimulation[[6]] <- as.Date("2021-04-23", "%Y-%m-%d")
listDateStartSimulation[[7]] <- as.Date("2021-04-23", "%Y-%m-%d")
dateStartSimulation <- listDateStartSimulation[[1]]

#COVID-19 Panel-data by Prefecture (2020-01-01 to 2021-05-09)
dfCovidPanel <- readr::read_csv("csv/input_data/CSV_covid19_panel_by_pref.csv")
dfInfo <- dfCovidPanel %>%
  dplyr::select(numDays, date, date_year, date_month, date_day, date_holiday, prefCode)

#Simulation Results (1: Model with mobility, 2: Model without mobility)
#Four case scenarios
listResult1 <- list()
listResult2 <- list()
listResult1[[1]] <- readr::read_csv("csv/case01/CSV_simulation_with_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[1]] <- readr::read_csv("csv/case01/CSV_simulation_without_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult1[[2]] <- readr::read_csv("csv/case02/CSV_simulation_with_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[2]] <- readr::read_csv("csv/case02/CSV_simulation_without_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult1[[3]] <- readr::read_csv("csv/case03/CSV_simulation_with_mobility_at_8pm.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[3]] <- readr::read_csv("csv/case03/CSV_simulation_with_mobility_at_2pm.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[1]] <- readr::read_csv("csv/case01/CSV_simulation_without_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult1[[4]] <- readr::read_csv("csv/case04/CSV_simulation_with_mobility_except_I.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[4]] <- readr::read_csv("csv/case04/CSV_simulation_with_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult1[[5]] <- readr::read_csv("csv/case05/CSV_simulation_with_mobility_except_Greater_Tokyo_Area.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[5]] <- readr::read_csv("csv/case05/CSV_simulation_with_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult1[[6]] <- readr::read_csv("csv/case06/CSV_simulation_with_mobility_except_Greater_Osaka_Area.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[6]] <- readr::read_csv("csv/case06/CSV_simulation_with_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult1[[7]] <- readr::read_csv("csv/case07/CSV_simulation_with_mobility_except_Tokyo_Osaka.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
listResult2[[7]] <- readr::read_csv("csv/case07/CSV_simulation_with_mobility.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))

#OD Flows across Prefectures
sfFlow <- sf::read_sf("shp/SHP_lines_od_flows_by_pref_time14.shp")
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
prefLinePlot1UI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "520px"))
}
#Module for Prefecture
prefLinePlot1 <-
  function(input,
           output,
           session,
           inputDataFrameData,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation,
           inputLabelLine,
           inputColorLine) {
    
    #Highcharts
    output$linePlot <- renderHighchart({
      
      #DataFrame
      dfResults1LongPref <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, Alpha)
      dfResults2LongPref <- inputDataFrameResult2 %>%
        dplyr::select(numDays, date, Alpha)
      
      #Observed Data
      dfTemp0 <- inputDataFrameData
      #Starting Date of Simulation
      dfTempFilter <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterAlpha <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterAlpha$date-1, "%Y-%m-%d"))
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      #Simulated Data from Model with Mobility
      dfTemp1 <- dfResults1LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(Alpha = if_else(date < inputDataRangeStartSimulation, NA_real_, Alpha))
      #Simulated Data from Model without Mobility
      dfTemp2 <- dfResults2LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(Alpha = if_else(date < inputDataRangeStartSimulation, NA_real_, Alpha))
      
      #LABEL
      labelLine0 <- inputLabelLine[1]
      labelLine1 <- inputLabelLine[2]
      labelLine2 <- inputLabelLine[3]
      colorLine0 <- inputColorLine[1]
      colorLine1 <- inputColorLine[2]
      colorLine2 <- inputColorLine[3]
      
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
          data = dfTempFilterAlpha,
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
          plotLines = list(list(value = 1/R0, color = "red", width = 2, dashStyle = "shortdash")),
          allowDecimals = TRUE
        ) %>%
        hc_xAxis(categories = dfTemp1$date) %>%
        hc_tooltip(valueDecimals = 3,
                   pointFormat = "Number: {point.y}") %>%
        hc_add_theme(hc_theme_gridlight())

      #plot
      hc
    }
    )
    
  }

## ++++++++++++++++++++++++++++++++++++
## Module for visualization
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefLinePlot2UI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "520px"))
}
#Module for Prefecture
prefLinePlot2 <-
  function(input,
           output,
           session,
           inputDataFrameData,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation,
           inputLabelLine,
           inputColorLine) {
    
    #Highcharts
    output$linePlot <- renderHighchart({
      
      #DataFrame
      dfResults1LongPref <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, S)
      dfResults2LongPref <- inputDataFrameResult2 %>%
        dplyr::select(numDays, date, S)
      
      #Observed Data
      dfTemp0 <- inputDataFrameData 
      #Starting Date of Simulation
      dfTempFilter <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterAlpha <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterAlpha$date-1, "%Y-%m-%d"))

      #Highcharts
      #Adjust xAxisLabel: numDays -1
      #Simulated Data from Model with Mobility
      dfTemp1 <- dfResults1LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(S = if_else(date < inputDataRangeStartSimulation, NA_real_, S))
      #Simulated Data from Model without Mobility
      dfTemp2 <- dfResults2LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(S = if_else(date < inputDataRangeStartSimulation, NA_real_, S))

      #LABEL
      labelLine0 <- inputLabelLine[1]
      labelLine1 <- inputLabelLine[2]
      labelLine2 <- inputLabelLine[3]
      colorLine0 <- inputColorLine[1]
      colorLine1 <- inputColorLine[2]
      colorLine2 <- inputColorLine[3]

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
      
      #plot
      hc
    }
    )
    
  }

## ++++++++++++++++++++++++++++++++++++
## Module for visualization
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefLinePlot3UI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "520px"))
}
#Module for Prefecture
prefLinePlot3 <-
  function(input,
           output,
           session,
           inputDataFrameData,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation,
           inputLabelLine,
           inputColorLine) {

    #Highcharts
    output$linePlot <- renderHighchart({
      
      #DataFrame
      dfResults1LongPref <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, E)
      dfResults2LongPref <- inputDataFrameResult2 %>%
        dplyr::select(numDays, date, E)
      
      #Observed Data
      dfTemp0 <- inputDataFrameData
      #Starting Date of Simulation
      dfTempFilter <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterAlpha <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterAlpha$date-1, "%Y-%m-%d"))
      
      #LABEL
      labelLine0 <- inputLabelLine[1]
      labelLine1 <- inputLabelLine[2]
      labelLine2 <- inputLabelLine[3]
      colorLine0 <- inputColorLine[1]
      colorLine1 <- inputColorLine[2]
      colorLine2 <- inputColorLine[3]
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      #Simulated Data from Model with Mobility
      dfTemp1 <- dfResults1LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(E = if_else(date < inputDataRangeStartSimulation, NA_real_, E))
      #Simulated Data from Model without Mobility
      dfTemp2 <- dfResults2LongPref %>%
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
      
      #plot
      hc
    }
    )
    
  }

## ++++++++++++++++++++++++++++++++++++
## Module for visualization
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefLinePlot4UI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "520px"))
}
#Module for Prefecture
prefLinePlot4 <-
  function(input,
           output,
           session,
           inputDataFrameData,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation,
           inputLabelLine,
           inputColorLine) {
    
    #Highcharts
    output$linePlot <- renderHighchart({
      
      #DataFrame
      dfResults1LongPref <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, I)
      dfResults2LongPref <- inputDataFrameResult2 %>%
        dplyr::select(numDays, date, I)
      
      #Observed Data
      dfTemp0 <- inputDataFrameData
      #Starting Date of Simulation
      dfTempFilter <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterAlpha <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterAlpha$date-1, "%Y-%m-%d"))
      
      #LABEL
      labelLine0 <- inputLabelLine[1]
      labelLine1 <- inputLabelLine[2]
      labelLine2 <- inputLabelLine[3]
      colorLine0 <- inputColorLine[1]
      colorLine1 <- inputColorLine[2]
      colorLine2 <- inputColorLine[3]
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      #Simulated Data from Model with Mobility
      dfTemp1 <- dfResults1LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(I = if_else(date < inputDataRangeStartSimulation, NA_real_, I))
      #Simulated Data from Model without Mobility
      dfTemp2 <- dfResults2LongPref %>%
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
      
      #plot
      hc
    }
    )
    
  }

## ++++++++++++++++++++++++++++++++++++
## Module for visualization
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefLinePlot5UI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "520px"))
}
#Module for Prefecture
prefLinePlot5 <-
  function(input,
           output,
           session,
           inputDataFrameData,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation,
           inputLabelLine,
           inputColorLine) {
    
    #Highcharts
    output$linePlot <- renderHighchart({
      
      #DataFrame
      dfResults1LongPref <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, R)
      dfResults2LongPref <- inputDataFrameResult2 %>%
        dplyr::select(numDays, date, R)
      
      #Observed Data
      dfTemp0 <- inputDataFrameData
      #Starting Date of Simulation
      dfTempFilter <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterAlpha <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterAlpha$date-1, "%Y-%m-%d"))

      #LABEL
      labelLine0 <- inputLabelLine[1]
      labelLine1 <- inputLabelLine[2]
      labelLine2 <- inputLabelLine[3]
      colorLine0 <- inputColorLine[1]
      colorLine1 <- inputColorLine[2]
      colorLine2 <- inputColorLine[3]
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      #Simulated Data from Model with Mobility
      dfTemp1 <- dfResults1LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(R = if_else(date < inputDataRangeStartSimulation, NA_real_, R))
      #Simulated Data from Model without Mobility
      dfTemp2 <- dfResults2LongPref %>%
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
      
      #plot
      hc
    }
    )
    
  }

## ++++++++++++++++++++++++++++++++++++
## Module for visualization
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefLinePlot6UI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "520px"))
}
#Module for Prefecture
prefLinePlot6 <-
  function(input,
           output,
           session,
           inputDataFrameData,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation,
           inputLabelLine,
           inputColorLine) {
    
    #Highcharts
    output$linePlot <- renderHighchart({
      
      #DataFrame
      dfResults1LongPref <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, dI)
      dfResults2LongPref <- inputDataFrameResult2 %>%
        dplyr::select(numDays, date, dI)
      
      #Observed Data
      dfTemp0 <- inputDataFrameData
      #Starting Date of Simulation
      dfTempFilter <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterAlpha <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterAlpha$date-1, "%Y-%m-%d"))

      #LABEL
      labelLine0 <- inputLabelLine[1]
      labelLine1 <- inputLabelLine[2]
      labelLine2 <- inputLabelLine[3]
      colorLine0 <- inputColorLine[1]
      colorLine1 <- inputColorLine[2]
      colorLine2 <- inputColorLine[3]
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      #Simulated Data from Model with Mobility
      dfTemp1 <- dfResults1LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(dI = if_else(date < inputDataRangeStartSimulation, NA_real_, dI))
      #Simulated Data from Model without Mobility
      dfTemp2 <- dfResults2LongPref %>%
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
      
      #plot
      hc
    }
    )
    
  }

## ++++++++++++++++++++++++++++++++++++
## Module for visualization
## ++++++++++++++++++++++++++++++++++++
#Module UI for Prefecture
prefLinePlot7UI <- function(id) {
  ns <- NS(id)
  tagList(highchartOutput(ns("linePlot"), height = "520px"))
}
#Module for Prefecture
prefLinePlot7 <-
  function(input,
           output,
           session,
           inputDataFrameData,
           inputDataFrameResult1,
           inputDataFrameResult2,
           inputDataRangeStartSimulation,
           inputDataRangeEndSimulation,
           inputLabelLine,
           inputColorLine) {
    
    #Highcharts
    output$linePlot <- renderHighchart({
      
      #DataFrame
      dfResults1LongPref <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, RatioLambda)
      dfResults2LongPref <- inputDataFrameResult2 %>%
        dplyr::select(numDays, date, RatioLambda)

      #Observed Data
      dfTemp0 <- dfResults1LongPref
      #Starting Date of Simulation
      dfTempFilter <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilter$date, "%Y-%m-%d"))
      #Starting Date of Simulation
      dfTempFilterAlpha <- dfResults1LongPref %>%
        dplyr::filter(date == inputDataRangeStartSimulation+1)
      labelPoint <- paste0("Starting date of simulation: ", format(dfTempFilterAlpha$date-1, "%Y-%m-%d"))
      
      #LABEL
      labelLine0 <- inputLabelLine[1]
      labelLine1 <- inputLabelLine[2]
      labelLine2 <- inputLabelLine[3]
      colorLine0 <- inputColorLine[1]
      colorLine1 <- inputColorLine[2]
      colorLine2 <- inputColorLine[3]
      
      #Highcharts
      #Adjust xAxisLabel: numDays -1
      #Simulated Data from Model with Mobility
      dfTemp1 <- dfResults1LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(RatioLambda = if_else(date < inputDataRangeStartSimulation, NA_real_, RatioLambda))
      #Simulated Data from Model without Mobility
      dfTemp2 <- dfResults2LongPref %>%
        dplyr::filter(date <= inputDataRangeEndSimulation()) %>%
        dplyr::mutate(RatioLambda = if_else(date < inputDataRangeStartSimulation, NA_real_, RatioLambda))
      
      #RatioLambda
      hc <- highchart() %>%
        hc_add_series(
          data = dfTemp2,
          hcaes(numDays-1, (RatioLambda)),
          type = "line",
          color = colorLine2,
          name = labelLine2,
          lineWidth = 5,
          showInLegend = TRUE
        ) %>%
        hc_add_series(
          data = dfTemp1,
          hcaes(numDays-1, (RatioLambda)),
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
          min = min(dfTemp1$RatioLambda),
          max = max(dfTemp1$RatioLambda),
          allowDecimals = TRUE
        ) %>%
        hc_xAxis(categories = dfTemp1$date) %>%
        hc_tooltip(valueDecimals = 3,
                   pointFormat = "Number: {point.y}") %>%
        hc_add_theme(hc_theme_gridlight())
      
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
           inputDateRangeStartSimulation,
           inputDateRangeEndSimulation,
           inputDateSimulationMap) {
    
    #Highcharts
    output$miniLinePlot <- renderHighchart({
      #Observed Data
      dfTemp0 <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, dI) %>%
        dplyr::filter(date <= inputDateRangeEndSimulation()) %>%
        dplyr::mutate(dI = if_else(date > inputDateRangeStartSimulation, NA_real_, dI))
      #Simulated Data from Model with Mobility
      dfTemp1 <- inputDataFrameResult1 %>%
        dplyr::select(numDays, date, dI) %>%
        dplyr::filter(date <= inputDateRangeEndSimulation()) %>%
        dplyr::mutate(dI = if_else(date < inputDateRangeStartSimulation, NA_real_, dI))
      
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
          hcaes(numDays-1, (dI)),
          type = "line",
          name = "Daily Number of New Infections (Observed)",
          color = "black",
          showInLegend = FALSE
        ) %>%
        hc_add_series(
          data = (dfTemp1),
          hcaes(numDays-1, (dI)),
          type = "line",
          name = "Daily Number of New Infections (Simulated from Model with Mobility)",
          color = "black",
          showInLegend = FALSE
        ) %>%
        hc_add_series(
          data = dfTempFilter1,
          hcaes(numDays-1, (dI)),
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
        hc_yAxis(title = list(text = "Daily Number of New Infections (Observed and Simulated)"),
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
