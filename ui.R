## (c) Keisuke Kondo
## Date (First Version): 2020-05-05
## Date (Latest Version): 2021-05-17
##
## - global.R
## - server.R
## - ui.R
##

#HEADER-------------------------------------------------------------------------
header <- dashboardHeader(
  title = "COVID-19 Simulator",
  titleWidth = 250,
  disable = FALSE
)
#SIDEBAR------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 250,
  #++++++++++++++++++++++++++++++++++++++
  #CSS
  useShinyjs(),
  tags$head(
    # Include CSS
    includeCSS("styles.css")
  ),
  #++++++++++++++++++++++++++++++++++++++
  h2(span(style="border-bottom: solid 1px white;", "Settings for Visualization")),
  #NOTE
  p("The COVID-19 Simulator visualizes the simulation results of the spatial spread of COVID-19 across 47 prefectures in Japan based on the spatial SEIR model developed by Kondo (2020)."),
  # Slider bar for Basic Reproductive Ratio R0
  awesomeRadio(
    "radioCase",
    label = h3(span(style="border-bottom: solid 1px white;", icon("hand-point-down"), HTML(paste0("Select one of the following scenarios:")))),
    choices = c(
      "Scenario 1" = 1,
      "Scenario 2" = 2,
      "Scenario 3" = 3,
      "Scenario 4" = 4,
      "Scenario 5" = 5,
      "Scenario 6" = 6,
      "Scenario 7" = 7
    ),
    selected = 2,
    status = "primary"
  ),
  box(title = span(style="font-size: 14px", "Explation of this scenario"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      div(style="font-size: 12px; color: black;",
        htmlOutput("textCase")
      )
  ),
  br(),
  div(
    actionButton("buttonSimulation", span(icon("play-circle"), "Visualize This Scenario"), class="btn btn-info"),
    p("Press the Visualize This Scenario button after choosing one of the Cases."),
    actionButton("buttonReset", span(icon("sync"), "Initialize"), class="btn btn-info"),
    p("Press the Initialize button when the page fails to load.")
  )
)
#BODY---------------------------------------------------------------------------
body <- dashboardBody(
  #++++++++++++++++++++++++++++++++++++++
  #CSS
  useShinyjs(),
  tags$head(
    # Include CSS
    includeCSS("styles.css")
  ),
  #++++++++++++++++++++++++++++++++++++++
  ####################################
  ## NAVBARPAGE
  ## - Visualize simulation
  ## - Model
  ## - Data
  ## - Author
  ## - Terms of Use
  ## - Github
  ####################################
  navbarPage(span(style="font-weight:bold;color:white", "MENU"),
             id = "navbarpageMain",
             theme = shinytheme("flatly"),
             #++++++++++++++++++++++++++++++++++++++
             tabPanel("Visualization", icon = icon("chart-bar"),
                      ####################################
                      ## TABBOX
                      ## - Time-Series
                      ## - Spatial Distribution
                      ## - COVID-19 Data
                      ## - Spatial Network Data
                      ####################################
                      div(style="margin-left: -30px;margin-right: -30px;",
                          ## - Time-Series
                          tabBox(width = 12,
                                 #------------------------------------------------
                                 tabPanel(title="Time-Series", icon = icon("chart-line"),
                                          fluidRow(
                                            column(
                                              width = 12,
                                              div(
                                                style = "padding-left:10px;padding-right:10px;",
                                                h2(span(icon("chart-line"), "Time-Series")),
                                                p("This page visualizes time-series data simulated from the spatial SEIR model by prefecture.")                                              )
                                            ),
                                            column(
                                              width = 6,
                                              div(
                                                style = "padding-left:10px;padding-right:10px;",
                                                #Prefecture
                                                selectInput(
                                                  "prefCodeLinePlotSim",
                                                  width = "100%",
                                                  label = h3(span(icon("chart-line"), "Select prefecture")),
                                                  choices = listPref0,
                                                  selected = listPref0[[14]]
                                                )
                                              )
                                            ),
                                            column(
                                              width = 6,
                                              div(
                                                style = "padding-left:10px;padding-right:10px;",
                                                #DateRange of Simulation
                                                dateInput(
                                                  "dateRangeEndSimulation",
                                                  label = h3(span(style="border-bottom: solid 1px white;", icon("hand-point-down"), "Select the end date"
                                                  )),
                                                  value = as.Date("2022-12-31", "%Y-%m-%d"),
                                                  min = as.Date("2020-12-31", "%Y-%m-%d"),
                                                  max = as.Date("2023-12-31", "%Y-%m-%d"),
                                                  startview = "year",
                                                  language = "en",
                                                  width = "100%"
                                                )
                                              )
                                            ),
                                            column(
                                              width = 12,
                                              div(style = "margin:10px; clear: both;",
                                                box(
                                                  width = NULL,
                                                  title = (span(
                                                    icon("chart-line"), "Scaling Factor of Transmission Rate"
                                                  )),
                                                  solidHeader = TRUE,
                                                  status = "primary",
                                                  prefLinePlot1UI("prefLinePlotEach1") %>%
                                                    withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                )
                                              )
                                            ),
                                            column(
                                              width = 12,
                                              div(style = "margin:10px; clear: both;",
                                                  box(
                                                    width = NULL,
                                                    title = (span(
                                                      icon("chart-line"), "Number of Susceptible People"
                                                    )),
                                                    solidHeader = TRUE,
                                                    status = "primary",
                                                    prefLinePlot2UI("prefLinePlotEach2") %>%
                                                      withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                  )
                                              )
                                            ),
                                            column(
                                              width = 12,
                                              div(style = "margin:10px; clear: both;",
                                                  box(
                                                    width = NULL,
                                                    title = (span(
                                                      icon("chart-line"), "Number of Exposed People"
                                                    )),
                                                    solidHeader = TRUE,
                                                    status = "primary",
                                                    prefLinePlot3UI("prefLinePlotEach3") %>%
                                                      withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                  )
                                              )
                                            ),
                                            column(
                                              width = 12,
                                              div(style = "margin:10px; clear: both;",
                                                  box(
                                                    width = NULL,
                                                    title = (span(
                                                      icon("chart-line"), "Number of Infectious People"
                                                    )),
                                                    solidHeader = TRUE,
                                                    status = "primary",
                                                    prefLinePlot4UI("prefLinePlotEach4") %>%
                                                      withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                  )
                                              )
                                            ),
                                            column(
                                              width = 12,
                                              div(style = "margin:10px; clear: both;",
                                                  box(
                                                    width = NULL,
                                                    title = (span(
                                                      icon("chart-line"), "Number of Recovered People"
                                                    )),
                                                    solidHeader = TRUE,
                                                    status = "primary",
                                                    prefLinePlot5UI("prefLinePlotEach5") %>%
                                                      withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                  )
                                              )
                                            ),
                                            column(
                                              width = 12,
                                              div(style = "margin:10px; clear: both;",
                                                  box(
                                                    width = NULL,
                                                    title = (span(
                                                      icon("chart-line"), "Daily Number of New Infections"
                                                    )),
                                                    solidHeader = TRUE,
                                                    status = "primary",
                                                    prefLinePlot6UI("prefLinePlotEach6") %>%
                                                      withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                  )
                                              )
                                            ),
                                            column(
                                              width = 12,
                                              div(style = "margin:10px; clear: both;",
                                                  box(
                                                    width = NULL,
                                                    title = (span(
                                                      icon("chart-line"), "Ratio of Daytime and Nighttime Force of Infection"
                                                    )),
                                                    solidHeader = TRUE,
                                                    status = "primary",
                                                    prefLinePlot7UI("prefLinePlotEach7") %>%
                                                      withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                  )
                                              )
                                            )                                          )
                                 ),
                                 #------------------------------------------------
                                 ## - Spatial Distribution
                                 tabPanel(title="Sptial Distribution", icon = icon("globe-asia"),
                                          absolutePanel(id="controls_sd1",
                                                        class = "panel panel-default",
                                                        top = "auto",
                                                        bottom = 25,
                                                        left = "auto",
                                                        right = 25,
                                                        width = "35vw",
                                                        height = "auto",
                                                        draggable = FALSE,
                                                        style="z-index:10;",
                                                        h3(span(icon("globe-asia"), "Spatial Distribution")),
                                                        p("This page visualizes the spatial distribution of simulated number of infectious persons on the selected date."),
                                                        #
                                                        dateInput(
                                                          "dateSimulationMap",
                                                          label = h4(span(icon("calendar"), "Select date:")),
                                                          value = as.Date("2020-08-31", "%Y-%m-%d"),
                                                          min = as.Date("2020-04-01", "%Y-%m-%d"),
                                                          max = as.Date("2022-12-31", "%Y-%m-%d"),
                                                          startview = "year",
                                                          language = "en",
                                                        ),
                                                        selectInput(
                                                          "prefCodeCovid19mapSim",
                                                          width = "100%",
                                                          label = h4(span(icon("chart-line"), "Select prefecture for time-series data:")),
                                                          choices = listPref0,
                                                          selected = listPref0[[1]]
                                                        ),
                                                        prefMiniLinePlotUI("prefMiniLinePlotEach")
                                          ),
                                          leafletOutput("covid19mapSimulated", height = 660) %>%
                                            withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                 ),
                                 #------------------------------------------------
                                 ## - COVID-19 Data
                                 tabPanel(title="COVID-19 Data Viewer", icon = icon("dashboard"),
                                          absolutePanel(id="controls_db",
                                                        class = "panel panel-default",
                                                        top = "auto",
                                                        bottom = 25,
                                                        left = "auto",
                                                        right = 25,
                                                        width = "35vw",
                                                        height = "auto",
                                                        draggable = FALSE,
                                                        style="z-index:10;",
                                                        #
                                                        h3(span(icon("dashboard"), "COVID-19 Data Viewer")),
                                                        p("This page visualizes spatial distribution of observed number of tested positive on the selected date."),
                                                        # Type
                                                        radioButtons("typeOfVar",
                                                                     label = span(icon("chart-line"), "Daily or cumulative number of tested positive:"),
                                                                     choices = list("Daily number" = 1, "Cumulative number" = 2),
                                                                     selected = 1,
                                                                     width = "100%"
                                                        ),
                                                        # Day
                                                        dateInput("dateCovidMap",
                                                                  label = span(icon("calendar"), "Select date:"),
                                                                  value = as.Date("2020-04-07","%Y-%m-%d"),
                                                                  min = as.Date("2020-01-01","%Y-%m-%d"),
                                                                  max = dateEndObservedData,
                                                                  startview = "month",
                                                                  language = "en"
                                                        ),
                                                        selectInput(
                                                          "prefCodeCovid19map",
                                                          label = span(icon("hand-point-right"), "Select prefecture for time-series data:"),
                                                          choices = listPref0,
                                                          selected = listPref0[[1]]
                                                        ),
                                                        prefBarPlotUI("prefBarPlotEach")
                                          ),
                                          #covid19map by leaflet
                                          leafletOutput("covid19mapObserbed", height = 660) %>%
                                            withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                 ),
                                 #------------------------------------------------
                                 ## - Spatial Network Data
                                 tabPanel(title="Spatial Network Data", icon = icon("share-alt"),
                                          absolutePanel(id="controls_sn",
                                                        class = "panel panel-default",
                                                        top = "auto",
                                                        bottom = 30,
                                                        left = 25,
                                                        right = "auto",
                                                        width = 250,
                                                        height = "auto",
                                                        draggable = FALSE,
                                                        style="z-index:10;",
                                                        h3(span(icon("share-alt"), "Spatial Network")),
                                                        p("This page visualizes spatial mobility network across 47 prefectures."),
                                                        #
                                                        selectInput("prefCodeFlow",
                                                                    label = h4(span(icon("map-marked-alt"), "Select prefecture:")),
                                                                    choices = listPref,
                                                                    selected = listPref[[13]]
                                                        ),
                                                        # Slider bar for Infectious State
                                                        selectInput(
                                                          "periodOfMonth",
                                                          label = h4(span(icon("calendar"), "Select month:")),
                                                          choices = listMonth,
                                                          selected = listMonth[[4]],
                                                        ),
                                                        #
                                                        radioButtons("periodOfDay",
                                                                     label = h4(span(icon("calendar-plus"), "Select day of the Week:")),
                                                                     choices = list("Weekday" = 1, "Weekend" = 2),
                                                                     selected = 1,
                                                                     width = "100%"
                                                        ),
                                                        radioButtons("typeOfFlow",
                                                                     label = h4(span(icon("route"), "Select type of flow:")),
                                                                     choices = list("Level of Flow" = 1, "Share of Flow" = 2),
                                                                     selected = 1,
                                                                     width = "100%"
                                                        )
                                          ),
                                          leafletOutput("odflowmapLine", height = 660) %>%
                                            withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                 )
                          )
                      )
             ),
             #++++++++++++++++++++++++++++++++++++++
             tabPanel("Model", icon = icon("file-alt"),
                      div(style="margin-left: -30px;margin-right: -30px;",
                          #
                          column(width = 12,
                                 box(width = NULL, title = h2(span(icon("file-alt"), "Model")), solidHeader = TRUE,
                                     withMathJax(),
                                     #
                                     #
                                     p("Last updated: December 22, 2020", align = "right"),
                                     #
                                     #
                                     h3(style="border-bottom: solid 1px black;", "Spatial SEIR model with Interregional Mobility"),
                                     p("I developed a spatial Susceptible-Exposed-Infectious-Recovered (SEIR) model that analyzes the effect of interregional mobility on the spatial spread of the coronavirus disease 2019 (COVID-19) outbreak in Japan. National and local governments in Japan have requested that residents refrain from traveling across 47 prefectures during the state of emergency, which was declared on April 7 of 2020. Although the interregional mobility restriction can be relaxed to include some outings after the state of emergency, the current low level of lockdown restriction was the first such experience in Japan. Effective control measures that prevent spatial spread of severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) are urgently demanded, and how non-pharmaceutical interventions (NPIs) such as travel restrictions and social distancing mitigate the epidemic must be investigated. Therefore, how interregional mobility restriction limits the expansion of infection in Japan’s context is largely unknown. Therefore, I am aiming to provide meaningful implications for combating the COVID-19 pandemic through interregional mobility restrictions."),
                                     p("As the virus spreads through face-to-face contact, a spatial network of contagion was built by introducing interregional mobility into the standard SEIR model. The model assumes that people commute to a region of work or study in the daytime and return to their residential region at night. It further assumes that people are exposed to SARS-CoV-2 infection risk during their daytime activities, meaning that residents in one region are exposed to heterogeneous infection risks of SARS-CoV-2."),
                                     p("The interregional mobility is mathematically treated as an origin–destination (OD) matrix. I demonstrate that the spatial SEIR model reduces to the standard SEIR model when the off-diagonal elements of the OD matrix are zero (i.e., when people remain in their residential regions). Therefore, this spatial SEIR model can be viewed as a generalized version of the standard SEIR model."),
                                     p("The daily OD matrix is constructed from the interregional mobility data obtained by geospatial information technology, namely, from the locational information of mobile phone users. These data capture the specific situations of individuals, such as commuting to work on weekdays and remaining in the residential region or traveling to another region during the weekends. By tracking the interregional mobility on each month, day, and time of day throughout one year, I successfully captured the daily interregional mobility flows in the counterfactual situation."),
                                     p("This study aims to implicate effective control measures based on a simulation analysis. Because mitigating the COVID-19 pandemic is an urgent priority, an epidemic model that guides the planning of efficient control measures is essential when few ideal data are available. The spatial SEIR model assumes that the past interregional mobility trend will continue in future, regardless of how the COVID-19 pandemic evolves. Comparing those simulated from the SEIR model under the free mobility assumption with those simulated under the strict interregional mobility restriction, this study evaluates how restricting movement mitigates the spatial infection spread."),
                                     p("See Kondo (2020) for the details of the spatial SEIR model with interregional mobility and the scenarios for simulation."),
                                     p("For convenience of reference, the notation used in this study is listed below:"),
                                     HTML('<ul style="line-height:1.75;">
                                       <li>\\(\\mathcal{R}_{0}\\): basic reproductive ratio (2.6 int this study). <br>This ratio indicates average number of secondary cases produced by an infectious individual. </li>
                                       <li>\\(\\ell_{\\varepsilon}\\): average incubation period. The unit of the time period is daily (5 days in this study). </li>
                                       <li>\\(\\ell_{\\gamma}\\): average infectious period. The unit of the time period is daily (10 days in this study).</li>
                                       <li>\\(\\varepsilon\\): daily probability of an exposed individual becoming infectious, \\(1/\\ell_{\\varepsilon}\\). </li>
                                       <li>\\(\\gamma\\): daily probability that an infected individual recovers, \\(1/\\ell_{\\gamma}\\). </li>
                                       <li>\\(\\beta\\): transmission rate, \\(\\mathcal{R}_{0} \\times \\gamma\\) </li>
                                       <li>\\(\\beta(t)\\): time-varying transmission rate on date \\(t\\), \\(\\alpha(t) \\times \\beta\\) </li>
                                       <li>\\(\\alpha(t)\\): degree of non-pharmaceutical interventions on date \\(t\\), \\( \\alpha(t) \\in (0, 1) \\) </li>
                                       <li>\\(\\mathcal{R}_{e}(t)\\): effective reproducation rate on date \\(t\\), \\(\\alpha(t) \\times \\mathcal{R}_{0} \\times S(t) / N\\) </li>
                                       <li>\\(t\\): period of time. The unit of time period is daily. </li>
                                       <li>\\(m\\): number of total regions. This simulation considers 47 prefectures in Japan.</li>
                                       <li>\\(N\\): national total population, \\(N = \\sum^{m}_{k=1} N_{k} = \\sum^{m}_{k=1} \\tilde{N}_{k}(t) \\)</li>
                                       <li>\\(\\tilde{N}_{i}(t)\\): total population in region \\(i\\) in the daytime</li>
                                       <li>\\(N_{i}\\): total population in region \\(i\\) in the nighttime</li>
                                       <li>\\(\\tilde{S}_{i}(t)\\): number of susceptible individuals in region \\(i\\) in the daytime</li>
                                       <li>\\(S_{i}(t)\\): number of susceptible individuals in region \\(i\\) in the nighttime</li>
                                       <li>\\(\\tilde{S}_{i}(t)\\): number of exposed individuals in region \\(i\\) in the daytime</li>
                                       <li>\\(E_{i}(t)\\): number of exposed individuals in region \\(i\\) in the nighttime</li>
                                       <li>\\(\\tilde{I}_{i}(t)\\): number of infectious individuals in region \\(i\\) in the daytime</li>
                                       <li>\\(I_{i}(t)\\): number of infectious individuals in region \\(i\\) in the nighttime</li>
                                       <li>\\(\\tilde{R}_{i}(t)\\): number of recovered individuals in region \\(i\\) in the daytime</li>
                                       <li>\\(R_{i}(t)\\): number of recovered individuals in region \\(i\\) in the nighttime</li>
                                       <li>\\(\\beta \\tilde{I}_{i}(t) / \\tilde{N}_{i}(t) \\): force of infection in region \\(i\\) in the daytime</li>
                                       <li>\\(\\beta I_{i}(t) / N_{i} \\): force of infection in region \\(i\\) in the nighttime</li>
                                       </ul>'),
                                     #
                                     h3(style="border-bottom: solid 1px black;", "References"),
                                     HTML("<p>Kondo, Keisuke (2020) &quot;The impacts of interregional mobility restriction on spatial spread of COVID-19 in Japan,&quot; medRxiv, doi: https://doi.org/10.1101/2020.12.28.20248926.</p>"),
                                       p("URL: ", a(href = "https://doi.org/10.1101/2020.12.28.20248926", "doi: https://doi.org/10.1101/2020.12.28.20248926", .noWS = "outside"), .noWS = c("after-begin", "before-end")),

                                 )
                          )
                      )
             ),
             #++++++++++++++++++++++++++++++++++++++
             tabPanel(
               "Data",
               icon = icon("database"),
               div(style = "margin-left: -30px;margin-right: -30px;",
                   column(
                     width = 12,
                     box(
                       width = NULL,
                       title = h2(span(icon("database"), "Data")),
                       solidHeader = TRUE,
                       #
                       p("Last updated: May 17, 2021", align = "right"),
                       #
                       h3(style="border-bottom: solid 1px black;", "Cases of COVID-19 by Prefecture"),
                       p("The positive cases of COVID-19 in each prefecture were taken from the NHK (Japan Broadcasting Corporation). 
                         The following link is confirmed as of May 17, 2021."
                       ),
                       p("URL: ", a(href = "https://www3.nhk.or.jp/news/special/coronavirus/data-widget/", "https://www3.nhk.or.jp/news/special/coronavirus/data-widget/", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                       #
                       h3(style="border-bottom: solid 1px black;", "Effective Reproduction Number"),
                       p("The effective reproduction number of COVID-19 was taken from the Toyo Keizai Inc. 
                         The following link is confirmed as of May 17, 2021."
                       ),
                       p("URL: ", a(href = "https://toyokeizai.net/sp/visual/tko/covid19/en.html", "https://toyokeizai.net/sp/visual/tko/covid19/en.html", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                         #
                       h3(style="border-bottom: solid 1px black;", "Spatial Network of Interregional Mobility"),
                       p(
                         "Spatial network is considered as a human mobility across 47 prefectures. Interregional mobility data is taken from the Regional Economy and Society Analyzing System (RESAS)."
                       ),
                       p("URL: ", a(href = "https://resas.go.jp/", "https://resas.go.jp/", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                       #
                       h3(style="border-bottom: solid 1px black;", "Shapefile of Japanese Prefectures"),
                       p(
                         "The shapefiles of Japanese prefectures are available from e-Stat (Ministry of Internal Affairs and Communication)"
                       ),
                       p("URL: ", a(href = "https://www.e-stat.go.jp/en", "https://www.e-stat.go.jp/en", .noWS = "outside"), .noWS = c("after-begin", "before-end"))
                     )
                   ))
             ),
             #++++++++++++++++++++++++++++++++++++++
             tabPanel("Author", icon = icon("user"),
                      div(style="margin-left: -30px;margin-right: -30px;",
                          column(width = 12,
                                 box(width = NULL, title = h2(span(icon("user"), "Author")), solidHeader = TRUE,
                                     h3("Keisuke Kondo"),
                                     p("I am a senior fellow of the Research Institute of Economy, Trade and Industry (RIETI)."),
                                     h3("Contact"),
                                     p("Email: kondo-keisuke@rieti.go.jp"),
                                     p("URL: ", a(href = "https://sites.google.com/site/keisukekondokk/", "https://sites.google.com/site/keisukekondokk/", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                                     p("URL: ", a(href = "https://keisukekondokk.github.io/", "https://keisukekondokk.github.io/", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                                     p("Address: METI Annex 11F, 1-3-1 Kasumigaseki, Chiyoda-ku, Tokyo, 100-8901, Japan"),
                                     a(href="https://www.rieti.go.jp/en/", img(src="logo_rieti.jpeg", width= "480" )),
                                     br(clear="right"),
                                     br(),
                                     p("The views expressed here are solely those of the author, and neither represent those of the organization to which the author belongs nor the Research Institute of Economy, Trade and Industry.")
                                 )
                          )
                      )
             ),
             #++++++++++++++++++++++++++++++++++++++
             tabPanel("Terms of Use", icon = icon("file-signature"),
                      div(style="margin-left: -30px;margin-right: -30px;",
                          column(width = 12,
                                 box(width = NULL, title = h2(span(icon("file-signature"), "Terms of Use")), solidHeader = TRUE,
                                     p("Updated Date: December 22, 2020", align = "right"),
                                     p("Release Date: May 13, 2020", align = "right"),
                                     br(),
                                     p("Users (hereinafter referred to as the User or Users depending on context) of the content on this web site (hereinafter referred to as the Content) are required to conform to the terms of use described herein (hereinafter referred to as the Terms of Use). Furthermore, use of the Content constitutes agreement by the User with the Terms of Use. The contents of the Terms of Use are subject to change without prior notice."),
                                     h3("Copyright"),
                                     p("The copyright of the developed code belongs to Keisuke Kondo."),
                                     h3("Copyright of Third Parties"),
                                     HTML("<p>Keisuke Kondo developed the Content based on the information on From-To Analysis on the Regional Economy and Society Analyzing System (RESAS), which is freely available using the RESAS API. The original data of From-To Analysis is based on Mobile Spatial Statistics&reg; of NTT DOCOMO. The shapefile of the 47 prefectures in Japan was taken from the Portal Site of Official Statistics of Japan, e-Stat. Users must confirm the terms of use of the RESAS and the e-Stat, prior to using the Content.</p>"),
                                     h3("License "),
                                     p("The developed code is released under the MIT License."),
                                     h3("Disclaimer"),
                                     HTML("<ul>
                                            <li>Keisuke Kondo makes the utmost effort to maintain, but nevertheless does not guarantee, the accuracy, completeness, integrity, usability, and recency of the Content.</li>
                                            <li>Keisuke Kondo and any organization to which Keisuke Kondo belongs hereby disclaim responsibility and liability for any loss or damage that may be incurred by Users as a result of using the Content. Keisuke Kondo and any organization to which Keisuke Kondo belongs are neither responsible nor liable for any loss or damage that a User of the Content may cause to any third party as a result of using the Content</li>
                                            <li>The Content may be modified, moved or deleted without prior notice.</li>
                                            </ul>"),
                                     br()
                                 )
                          )
                      )
             ),
             #++++++++++++++++++++++++++++++++++++++
             tabPanel("GitHub", icon = icon("github"),
                      fluidRow(
                        #
                        column(width = 12,
                               box(width = NULL, title = h2(span(icon("github"), "GitHub")), solidHeader = TRUE,
                                   h3("View code"),
                                   p("The R code and data are available on Github."),
                                   p("URL: ", a(href = "https://keisukekondokk.github.io/", "https://keisukekondokk.github.io/", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                                   p("URL: ", a(href = "https://github.com/keisukekondokk/spatial-seir", "https://github.com/keisukekondokk/spatial-seir", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                                   p("URL: ", a(href = "https://github.com/keisukekondokk/covid19-simulator-japan", "https://github.com/keisukekondokk/covid19-simulator-japan", .noWS = "outside"), .noWS = c("after-begin", "before-end"))
                               )
                        )
                      )
             )
  )
)
#DASHBOARD----------------------------------------------------------------------
dashboardPage(
  header,
  sidebar,
  body
)
