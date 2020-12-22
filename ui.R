## (c) Keisuke Kondo
## Date (First Version): 2020-05-05
## Date (Latest Version): 2020-12-20
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
  p(span(style="border-bottom: solid 1px white;", "<BETA VERSION>")),
  p("NOTE: The COVID-19 Simulator visualizes the simulation results of the spatial spread of COVID-19 across 47 prefectures in Japan based on the spatial SEIR model developed by Kondo (2020). Currently, the COVID-19 Simulator offers the six case scenarios."),
  # Slider bar for Basic Reproductive Ratio R0
  awesomeRadio(
    "radioCase",
    label = h3(span(style="border-bottom: solid 1px white;", icon("hand-point-down"), HTML(paste0("Select one of the following case scenarios:")))),
    choices = c(
      "Case Scenario 1" = 1,
      "Case Scenario 2" = 2,
      "Case Scenario 3" = 3,
      "Case Scenario 4" = 4,
      "Case Scenario 5" = 5,
      "Case Scenario 6" = 6
    ),
    selected = 4,
    status = "primary"
  ),
  box(title = span(style="font-size: 14px", "Explation of this case scenario"),
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
                          tabBox(width = 12,
                                 #------------------------------------------------
                                 tabPanel(title="Time-Series", icon = icon("chart-line"),
                                          fluidRow(
                                            column(
                                              width = 3,
                                              div(
                                                style = "padding-left:10px;padding-right:10px;",
                                                h2(span(icon("chart-line"), "Time-Series")),
                                                p("This page visualizes time-series data simulated from the spatial SEIR model by prefecture."),
                                                # Type
                                                radioButtons(
                                                  "typeOfVarSimulation",
                                                  label = h3(span(
                                                    icon("hand-point-down"), "Select variable or parameter:"
                                                  )),
                                                  choices = list(
                                                    "Number of Susceptible Persons: \\(S(t)\\)" = 1,
                                                    "Number of Exposed Persons: \\(E(t)\\)" = 2,
                                                    "Number of Infectious Persons: \\(I(t)\\)" = 3,
                                                    "Number of Recovered Persons: \\(R(t)\\)" = 4,
                                                    "Number of New Positive Cases: \\(\\varepsilon E(t)\\)" = 5,
                                                    "Ratio of Force of Infection in the Daytime and Nighttime: \\( [\\tilde{I}(t)/\\tilde{N}(t)]/[I(t)/N] \\)" = 6,
                                                    "Time-varying Transmission Rate: \\(\\beta (t)\\)" = 7,
                                                    "Scaling Factor for Transmission Rate: \\(\\alpha (t)\\)" = 8,
                                                    "Effective Reproduction Number: \\(\\mathcal{R}_{e}(t)\\)" = 9
                                                  ),
                                                  selected = 3,
                                                  width = "100%"
                                                ),
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
                                              width = 9,
                                              div(style = "margin:10px; clear: both;",
                                                #Prefecture
                                                selectInput(
                                                  "prefCodeLinePlotSim",
                                                  width = "100%",
                                                  label = h3(span(icon("chart-line"), "Select prefecture")),
                                                  choices = listPref0,
                                                  selected = listPref0[[1]]
                                                ),
                                                box(
                                                  width = NULL,
                                                  title = (span(
                                                    icon("chart-line"), "Predicted Numbers from Spatial SEIR Model"
                                                  )),
                                                  solidHeader = TRUE,
                                                  status = "primary",
                                                  prefLinePlotUI("prefLinePlotEach") %>%
                                                    withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                                )
                                              )
                                            )
                                          )
                                 ),
                                 #------------------------------------------------
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
                                                          max = as.Date("2021-08-31", "%Y-%m-%d"),
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
                                          leafletOutput("cavid19mapSimulated", height = 660) %>%
                                            withSpinner(color = getOption("spinner.color", default = "#3C8EBC"))
                                 ),
                                 #------------------------------------------------
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
                                       <li>\\(\\beta(t)\\): time-varying transmission rate in date \\(t\\), \\(\\alpha(t) \\times \\beta\\) </li>
                                       <li>\\(\\alpha(t)\\): degree of non-pharmaceutical interventions in date \\(t\\), \\( \\alpha(t) \\in (0, 1) \\) </li>
                                       <li>\\(\\mathcal{R}_{e}(t)\\): effective reproducation rate in date \\(t\\), \\(\\alpha(t) \\times \\mathcal{R}_{0}\\) </li>
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
                                     HTML("<p>Kondo, Keisuke (2020) &quot;The impacts of interregional mobility restriction on spatial spread of COVID-19 in Japan,&quot; RIETI Discussion Paper. (available soon)</p>"),
                                     p("URL: ", a(href = "https://www.rieti.go.jp/en/index.html", "https://www.rieti.go.jp/en/index.html", .noWS = "outside"), .noWS = c("after-begin", "before-end")),

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
                       p("Last updated: November 13, 2020", align = "right"),
                       #
                       h3(style="border-bottom: solid 1px black;", "Cases of COVID-19 by Prefecture"),
                       p("The positive cases of COVID-19 in each prefecture are taken from official webpage of the each prefectural government. 
                         These numbers do not necessarily match with those which the Ministry of Health, Labour and Welfare publish each day.
                         The following link is confirmed as of November 27, 2020."),
                       HTML("<ol>
                               <li>Hokkaido<br>
                               http://www.pref.hokkaido.lg.jp/ <br>
                               https://www.harp.lg.jp/opendata/dataset/1369.html
                               </li>
                               <li>Aomori<br>
                               https://www.pref.aomori.lg.jp/ <br>
                               https://opendata.pref.aomori.lg.jp/dataset/1531.html
                               </li>
                               <li>Iwate<br>
                               https://www.pref.iwate.jp/ <br>
                               https://www.pref.iwate.jp/kurashikankyou/iryou/covid19/index.html
                               </li>
                               <li>Miyagi<br>
                               https://www.pref.miyagi.jp/site/covid-19/<br>
                               https://www.pref.miyagi.jp/site/covid-19/02.html
                               </li>
                               <li>Akita<br>
                               https://www.pref.akita.lg.jp/ <br>
                               https://www.pref.akita.lg.jp/pages/archive/47957 
                               </li>
                               <li>Yamagata<br>
                               https://www.pref.yamagata.jp/ <br>
                               https://www.pref.yamagata.jp/ou/bosai/020072/kochibou/coronavirus/coronavirus.html<br>
                               https://www.pref.yamagata.jp/ou/kenkofukushi/090001/20130425/shingata_corona.html
                               </li>
                               <li>Fukui<br>
                               https://www.pref.fukui.lg.jp/ <br>
                               https://www.pref.fukui.lg.jp/doc/kenkou/kansensyo-yobousessyu/corona.html<br>
                               https://www.pref.fukui.lg.jp/doc/toukei-jouhou/covid-19.html
                               </li>
                               <li>Ibaraki<br>
                               https://www.pref.ibaraki.jp/ <br>
                               https://www.pref.ibaraki.jp/1saigai/2019-ncov/index.html<br>
                               https://www.pref.ibaraki.jp/1saigai/2019-ncov/ichiran.html
                               </li>
                               <li>Tochigi<br>
                               http://www.pref.tochigi.lg.jp/ <br>
                               http://www.pref.tochigi.lg.jp/e04/welfare/hoken-eisei/kansen/hp/coronakensahasseijyoukyou.html
                               </li>
                               <li>Gunma<br>
                               https://www.pref.gunma.jp/ <br>
                               https://www.pref.gunma.jp/07/z87g_00016.html
                               </li>
                               <li>Saitama<br>
                               https://www.pref.saitama.lg.jp/<br>
                               https://www.pref.saitama.lg.jp/a0701/covid19/jokyo.html
                               </li>
                               <li>Chiba<br>
                               https://www.pref.chiba.lg.jp/<br>
                               https://www.pref.chiba.lg.jp/shippei/press/2019/ncov-index.html
                               </li>
                               <li>Tokyo<br>
                               https://www.metro.tokyo.lg.jp/<br>
                               https://stopcovid19.metro.tokyo.lg.jp/
                               </li>
                               <li>Kanagawa<br>
                               https://www.pref.kanagawa.jp/<br>
                               https://www.pref.kanagawa.jp/osirase/1369/
                               </li>
                               <li>Niigata<br>
                               https://www.pref.niigata.lg.jp/ <br>
                               https://www.pref.niigata.lg.jp/sec/kenko/covid19.html
                               </li>
                               <li>Toyama<br>
                               http://www.pref.toyama.jp/<br>
                               http://www.pref.toyama.jp/cms_sec/1205/kj00021798.html
                               </li>
                               <li>Ishikawa<br>
                               https://www.pref.ishikawa.lg.jp/<br>
                               https://www.pref.ishikawa.lg.jp/kansen/coronakennai.html
                               </li>
                               <li>Fukui<br>
                               https://www.pref.fukui.lg.jp/<br>
                               https://www.pref.fukui.lg.jp/doc/kenkou/kansensyo-yobousessyu/corona.html<br>
                               https://www.pref.fukui.lg.jp/doc/toukei-jouhou/covid-19.html<br>
                               https://covid19-fukui.com/
                               </li>
                               <li>Yamanashi<br>
                               https://www.pref.yamanashi.jp/<br>
                               https://www.pref.yamanashi.jp/koucho/coronavirus/info_coronavirus.html<br>
                               https://www.pref.yamanashi.jp/koucho/coronavirus/info_coronavirus_prevention.html
                               </li>
                               <li>Nagano<br>
                               https://www.pref.nagano.lg.jp/<br>
                               https://www.pref.nagano.lg.jp/hoken-shippei/kenko/kenko/kansensho/joho/corona-doko.html
                               </li>
                               <li>Gifu<br>
                               https://www.pref.gifu.lg.jp/<br>
                               https://www.pref.gifu.lg.jp/kinkyu-juyo-joho/shingata_corona_kansendoko.html
                               </li>
                               <li>Sizuoka<br>
                               https://www.pref.shizuoka.jp/<br>
                               https://www.pref.shizuoka.jp/kinkyu/covid-19.html<br>
                               https://opendata.pref.shizuoka.jp/dataset/8167.html
                               </li>
                               <li>Aichi<br>
                               https://www.pref.aichi.jp/<br>
                               https://www.pref.aichi.jp/site/covid19-aichi/<br>
                               </li>
                               <li>Mie<br>
                               https://www.pref.mie.lg.jp/<br>
                               https://www.pref.mie.lg.jp/YAKUMUS/HP/m0068000066_00002.htm
                               </li>
                               <li>Shiga<br>
                               https://www.pref.shiga.lg.jp/<br>
                               https://www.pref.shiga.lg.jp/ippan/kenkouiryouhukushi/yakuzi/309252.html<br>
                               https://www.pref.shiga.lg.jp/ippan/kenkouiryouhukushi/yakuzi/310735.html
                               </li>
                               <li>Kyoto<br>
                               https://www.pref.kyoto.jp/<br>
                               https://www.pref.kyoto.jp/kentai/news/novelcoronavirus.html
                               </li>
                               <li>Osaka<br>
                               http://www.pref.osaka.lg.jp/<br>
                               https://covid19-osaka.info/
                               </li>
                               <li>Hyogo<br>
                               https://web.pref.hyogo.lg.jp/<br>
                               https://web.pref.hyogo.lg.jp/kk03/corona_hasseijyokyo.html
                               </li>
                               <li>Nara<br>
                               http://www.pref.nara.jp/<br>
                               http://www.pref.nara.jp/55062.htm<br>
                               http://www.pref.nara.jp/55168.htm
                               </li>
                               <li>Wakayama<br>
                               https://www.pref.wakayama.lg.jp/<br>
                               https://www.pref.wakayama.lg.jp/prefg/000200/covid19.html<br>
                               https://wakayama-pref-org.github.io/
                               </li>
                               <li>Tottori<br>
                               https://www.pref.tottori.lg.jp/<br>
                               https://www.pref.tottori.lg.jp/corona-virus/
                               </li>
                               <li>Shimane<br>
                               https://www.pref.shimane.lg.jp/<br>
                               https://www.pref.shimane.lg.jp/bousai_info/bousai/kikikanri/shingata_taisaku/new_coronavirus_portal.html<br>
                               https://www.pref.shimane.lg.jp/medical/yakuji/kansensyo/other/topics/bukan2020.html
                               </li>
                               <li>Okayama<br>
                               https://www.pref.okayama.jp/<br>
                               https://www.pref.okayama.jp/kinkyu/645925.html
                               </li>
                               <li>Hiroshima<br>
                               https://www.pref.hiroshima.lg.jp/<br>
                               https://www.pref.hiroshima.lg.jp/site/2019-ncov/<br>
                               https://www.pref.hiroshima.lg.jp/soshiki/57/covid19-cases.html
                               </li>
                               <li>Yamaguchi<br>
                               https://www.pref.yamaguchi.lg.jp/<br>
                               https://www.pref.yamaguchi.lg.jp/cms/a10000/korona2020/202004240002.html<br>
                               https://yamaguchi.stopcovid19.jp/
                               </li>
                               <li>Tokushima<br>
                               https://www.pref.tokushima.lg.jp/<br>
                               https://www.pref.tokushima.lg.jp/ippannokata/kenko/kansensho/5035331/<br>
                               https://www.pref.tokushima.lg.jp/ippannokata/kenko/kansensho/5034012/
                               </li>
                               <li>Kagawa<br>
                               https://www.pref.kagawa.lg.jp/<br>
                               https://www.pref.kagawa.lg.jp/content/dir1/dir1_6/dir1_6_2/wt5q49200131182439.shtml
                               </li>
                               <li>Ehime<br>
                               https://www.pref.ehime.jp/<br>
                               https://www.pref.ehime.jp/h25500/kansen/covid19.html<br>
                               https://www.pref.ehime.jp/opendata-catalog/dataset/2174.html
                               </li>
                               <li>Kochi<br>
                               https://www.pref.kochi.lg.jp/<br>
                               https://www.pref.kochi.lg.jp/soshiki/111301/info-COVID-19.html<br>
                               https://www.pref.kochi.lg.jp/soshiki/111301/2020041300141.html
                               </li>
                               <li>Fukuoka<br>
                               https://www.pref.fukuoka.lg.jp/<br>
                               https://www.pref.fukuoka.lg.jp/contents/covid-19-portal.html<br>
                               https://ckan.open-governmentdata.org/dataset/8a9688c2-7b9f-4347-ad6e-de3b339ef740
                               </li>
                               <li>Saga<br>
                               https://www.pref.saga.lg.jp/<br>
                               https://www.pref.saga.lg.jp/kiji00373220/index.html
                               </li>
                               <li>Nagasaki<br>
                               https://www.pref.nagasaki.jp/<br>
                               https://www.pref.nagasaki.jp/bunrui/hukushi-hoken/kansensho/corona_nagasaki/<br>
                               https://www.pref.nagasaki.jp/bunrui/hukushi-hoken/kansensho/corona_nagasaki/corona_nagasaki_shousai/
                               </li>
                               <li>Kumamoto<br>
                               https://www.pref.kumamoto.jp/ <br>
                               https://www.pref.kumamoto.jp/kiji_32300.html <br>
                               https://www.pref.kumamoto.jp/kiji_22038.html <br>
                               </li>
                               <li>Oita<br>
                               https://www.pref.oita.jp/<br>
                               https://www.pref.oita.jp/site/covid19-oita/<br>
                               https://data.bodik.jp/dataset/_covid19
                               </li>
                               <li>Miyazaki<br>
                               https://www.pref.miyazaki.lg.jp/ <br>
                               https://www.pref.miyazaki.lg.jp/covid-19/index.html
                               </li>
                               <li>Kagoshima<br>
                               https://www.pref.kagoshima.jp/<br>
                               https://www.pref.kagoshima.jp/kenko-fukushi/covid19/index.html<br>
                               https://www.pref.kagoshima.jp/ae06/kenko-fukushi/kenko-iryo/kansen/kansensho/coronavirus.html
                               </li>
                               <li>Okinawa<br>
                               https://www.pref.okinawa.lg.jp/ <br>
                               https://www.pref.okinawa.lg.jp/site/chijiko/kohokoryu/koho/2020_new_corona_potal.html<br>
                               https://www.pref.okinawa.lg.jp/site/hoken/chiikihoken/kekkaku/covid19_hasseijoukyou.html
                               </li>
                               </ol>
                               "),
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
                       p("URL: ", a(href = "https://www.e-stat.go.jp/en", "https://www.e-stat.go.jp/en", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
                       h3(style="border-bottom: solid 1px black;", "Download Data"),
                       p(
                         "The data used in the COVID-19 Simulator is available from the following links:"
                       ),
                       h4(
                         span(
                           icon("table"),
                           "COVID-19 Daily and Cumulative Data of People Testing Positive up to November 10, 2020"
                         )
                       ),
                       downloadButton("downloadDataObs", "Download", class = "btn btn-primary btn-lg"),
                       h4(
                         span(
                           icon("table"),
                           "Simulation Results from Spatial SEIR Model with Mobility",
                         )
                       ),
                       downloadButton("downloadDataSim1", "Download", class = "btn btn-primary btn-lg"),
                       h4(
                         span(
                           icon("table"),
                           "Simulation Results from Spatial SEIR Model without Mobility"
                         )
                       ),
                       downloadButton("downloadDataSim2", "Download", class = "btn btn-primary btn-lg"),
                       h4(span(
                         icon("table"), "Origin-Destination Flows across 47 Prefectures"
                       )),
                       downloadButton("downloadDataFlow", "Download", class = "btn btn-primary btn-lg")
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
