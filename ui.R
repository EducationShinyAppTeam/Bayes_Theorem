library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)

shinyUI(fluidPage(
  
  withMathJax(),
  
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Bayes' Theorem"),
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Prerequisites", icon  = icon("book"), tabName = "prereq", selected = TRUE),
                    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                    menuItem("Explore", icon = icon("wpexplorer"), tabName = "explore")
                  )
                ),
                dashboardBody(
                  tags$head( 
                    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
                  ),
                  tabItems(
                    tabItem(tabName = "prereq",
                            h3(strong("Background: Bayes' Theorem")),
                            h4("For two events D and T, Bayes' Theorem relates P(D|T) to P(T|D) through:"),
                            fluidRow(
                              column(12, align="center",
                                     uiOutput('calculation_part'))),
                            h4("In the screening test example used in this application, we define:"),
                            h4(tags$li("D = Has the disease")),
                            h4(tags$li("T = Tests positive for the disease")),
                            h4(tags$li("P(D) is called the Prevalence")),
                            h4(tags$li("P(T|D) is called the Sensitivity")),
                            h4(tags$li("P(Not T|Not D) is called the Specificity")),
                            br(),
                            div(style = "text-align: center",
                                bsButton("goover", "Go to the overview", size = "large", icon = icon("bolt")))
                            ),
                    tabItem(tabName = "overview",
                            tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                            br(),br(),br(),
                              h3(strong("About:")),
                              h4("This app is designed to demonstrate Bayes' Theorem using the classic example of disease incidence."),
                              h3(strong("Instructions:")),
                              h4("Adjust the sliders to help you solve the challenges."),
                            br(),
                            div(style = "text-align: center",
                                bsButton("go", "GO!", size = "large", icon = icon("bolt"))),
                              h3(strong("Acknowledgements:")),
                              h4("This app was developed and coded by Sam Messer.")
                             
                                 ),
                    tabItem(tabName = "explore",
                            tags$head(
                              tags$style(".shiny-notification {position:fixed;bottom:50px;left:25%;width:50%;} ")),
                            div(style="display: inline-block;vertical-align:top;",
                                tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                            ),
                            
                            div(style="display: inline-block;vertical-align:top;",
                                circleButton("info",icon = icon("info"), status = "myClass",size = "xs"),
                                bsPopover("info", "Instructions","Adjust sliders and observe the effects",placement = "bottom", trigger = "hover", options = NULL)
                            ),
                            div(style="display: inline-block;vertical-align:top;",
                                circleButton("hint",icon = icon("question"), status = "myClass",size = "xs"),
                                bsPopover("hint", "Hint","Think about the way each slider affects the probability",placement = "bottom", trigger = "hover", options = NULL)
                            ),
                            h4(
                              wellPanel(
                                fluidRow(
                                  column(3,
                                         sliderInput("infect", "Prevalence (Per 1000 People)", min = 1, max = 100, step = 1, value = 5),
                                         bsPopover("infect", "Prevalence (Per 1000 People)","The average number of people (per 1000) that has the disease.",
                                                   placement = "bottom", trigger = "click", options = NULL),
                                         sliderInput("spec", "Specificity", min = 0.5, max = 0.999, value = 0.99, step = 0.001),
                                         bsPopover("spec", "Specificity","The probability of someone who <b>does not have</b> the disease testing positive for it.",
                                                   placement = "bottom", trigger = "click", options = NULL),
                                         sliderInput("sens", "Sensitivity", min = 0.5, max = 0.999, value = 0.995, step = 0.001),
                                         bsPopover("sens", "Sensitivity","The probability of someone who <b>has</b> the disease testing positive for it.",
                                                   placement = "bottom", trigger = "click", options = NULL),
                                         bsButton("new", " Generate New Sample", icon("retweet"))
                                         
                                  ),
                                  column(6,
                                         plotOutput("plot1"),
                                         bsPopover("plot1", "Sample Results","The points show a sample of 1000 people from the population. All are tested for the disease, and the results are displayed by the size and color of dot.",
                                                   placement = "bottom", trigger = "click", options = NULL)
                                  ),
                                  column(3, align = "center",
                                         h2("Challenge:"),
                                         textOutput("question"),
                                         br(),
                                         bsButton("ques", "New Challenge"),
                                         br(),
                                         br(),
                                         textOutput("sample_ans"),
                                         br(),
                                         bsButton("show_ans", "Show/Hide Sample Answer")
                                         )),
                                fluidRow(
                                  # column(12,align="center",
                                  #        uiOutput('formular')),
                                  column(12, align = "center",
                                         textOutput("result"))
                                  
                                ),
                                fluidRow(
                                  column(12, align="center",
                                         uiOutput('calculation')),
                                  br(),
                                  column(3, 
                                         checkboxInput('pop_result', 'Show Theoretical Result', value = TRUE))
                                )
                                )))
                              )
                            )
                            )))