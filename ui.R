library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(dplyr)
library(shinycssloaders)

shinyUI(fluidPage(
  
  withMathJax(),
  
  dashboardPage(skin = "blue",
                dashboardHeader(title="Bayes' Theorem",titleWidth=250,
                                tags$li(class="dropdown",
                                        tags$a(href='https://shinyapps.science.psu.edu/',
                                               icon("home", lib = "font-awesome"))),
                                tags$li(class="dropdown",
                                        actionLink("info",icon("info"),class="myClass")),
                                tags$li(class="dropdown",
                                        actionLink("hint",icon("question"),class="myClass"))),
  
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                    menuItem("Pre-requisites", icon  = icon("book"), tabName = "prereq"),
                    menuItem("Explore", icon = icon("wpexplorer"), tabName = "explore")
                  )
                ),
                dashboardBody(
                  tags$head( 
                    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
                  ),
                  tabItems(
                    tabItem(tabName = "overview",
                            tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                            br(),br(),br(),
                            h3(strong("About:")),
                            h4("This app is designed to demonstrate Bayes' Theorem using the classic example of disease incidence."),
                            h3(strong("Instructions:")),
                            h4("Adjust the sliders to help you solve the challenges."),
                            br(),
                            div(style = "text-align: center",
                                bsButton("go", "GO!", size = "large", icon = icon("bolt"),class="circle grow")),
                            h3(strong("Acknowledgements:")),
                            h4("This app was developed and coded by Sam Messer and improved by Yiyang Wang.")
                    ),
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
                                bsButton("goover", "Go to the Pre-requisites", size = "large", icon = icon("bolt"),class="circle grow"))
                            ),
                    tabItem(tabName = "explore",
                            h4(
                              wellPanel(
                                fluidRow(
                                  column(3,
                                         sliderInput("infect", "Prevalence (Per 1000 People)", min = 1, max = 100, step = 1, value = 5),
                                         bsPopover("infect", "Prevalence (Per 1000 People)","The average number of people (per 1000) that has the disease.",
                                                   placement = "bottom", options = NULL),
                                         sliderInput("spec", "Specificity", min = 0.5, max = 0.999, value = 0.99, step = 0.001),
                                         bsPopover("spec", "Specificity","The probability of someone who <b>does not have</b> the disease testing positive for it.",
                                                   placement = "bottom", options = NULL),
                                         sliderInput("sens", "Sensitivity", min = 0.5, max = 0.999, value = 0.995, step = 0.001),
                                         bsPopover("sens", "Sensitivity","The probability of someone who <b>has</b> the disease testing positive for it.",
                                                   placement = "bottom", options = NULL),
                                         bsButton("new", " Generate New Sample", icon("retweet"))
                                         
                                  ),
                                  column(6,
                                         plotOutput("plot1")%>% withSpinner(color="#337ab7"),
                                         bsPopover("plot1", "Sample Results","The points show a sample of 1000 people from the population. All are tested for the disease, and the results are displayed by the size and color of dot.",
                                                   placement = "bottom", options = NULL)
                                  ),
                                  column(3, align = "center",
                                         wellPanel(style = "background-color: #C8E4F5",
                                         h2("Challenge:"),
                                         textOutput("question"),
                                         br(),
                                         bsButton("ques", "New Challenge"),
                                         br(),
                                         br(),
                                         textOutput("sample_ans"),
                                         br(),
                                         bsButton("show_ans", "Show Sample Answer")
                                         ))),
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