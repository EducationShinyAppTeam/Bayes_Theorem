library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(dplyr)
library(shinycssloaders)
library(rlocker)

shinyUI(fluidPage(
  
  withMathJax(),
  
  dashboardPage(skin = c("#56B4E9"),
                dashboardHeader(
                  title = "Bayes' Theorem",
                  titleWidth = 250,
                  tags$li(class = "dropdown", actionLink("info", icon("info"))),
                  tags$li(
                    class = "dropdown",
                    boastUtils::surveyLink(name = "Bayes_Theorem")
                  ),
                  tags$li(
                    class = "dropdown",
                    tags$a(href = 'https://shinyapps.science.psu.edu/',
                           icon("home"))
                  )
                )
              ),
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                    menuItem("Prerequisites", tabName = "prereq", icon  = icon("book")),
                    menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")), 
                    menuItem("References", tabName = "references", icon = icon("leanpub"))
                  ), 
                  tags$div(
                    class = "sidebar-logo",
                    boastUtils::sidebarFooter()
                )
              ), 
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview", 
        withMathJax(),
        h3(strong("About:")), 
        h4("This app is designed to demonstrate Bayes' Theorem using the classic example of disease incidence."),
        h3(strong("Instructions:")), 
        h4("Adjust the sliders to help you solve the challenges."), 
        br(), 
        div(style = "text-align: center", 
            bsButton("go", "Read the Prerequisites", size = "large", icon = icon("bolt"),class="circle grow")), 
        h3(strong("Acknowledgements:")),
        h4("This app was developed and coded by Sam Messer and improved by Yiyang Wang.")
      ), 
      
      tabItem(tabName = "prereq", 
              h3(strong("Background: Bayes' Theorem")), 
              h4("For two events D and T, Bayes' Theorem relates P(D|T) to P(T|D) through:"), 
              fluidRow(
                column(12, align="center", 
                       uiOutput('calculation_part')
                )
              ), 
              h4("In the screening test example used in this application, we define:"), 
              h4(tags$li("D = Has the disease")), 
              h4(tags$li("T = Tests positive for the disease")), 
              h4(tags$li("P(D) is called the Prevalence")), 
              h4(tags$li("P(T|D) is called the Sensitivity")), 
              h4(tags$li("P(Not T|Not D) is called the Specificity")), 
              br(), 
              div(style = "text-align: center", 
                  bsButton("goover", "Start the Challenge", size = "large", icon = icon("bolt"),class="circle grow")
                  )
      ), 
      
      tabItem(tabName = "explore", 
              sidebarLayout(
                sidebarPanel(
                  wellPanel(
                    h2("Challenge:"),
                    textOutput("question"),
                    br(),
                    bsButton("ques", "New Challenge"),
                    br(),
                    br(),
                    textOutput("sample_ans"),
                    br(),
                    bsButton("show_ans", "Show Sample Answer"),
                    br(), 
                    circleButton(
                      inputId = "hint", 
                      icon = icon("question"), 
                      status = "myClass", 
                      size = "xs"
                      ),
                    sliderInput(
                      inputId = "infect", 
                      label = "Prevalence (Per 1000 People)", 
                      min = 1, 
                      max = 100, 
                      step = 1, 
                      value = 5
                      ),
                    bsPopover(
                      id = "infect", 
                      title = "Prevalence (Per 1000 People)", 
                      content = "The average number of people (per 1000) that has the disease.", 
                      placement = "bottom", 
                      options = NULL
                      ),
                    sliderInput(
                      inputId = "spec", 
                      label = "Specificity", 
                      min = 0.5, 
                      max = 0.999, 
                      value = 0.99, 
                      step = 0.001
                      ),
                    bsPopover(
                      id = "spec", 
                      title = "Specificity", 
                      content = "The probability of someone who <b>does not have</b> the disease testing positive for it.", 
                      placement = "bottom", 
                      options = NULL
                      ),
                    sliderInput(
                      inputId = "sens", 
                      label = "Sensitivity", 
                      min = 0.5, 
                      max = 0.999, 
                      value = 0.995, 
                      step = 0.001
                      ),
                    bsPopover(
                      id = "sens", 
                      title = "Sensitivity", 
                      content = "The probability of someone who <b>has</b> the disease testing positive for it.", 
                      placement = "bottom", options = NULL
                      ),
                    bsButton(
                      id = "new", 
                      title = " Generate New Sample", 
                      icon("retweet")
                      )
                  )
                ),
                mainPanel(
                  plotOutput("plot1")%>% withSpinner(color="#337ab7"),
                  bsPopover(
                    id = "plot1", 
                    title = "Sample Results", 
                    content = "The points show a sample of 1000 people from the population. All are tested for the disease, and the results are displayed by the size and color of dot.", 
                    placement = "bottom", options = NULL
                  ), 
                  textOutput("result"), 
                  uiOutput('calculation'), 
                  checkboxInput(
                    inputId = 'pop_result', 
                    label = 'Show Theoretical Result', 
                    value = TRUE)
                )
              )
      )
))