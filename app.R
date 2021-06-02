# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(dplyr)
library(shinycssloaders)
library(rlocker)
library(boastUtils)

# Load additional dependencies and setup functions
bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

# source("global.R")

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Bayes' Theorem", 
      titleWidth = 250,
      tags$li(
        class = "dropdown", 
        actionLink("info",icon("info"))
      ),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Bayes_Theorem"
        )
      ),
      tags$li(class = "dropdown", 
              tags$a(href='https://shinyapps.science.psu.edu/', 
                     icon("home", lib = "font-awesome")
              )
      )
    ),
    
    ### Create the sidebar navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prereq", icon  = icon("book")),
        menuItem("Challenge", tabName = "challenge", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          h1("Bayes' Theorem App Overview"), 
          br(), 
          h2("About:"),
          p("This app is designed to demonstrate Bayes' Theorem using the 
            classic example of disease incidence."),
          br(), 
          h2("Instructions:"), 
          p("Adjust the sliders to help you solve the challenges."), 
          br(),
          div(style = "text-align: center",
              bsButton(
                inputId = "goprereq", 
                label = "Read the prerequisites", 
                size = "large", 
                icon = icon("bolt")
              )
          ), 
          br(),
          br(), 
          h2("Acknowledgements:"), 
          tags$li("Created by Sam Messer, 2018."),
          tags$li("Updated by Yiyang Wang, 2019."),
          tags$li("Improved by Kellien Peritz, 2021.")
        ),
        
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prereq",
          withMathJax(),
          h2("Background of Bayes' Theorem"),
          br(),
          h4("For two events D and T, Bayes' Theorem relates P(D|T) to P(T|D) through:"),
          div(style = "text-align: center;",
              uiOutput('calculation_part')
          ),
          br(),
          h4("In the screening test example used in this application, we define:"),
          tags$ul(
            tags$li("D = Has the disease"),
            tags$li("T = Tests positive for the disease"),
            tags$li("P(D) is called the Prevalence"),
            tags$li("P(T|D) is called the Sensitivity"),
            tags$li("P(Not T|Not D) is called the Specificity")
          ),
          br(),
          div(style = "text-align: center;",
              bsButton(
                inputId = "gochallenge",
                label = "Go to the Challenge",
                size = "large",
                icon = icon("bolt")
              )
          )
        ),
        #### Set up a Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Challenge Yourself"), 
          fluidRow(
            column(
              width = 6,
              offset = 0,
              wellPanel(
                bsButton(
                  inputId = "hint",
                  label = "Show hint", 
                  icon = icon("question"),
                  size = "xs"
                ), 
                sliderInput(
                  inputId = "infect", 
                  label = "Prevalence (Per 1000 People)", 
                  min = 1, 
                  max = 100, 
                  step = 1, 
                  value = 5),
                bsPopover(
                  id = "infect", 
                  title = "Prevalence (Per 1000 People)",
                  content = "The average number of people (per 1000) that has the disease.", 
                  placement = "bottom", 
                  options = NULL),
                sliderInput(
                  inputId = "spec", 
                  label = "Specificity", 
                  min = 0.5, 
                  max = 0.999, 
                  value = 0.99, 
                  step = 0.001),
                bsPopover(
                  id = "spec", 
                  title = "Specificity",
                  content = "The probability of someone who <b>does not have</b> 
                            the disease testing positive for it.", 
                  placement = "bottom", options = NULL),
                sliderInput(
                  inputId = "sens", 
                  label = "Sensitivity", 
                  min = 0.5, 
                  max = 0.999, 
                  value = 0.995, 
                  step = 0.001),
                bsPopover(
                  id = "sens", 
                  title = "Sensitivity",
                  content = "The probability of someone who <b>has</b> the 
                            disease testing positive for it.", 
                  placement = "bottom", 
                  options = NULL),
              )
            ),
            column(
              width = 6, 
              offset = 0,
              wellPanel(
                bsButton(
                  inputId = "ques", 
                  label = "New Challenge"),
                br(), 
                br(),
                textOutput("question"),
                br(), 
                bsButton(
                  inputId = "show_ans", 
                  label = "Show Sample Answer"),
                br(),
                textOutput("sample_ans")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              offset = 0,
              wellPanel(
                plotOutput("plot1")%>% withSpinner(color="#337ab7"),
                bsPopover(
                  id = "plot1", 
                  title = "Sample Results",
                  content = "The points show a sample of 1000 people from the 
                            population. All are tested for the disease, and the 
                            results are displayed by the size and color of dot.", 
                  placement = "bottom", 
                  options = NULL),
                div(style = "text-align: center",
                    bsButton(
                      inputId = "new", 
                      label = "Generate New Sample", 
                      icon("retweet")
                    )
                )
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              offset = 0,
              wellPanel(
                checkboxInput(
                  inputId = "pop_result", 
                  label = "Show Theoretical Result", 
                  value = TRUE),
                textOutput("result"), 
                uiOutput('calculation')
              )
            )
          )
        ),
          
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. 
            R package version 0.1.6.3. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
            ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard: 
            Create Dashboards with 'Shiny'. R package version 0.7.1. Available 
            from https://CRAN.R-project.org/package=shinydashboard"
            ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. 
            (2020). shiny: Web Application Framework for R. R package version 
            1.5.0. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: 
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Available 
            from https://CRAN.R-project.org/package=shinyWidgets"
            ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

  # Define server logic ----
  server <- function(input, output, session) {
  ## Set up Rlocker
  # connection <- rlocker::connect(session, list(
  #   base_url = "https://learning-locker.stat.vmhost.psu.edu/",
  #   auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
  #   agent = rlocker::createAgent()
  # )) 
  # 
  # currentUser <- 
  #   connection$agent
  # 
  # if(connection$status != 200){
  #   warning(paste(connection$status, "\nTry checking your auth token.")) 
  # }
  
  #go to challenge
  observeEvent(
    eventExpr = input$gochallenge, 
    handlerExpr = {
    updateTabItems(
      session = session, 
      inputId = "pages", 
      selected = "challenge")
    }
  )
  
  #Initialize the counts at 0
  t_neg = 0
  t_pos = 0
  f_neg = 0
  f_pos = 0
  
  #GO button on overview page
  observeEvent(
    eventExpr = input$goprereq, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "prereq")
    }
  )
  
  #Generate new sample, changes who has the disease
  pick <- reactive({
    input$new
    rnorm(1000)
  })
  
  ## The main display ----
  output$plot1 <- renderPlot({
    
    #Draw an empty plot with no outside box or axes
    plot(x = NULL, y = NULL, 
         xlim = c(0, 43),
         ylim = c(-5, 25),
         xaxt="n",
         yaxt="n",
         xlab = "", ylab = "", main = "Sample of 1000 People from this Population",
         bty = "n")
    #Initialize the iterative variable. This will be the index to access in our lists. 
    k = 1
    #Generate two lists: pickdata is a list that determines whether each individual has the disease, while test determines
    #test result, based on whether or not they have the disease
    pickdata <- pick()
    test<- rnorm(1000)
    
    for (i in c(1:40)) {
      for (j in c(1:25)) {
        if (pickdata[k] > qnorm(1 - (input$infect / 1000))) { 
          #Assign disease
          if (test[k] > qnorm(1 - input$sens)) { 
            #Assign test result if they have disease
            points(i, j, pch = 21, col = "#6DA9FF", bg="#17FF00", cex = 1.75)
            t_pos <<- t_pos + 1
          } else {
            #SHow false negative
            points(i, j, pch = 19, col = "#FF0000", cex = 1.75) 
            f_neg <<- f_neg + 1
          }
        } else {
          if (test[k] > qnorm(input$spec)) { 
            #Assign test result if they don't have the disease
            points(i, j, pch = 19, col = "#003AFF", cex = 1.75)
            f_pos <<- f_pos + 1
          } else {
            points(i, j, pch = 19, col = "#949794", cex = 0.75)
            t_neg <<- t_neg + 1
          }
        }
        k = k + 1
      }
    }
    
    legend(x = "bottom", 
           legend = c(paste0("True Negative (", t_neg, ")"),
                      paste0("True Positive (", t_pos, ")"),
                      paste0("False Negative (", f_neg, ")"),
                      paste0("False Positive (", f_pos, ")")),
           horiz = FALSE, 
           bty = "n", 
           pch = 21, 
           col = c("#949794", "#003AFF", "#FF0000", "#003AFF"),
           pt.cex = c(0.75, 1.75, 1.75, 1.75), 
           pt.bg = c("#949794", "#17FF00", "#FF0000", "#003AFF"), 
           ncol = 2)
    
    t__pos = t_pos
    f__pos = f_pos
    
    #Test result message with displays for 0 and 1 positive result
    output$result <- renderText({
      if ((t_pos + f_pos) == 1) {
        sprintf("There was 1 positive result, of which %s actually had the disease.
                This gives a sample estimate of a %2.f %% chance of actually having the disease if one tests positive for it.",
                t__pos, ((t__pos / (t__pos + f__pos)) * 100))
      } else if ((t__pos + f__pos) == 0) {
        sprintf("There were 0 positive results.")
      } else {
        sprintf("There were %s positive results, of which %s actually had the disease.
                This gives a sample estimate of a %2.f %% chance of actually having the disease if one tests positive for it.",
                t__pos + f__pos, t__pos, ((t__pos / (t__pos + f__pos)) * 100))
      }
    }) 
    
    #Reset values for all possible test results
    t_neg <<- 0
    t_pos <<- 0
    f_neg <<- 0
    f_pos <<- 0
    
  }, 
  bg = "#F5F5F5")
  
  numbers <- reactiveValues(question=c())
  
  num_qs <- length(bank$question)
  
  numbers$question = 1
  
  output$question <- renderText(bank[numbers$question, 2])
  
  counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object
  
  observeEvent(input$show_ans, {
    counter$countervalue <- counter$countervalue + 1
    if ((counter$countervalue %% 2) == 0) {
      output$sample_ans <- renderText("")
      updateButton(session=session, inputId = "show_ans", label = "Show Sample Answer")
      v<<-FALSE
    }
    else {
      updateButton(session=session, inputId = "show_ans", label = "Hide Sample Answer")
      output$sample_ans <- renderText(bank[numbers$question, 3])
      v<<-TRUE
    }
  })
  
  ## Challenges ----
  
  observeEvent(input$ques, {
    
    observe({
      numbers$question=sample(1:num_qs,1)
    })
    
    counter$countervalue <- 0
    output$sample_ans <- renderText("")
  })
  
  output$mathTest <- renderUI({
    "\\[\\begin{align*}g(y) &= \\frac{test}{case} \\\\&= works\\end{align*}\\]"
  })
  
  #Show calculation
  output$calculation <- renderUI({
    if (!input$pop_result) return()
    withMathJax(
      sprintf(
        fmt = "\\[\\begin{align}P(Disease|Positive) &=
        \\frac{Sensitivity * Prevalence}
      {Sensitivity * Prevalence + (1 - Specificity) * (1 - Prevalence)} \\\\
      &= \\frac{%.3f * %.3f} {%.3f * %.3f + (1 - %.3f) * (1 - %.3f)} \\\\
      &= %.3f\\end{align}\\]",
      input$sens, 
      input$infect/1000, 
      input$sens, 
      input$infect/1000, 
      input$spec, 
      input$infect/1000, 
      (input$sens * (input$infect/1000)) / ((input$sens * (input$infect/1000)) + ((1 - input$spec) * (1 - (input$infect/1000))))
      )
    )
  })
  
  
  output$calculation_part <- renderUI({
    withMathJax(
      helpText(sprintf('$$P(D|T) = 
                        \\frac{P(T|D)*P(D)}
                        {P(T|D)*P(D)+P(T|D^{c})*P(D^{c})}$$')))
  })
  
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hint:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text="Think about the way each slider affects the probability"
    )
  })
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app is designed to demonstrate Bayes’ Theorem using the classic example of disease incidence.",
      type = NULL
    )
  })
  
  # ####add rlocker statement generated
  # # Gets current page address from the current session
  # getCurrentAddress <- function(session){
  #   return(paste0(
  #     session$clientData$url_protocol, "//",
  #     session$clientData$url_hostname,
  #     session$clientData$url_pathname, ":",
  #     session$clientData$url_port,
  #     session$clientData$url_search
  #   ))
  # }
  
  ####v means if the user view the sample answer of not, it displays as the last object of the response####
  v<<-FALSE
  # observeEvent(input$infect | input$spec | input$sens | input$ques | input$show_ans | input$show_ans,{
  #   statement <- rlocker::createStatement(
  #     list(
  #       verb = list(
  #         display = "interacted"
  #       ),
  #       object = list(
  #         id = paste0(getCurrentAddress(session), "#", numbers$question),
  #         name = paste('Question', numbers$question),
  #         description = bank[numbers$question, 2]
  #       ),
  #       result = list(
  #         success = NA,
  #         response = paste(input$infect, input$spec, input$sens, v)
  #       )
  #     )
  #   )
  #   # Store statement in locker and return status
  #   status <- rlocker::store(session, statement)
  #   
  #   print(statement) # remove me
  #   print(status) # remove me
  # })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
