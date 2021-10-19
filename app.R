# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(dplyr)
library(boastUtils)
library(ggplot2)

# Load files ----
questionBank <- read.csv("questionbank.csv", header =  TRUE)

# Global Constant ----
baseData <- data.frame(
  x = rep(1:40, each = 25),
  y = rep(1:25, times = 40)
)

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "blue",
    ## Header ----
    dashboardHeader(
      title = "Bayes' Theorem",
      titleWidth = 250,
      tags$li(
        class = "dropdown",
        actionLink("info", icon("info"))
      ),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Bayes_Theorem")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prereq", icon  = icon("book")),
        menuItem("Challenge", tabName = "challenge", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview----
        tabItem(
          tabName = "overview",
          h1("Bayes' Theorem App Overview"),
          br(),
          p("This app is designed to demonstrate Bayes' Theorem using the
            classic example of disease incidence."),
          br(),
          h2("Instructions"),
          p("Adjust the sliders to help you solve the challenges."),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goPrereq",
              label = "Read the prerequisites",
              size = "large",
              icon = icon("bolt")
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app was originally created by Sam Messer in 2018, with updates
            by Yiyang Wang (2019) and Kellien Pertiz (2021).",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 10/15/2021 by NJH.")
          )
        ),
        ### Prerequisites ----
        tabItem(
          tabName = "prereq",
          withMathJax(),
          h2("Background of Bayes' Theorem"),
          br(),
          p("For two events denoted \\(D\\) and \\(T\\), Bayes' Theorem relates
            the probability of \\(D\\) occuring given that \\(T\\) occurred
            (expressed as \\(P(D|T)\\)) to the probability that \\(T\\) occurs
            given that we know \\(D\\) occurred (\\(P(T|D)\\)) through the formula:
            \\[\\begin{align} P(D|T) &=\\frac{P(T|D)\\cdot P(D)}{P(T|D)\\cdot
            P(D) + P(T|D^C)\\cdot P(D^C)} \\\\
            &= \\frac{\\text{Sensitivity} \\cdot \\text{Prevalence}}
            {\\left(\\text{Sensitivity}\\cdot\\text{Prevalence}\\right) +
            \\left(1-\\text{Specificity}\\right)\\cdot
            \\left(1-\\text{Prevalence}\\right)}\\end{align}\\]
            where \\(D^C\\) is the complement of \\(D\\)."
          ),
          p("In the screening test example used in this application, we define:"),
          tags$ul(
            tags$li("\\(D\\) to be the event that a person has the disease of
                    interest"),
            tags$li("\\(T\\) to be the event that a person tests positive for
                    the disease of interest"),
            tags$li("the probability of a person having the disease of interest,
                    \\(P(D)\\), is prevalence of the disease"),
            tags$li("the probability of a person testing positive given they have
                    the disease of interest, \\(P(T|D)\\), is test's sensitivity"),
            tags$li("the probability of a person testing negative when they do
                    not have the disease of interest, \\(P(T^C|D^C)\\), is the
                    test's specificity")
          ),
          br(),
          div(style = "text-align: center;",
              bsButton(
                inputId = "goChallenge",
                label = "Go to the Challenge",
                size = "large",
                icon = icon("bolt")
              )
          )
        ),
        ### Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("Use the sliders to adjust the disease's prevalence, and the test's
            specificty and sensitivity to match the challenge given. You can
            generate a sample of 1000 people based upon your slider values and
            compare the sample results to calculations from Bayes' Theorem."),
          p("As you adjust the sliders, think about how each one affects the
            probability."),
          fluidRow(
            column(
              width = 5,
              offset = 0,
              wellPanel(
                h3("Challenge"),
                textOutput("question"),
                br(),
                bsButton(
                  inputId = "showAnswer",
                  label = "Show Sample Answer",
                  size = "large",
                  type = "toggle"
                ),
                textOutput("sampleAnswer"),
                br(),
                bsButton(
                  inputId = "newQuestion",
                  label = "New Challenge",
                  size = "large"
                ),
                br(),
                br(),
                sliderInput(
                  inputId = "prevalence",
                  label = "Prevalence (per 1000 people)",
                  min = 1,
                  max = 100,
                  step = 1,
                  value = 5
                ),
                sliderInput(
                  inputId = "sensitivity",
                  label = "Sensitivity",
                  min = 0.5,
                  max = 0.999,
                  value = 0.995,
                  step = 0.001
                ),
                sliderInput(
                  inputId = "specificity",
                  label = "Specificity",
                  min = 0.5,
                  max = 0.999,
                  value = 0.99,
                  step = 0.001
                )
              )
            ),
            column(
              width = 7,
              offset = 0,
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "newSample",
                  label = "Generate New Sample",
                  icon = icon("retweet"),
                  size = "large"
                )
              ),
              br(),
              plotOutput("newPlot"),
              # plotOutput("plot1") %>%
              #   withSpinner(color = boastPalette[1]),
              ## Text for graph
              ### "The points show a sample of 1000 people from the
              ### population. All are tested for the disease, and the
              ### results are displayed by the size and color of dot."
              br(),
              uiOutput("sampleResults")
            )
          ),
          checkboxInput(
            inputId = "theoryCalc",
            label = "Show Theoretical Result",
            value = TRUE
          ),
          uiOutput('calculation')
        ),
        ### References ----
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
  
  ## Define user variables ----
  sampleData <- reactiveVal(NULL)
  currentQID <- reactiveVal(sample.int(n = nrow(questionBank), size = 1))
  

  ## Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "Use the sliders to explore Bayes' Theorem and match the displayed
        challenge",
        type = "info"
      )
  })

  ## Go to prereq's button ----
  observeEvent(
    eventExpr = input$goPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prereq"
      )
    }
  )

  ## Go to challenge button ----
  observeEvent(
    eventExpr = input$goChallenge,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "challenge"
      )
    }
  )

  #Initialize the counts at 0
  # t_neg = 0
  # t_pos = 0
  # f_neg = 0
  # f_pos = 0



  #Generate new sample, changes who has the disease
  # pick <- reactive({
  #   input$newSample
  #   rnorm(1000)
  # })

  ## Display and update challenge ----
  output$question <- renderText({
    questionBank$question[currentQID()]
    })
  
  observeEvent(
    eventExpr = input$newQuestion,
    handlerExpr = {
      currentQID(sample.int(n = nrow(questionBank), size = 1))
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Display sample answer ----
  observeEvent(
    eventExpr = input$showAnswer, 
    handlerExpr = {
      if (!input$showAnswer) {
        output$sampleAnswer <- NULL
        updateButton(
          session = session,
          inputId = "showAnswer",
          label = "Show Sample Answer"
        )
        # v <<- FALSE
      } else {
        updateButton(
          session = session, 
          inputId = "showAnswer",
          label = "Hide Sample Answer"
        )
        output$sampleAnswer <- renderText(questionBank$sampleAnswer[currentQID()])
        # v <<- TRUE
      }
    })
  
  ## Create data ----
  ### Sample size is fixed at 1000
  ### Two stage approach
  #### Stage 1: Use prevalence info for who does/doesn't have disease
  #### Stage 2: use sensitivity & specificity for FP, FN, TP, TN
  observeEvent(
    eventExpr = input$newSample,
    handlerExpr = {
      tempData <- baseData
      tempData$probDisease <- runif(1000, min = 0, max = 1)
      tempData$probTest <- runif(1000, min = 0, max = 1)
      tempData <- tempData %>%
        mutate(
          hasDisease = ifelse(
            test = probDisease <= input$prevalence/1000,
            yes = "Disease",
            no = "No"
          )
        ) %>%
        mutate(
          status = case_when(
            hasDisease == "Disease" & probTest <= input$sensitivity ~ "True Positive",
            hasDisease == "Disease" & probTest > input$sensitivity ~ "False Negative",
            hasDisease == "No" & probTest <= input$specificity ~ "True Negative",
            hasDisease == "No" & probTest > input$specificity ~ "False Positive",
            TRUE ~ "error"
          )
        )
      sampleData(tempData)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Display sample plot ----
  output$newPlot <- renderPlot(
    expr = {
      validate(
        need(
          expr = !is.null(sampleData()$status),
          message = "Click on Generate New Sample to create a plot"
        )
      )
      ggplot(
        data = sampleData(),
        mapping = aes(x = x, y = y, color = status, shape = status)
      ) +
        geom_point(size = 3) +
        theme_void() +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        ) +
        labs(
          title = "Sample of 1000 People from the Population"
        ) +
        guides(
          color = guide_legend(title = NULL, nrow = 2),
          shape = guide_legend(title = NULL, nrow = 2)
        ) +
        scale_color_manual(
          values = c(
            "True Positive" = psuPalette[3],
            "False Negative" = psuPalette[2],
            "True Negative" = psuPalette[5],
            "False Positive" = psuPalette[1]
          )
        ) +
        scale_shape_manual(
          values = c(
            "True Positive" = 17,
            "False Negative" = 16,
            "True Negative" = 18,
            "False Positive" = 15
          )
        )
    },
    alt = "The points show a sample of 1000 people from the population. All are
    tested for the disease, and the results are displayed by the shape and color
    of dot."
  )

  ## Display sample results ----
  output$sampleResults <- renderUI({
    if (is.null(sampleData()$status)) {
      NULL
    } else {
      freqs <- table(sampleData()$status)
      freqs[is.na(freqs)] <- 0
      p("There were", freqs["False Positive"] + freqs["True Positive"], "positive
      test results, of which ", freqs["True Positive"], "people actually had the
      disease. Based upon this sample, the estimated probability of having the
      disease given a postivie test result is", paste0(round(
        freqs["True Positive"] / (freqs["False Positive"] + freqs["True Positive"]),
        digits = 3) * 100, "%."))
    }
  })

  ## Theoretical results ----
  output$calculation <- renderUI({
    if (input$theoryCalc) {
      withMathJax(
        sprintf(
          fmt = "\\[\\begin{align}P(Disease|Positive) &=
            \\frac{Sensitivity * Prevalence}
            {Sensitivity * Prevalence + (1 - Specificity) * (1 - Prevalence)} \\\\
            &= \\frac{%.3f * %.3f} {%.3f * %.3f + (1 - %.3f) * (1 - %.3f)} \\\\
            &= %.3f\\end{align}\\]",
          input$sensitivity,
          input$prevalence/1000,
          input$sensitivity,
          input$prevalence/1000,
          input$specificity,
          input$prevalence/1000,
          (input$sensitivity * (input$prevalence/1000)) / ((input$sensitivity * (input$prevalence/1000)) + ((1 - input$specificity) * (1 - (input$prevalence/1000))))
        )
      )
    } else {
      NULL
    }
  })

  # ## The main display ----
  # output$plot1 <- renderPlot({
  #   par(mar = c(0.1,0.1,1,0.1))
  # 
  #   #Draw an empty plot with no outside box or axes
  #   plot(x = NULL, y = NULL,
  #        xlim = c(0, 43),
  #        ylim = c(-5, 25),
  #        xaxt="n",
  #        yaxt="n",
  #        xlab = "", ylab = "", main = "Sample of 1000 People from this Population",
  #        bty = "n")
  #   #Initialize the iterative variable. This will be the index to access in our lists.
  #   k = 1
  #   #Generate two lists: pickdata is a list that determines whether each individual has the disease, while test determines
  #   #test result, based on whether or not they have the disease
  #   pickdata <- pick()
  #   test<- rnorm(1000)
  # 
  #   for (i in c(1:40)) {
  #     for (j in c(1:25)) {
  #       if (pickdata[k] > qnorm(1 - (input$prevalence / 1000))) {
  #         #Assign disease
  #         if (test[k] > qnorm(1 - input$sensitivity)) {
  #           #Assign test result if they have disease
  #           points(i, j, pch = 21, col = boastPalette[8], bg = psuPalette[8], cex = 1.75)
  #           t_pos <<- t_pos + 1
  #         } else {
  #           #SHow false negative
  #           points(i, j, pch = 19, col = psuPalette[2], cex = 1.75)
  #           f_neg <<- f_neg + 1
  #         }
  #       } else {
  #         if (test[k] > qnorm(input$specificity)) {
  #           #Assign test result if they don't have the disease
  #           points(i, j, pch = 19, col = boastPalette[1], cex = 1.75)
  #           f_pos <<- f_pos + 1
  #         } else {
  #           points(i, j, pch = 19, col = boastPalette[7], cex = 0.75)
  #           t_neg <<- t_neg + 1
  #         }
  #       }
  #       k = k + 1
  #     }
  #   }
  # 
  #   legend(x = "bottom",
  #          legend = c(paste0("True Negative (", t_neg, ")"),
  #                     paste0("True Positive (", t_pos, ")"),
  #                     paste0("False Negative (", f_neg, ")"),
  #                     paste0("False Positive (", f_pos, ")")),
  #          horiz = FALSE,
  #          bty = "n",
  #          pch = 21,
  #          col = c(boastPalette[7], boastPalette[1], psuPalette[2], boastPalette[1]),
  #          pt.cex = c(0.75, 1.75, 1.75, 1.75),
  #          pt.bg = c(boastPalette[7], psuPalette[8], psuPalette[2], boastPalette[1]),
  #          ncol = 2)
  # 
  #   t__pos = t_pos
  #   f__pos = f_pos
  # 
  #   #Test result message with displays for 0 and 1 positive result
  #   output$result <- renderText({
  #     if ((t_pos + f_pos) == 1) {
  #       sprintf("There was 1 positive result, of which %s actually had the disease.
  #               This gives a sample estimate of a %2.f %% chance of actually having the disease if one tests positive for it.",
  #               t__pos, ((t__pos / (t__pos + f__pos)) * 100))
  #     } else if ((t__pos + f__pos) == 0) {
  #       sprintf("There were 0 positive results.")
  #     } else {
  #       sprintf("There were %s positive results, of which %s actually had the disease.
  #               This gives a sample estimate of a %2.f %% chance of actually having the disease if one tests positive for it.",
  #               t__pos + f__pos, t__pos, ((t__pos / (t__pos + f__pos)) * 100))
  #     }
  #   })
  # 
  #   #Reset values for all possible test results
  #   t_neg <<- 0
  #   t_pos <<- 0
  #   f_neg <<- 0
  #   f_pos <<- 0
  # 
  # })

  # numbers <- reactiveValues(question=c())
  # 
  # num_qs <- length(questionBank$question)
  # 
  # numbers$question = 1
  # 
  # counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object

  ## Challenges ----

  # observeEvent(input$ques, {
  # 
  #   observe({
  #     numbers$question=sample(1:num_qs,1)
  #   })
  # 
  #   counter$countervalue <- 0
  #   output$sampleAnswer <- renderText("")
  # })

  
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
  # v<<-FALSE
  # observeEvent(input$prevalence | input$specificity | input$sensitivity | input$ques | input$showAnswer | input$showAnswer,{
  #   statement <- rlocker::createStatement(
  #     list(
  #       verb = list(
  #         display = "interacted"
  #       ),
  #       object = list(
  #         id = paste0(getCurrentAddress(session), "#", numbers$question),
  #         name = paste('Question', numbers$question),
  #         description = questionBank[numbers$question, 2]
  #       ),
  #       result = list(
  #         success = NA,
  #         response = paste(input$prevalence, input$specificity, input$sensitivity, v)
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
