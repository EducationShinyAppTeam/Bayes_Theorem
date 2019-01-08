library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)

bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
  #go to overview
  observeEvent(input$goover,{
    updateTabItems(session, "tabs", "overview")
  })
  #Initialize the counts at 0
  t_neg = 0
  t_pos = 0
  f_neg = 0
  f_pos = 0
  
  #GO button on overview page
  observeEvent(input$go, {
      updateTabItems(session, "tabs", "explore")
  })
  
  #Generate new sample, changes who has the disease
  pick <- reactive({
    input$new
    rnorm(1000)
  })
  
  #The main display in the center of the window
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
        if (pickdata[k] > qnorm(1 - (input$infect / 1000))) { #Assign disease
          if (test[k] > qnorm(1 - input$sens)) { #Assign test result if they have disease
            points(i, j, pch = 21, col = "blue", bg = "green", cex = 1.75)
            t_pos <<- t_pos + 1
          } else {
            points(i, j, pch = 19, col = "red", cex = 1.75) #SHow false negative
            f_neg <<- f_neg + 1
          }
        } else {
          if (test[k] > qnorm(input$spec)) { #Assign test result if they don't have the disease
            points(i, j, pch = 19, col = "blue", cex = 1.75)
            f_pos <<- f_pos + 1
          } else {
            points(i, j, pch = 19, col = "black", cex = 0.75)
            t_neg <<- t_neg + 1
          }
        }
        k = k + 1
      }
    }
    
    legend(x = "bottom", legend = c(paste0("True Negative (", t_neg, ")"),
                                    paste0("True Positive (", t_pos, ")"),
                                    paste0("False Negative (", f_neg, ")"),
                                    paste0("False Positive (", f_pos, ")")),
           horiz = FALSE, bty = "n", pch = 21, col = c("black", "blue", "red", "blue"),
           pt.cex = c(0.75, 2, 2, 2), pt.bg = c("black", "green", "red", "blue"), ncol = 2)
    
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
    
  }, bg = "#F5F5F5")
  
  numbers <- reactiveValues(question=c())
  
  num_qs <- length(bank$question)
  
  numbers$question = 1
  
  output$question <- renderText(bank[numbers$question, 2])
  
  counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object
  
  observeEvent(input$show_ans, {
    counter$countervalue <- counter$countervalue + 1
    if ((counter$countervalue %% 2) == 0) {
      output$sample_ans <- renderText("")
    }
    else {
      output$sample_ans <- renderText(bank[numbers$question, 3])
    }
  })
  
  #Challenges
  observeEvent(input$ques, {
    
    observe({
      numbers$question=sample(1:num_qs,1)
    })
    
    counter$countervalue <- 0
    output$sample_ans <- renderText("")
  })
    
  #SHow calculation
  output$calculation <- renderUI({
    if (!input$pop_result) return()
    withMathJax(
      helpText(sprintf('$$\\small{P(Disease|Positive) = 
                        \\frac{Sensitivity * Prevalence}
                        {Sensitivity * Prevalence
                        + (1 - Specificity) * (1 - Prevalence)} =
                        \\frac{%s * %0.3f}
                        {%s * %0.3f + (1 - %s) 
                        * (1 - %0.3f)} = %1.3f}$$', 
                        input$sens, input$infect/1000, input$sens, input$infect/1000, input$spec, input$infect/1000,
                        (input$sens * (input$infect/1000)) / 
                        ((input$sens * (input$infect/1000)) + ((1 - input$spec) * (1 - (input$infect/1000))))))
    )
  })
  
  output$calculation_part <- renderUI({
    withMathJax(
      helpText(sprintf('$$P(D|T) = 
                        \\frac{P(T|D)*P(D)}
                        {P(T|D)*P(D)+P(T|D^{c})*P(D^{c})}$$')))
  })
})


          
