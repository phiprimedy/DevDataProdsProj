library(shiny)

shinyUI(pageWithSidebar(
    # Application title
    headerPanel("Child Height Prediction using Galton data"),
    sidebarPanel(
      
      sliderInput("fatherheight", 
                   "Father's height (inches)", 
                   as.integer(mean(galton$parent)), 
                   min = as.integer(0.95 * min(galton$parent)), 
                   max = as.integer(1.05 *max(galton$parent)),
                   step = 1),
      sliderInput("motherheight", 
                   "Mother's height (inches)", 
                   as.integer(mean(galton$parent)/1.08), 
                   min = as.integer(0.95 * min(galton$parent)), 
                   max = as.integer(1.05 *max(galton$parent)),
                   step = 1),
      br(),
      submitButton("Submit"),
      br(),
      br(),
      h4("Instructions"),
      em("Use the sliders above to select the father's and mother's height and then press Submit."),
      em("Observe the results on the right.")
    ),
    
    mainPanel(
      h3("Input"),
      p("Father's height and mother's height were entered as "), 
      strong(textOutput("inputValueFather")),
      p(" inches and "),
      strong(textOutput("inputValueMother")),
      p(" inches respectively."),
      br(),
      p("This implies a midparent height of "),
      strong(textOutput("midParentHeight")),
      p(" inches."),
      br(),
      em("Note: The midparent height is an average of the "),
      em("father's height and 1.08 times the mother's height. It is computed based on your inputs."),
      h3("Linear Model Plot"),
      plotOutput("fitplot"),
      h3("Prediction for Child Height"),
      strong("fit: Fitted value"), br(),
      strong("lwr,upr: Lower and upper bounds of 95% confidence interval"),br(),
      em("(Values are in inches)"),
      br(),br(),
      verbatimTextOutput("prediction"),
      h3("Frequency distributions"),
      em("The purpose of the following plots is to illustrate Regression toward the mean."),
      em("You will notice that the predicted child height is typically"),
      em("closer to the mean than the parents height."),
      em("Although this is not a continuous dataset strictly speaking, we still use frequency density plots "),
      em("instead of histograms for convenience."),
      plotOutput("parentplot"),
      plotOutput("childplot")
    )
  ) 
 )