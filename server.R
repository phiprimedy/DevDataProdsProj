library(shiny)
library(UsingR)
data(galton)
model <- lm(child ~ parent, data=galton)

calcMidParentHeight <- function(fatherheight, motherheight) {
  (fatherheight + 1.08*motherheight)/2.0
}

predictChildHeight <- function(midParentHeight) {
  predict(model, data.frame(parent=c(midParentHeight)), 
    interval="confidence")
}

shinyServer(
  function(input, output) {
    output$inputValueFather <- renderText({input$fatherheight})
    output$inputValueMother <- renderText({input$motherheight})
    midParentHeight <- reactive({calcMidParentHeight(input$fatherheight,
                                                    input$motherheight)}
                      )
    output$midParentHeight <- renderText(midParentHeight())
    
    predictedChildHeight <- reactive({
      predict(model, data.frame(parent=c(midParentHeight())), interval="confidence")})
    
    output$prediction <- renderPrint({predictedChildHeight()})
    
    output$fitplot <- renderPlot({
      plot(galton$parent, galton$child, xlab="Midparent height(inches)",
           xlim=c(60, 80),
           ylim=c(60, 80),
           ylab="Child's height (inches)",
           main="Galton data set: Linear Regression fit")
      #lines(galton$parent, model$fitted, col="red", lwd=3)
      abline(coef=model$coef, col="red", lwd=3)
      abline(v=midParentHeight(), col="blue", lwd=1)
      abline(h=predictedChildHeight()[[1]], col="blue", lwd=1)
    })
    output$parentplot <- renderPlot({
      plot(density(galton$parent), xlab="Midparent height(inches)",
           main="Frequency Distribution of Midparent height", xlim=c(60, 80))
      abline(v=mean(galton$parent), col="blue", lwd=3)
      text(mean(galton$parent)+1, 0, "mean", col = "blue")
      abline(v=midParentHeight(), col="red", lwd=1)
      text(midParentHeight()-0.5, 0.1, "input", col = "red")
    })
    output$childplot <- renderPlot({
      plot(density(galton$child), xlab="Child height(inches)",
           main="Frequency Distribution of Child height", xlim=c(60, 80))
      abline(v=mean(galton$child), col="blue", lwd=3)
      text(mean(galton$child)+1, 0, "mean", col = "blue")
      abline(v=predictedChildHeight()[[1]], col="red", lwd=1)
      text(predictedChildHeight()[[1]]-1.0, 0.1, "predicted", col = "red")
    })
  }
)

