library(shiny)
library(splines)
data<-data("mtcars")

ui <- fluidPage(
  titlePanel("Interactive 2D Spline Plot for mtcars data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x_coord", "Miles per gallon:", min = 11, max = 33.9, value =14.3),
      sliderInput("y_coord", "Horsepower:", min = 52, max = 335, value = 245)
    ),
    mainPanel(
      plotOutput("spline_plot")
    )
  )
)

server <- function(input, output) {
  # Initial data points
  data <- data.frame(x = mtcars$mpg, 
                     y = mtcars$hp)
  
  observe({
    # Get the updated data point position
    new_x <- input$x_coord
    new_y <- input$y_coord
    
    # Update the data point
    data[7, ] <- c(new_x, new_y)
    
    # Fit a spline to the updated data
    spline_fit <- smooth.spline(data$x, data$y, df=10)
    
    # Create the plot
    output$spline_plot <- renderPlot({
      plot(data$x, data$y, pch = 19, col = "cornflowerblue", xlim = c(10.4, 33.9), ylim = c(52, 335),xlab = "Miles per gallon", ylab = "Horsepower")
      lines(spline_fit, col = "red")
      abline(v = new_x, h = new_y, lty = 2)
      title(main = "Interactive spline plot for hp vs. mpg")
      points(new_x, new_y, pch = 19, col = "green", cex = 1.5)
    })
  })
}

shinyApp(ui, server)
