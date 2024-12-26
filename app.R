#
# This is a Shiny web application which illustrates the interactions of various
# choices of generation, storage, and baseload capacity.

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(markdown)
library(plotly)
library(RcppRoll)

comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")
markdownFile<-function(filename) {
  t<-read_file(pipe(paste0("cat m4defsnull.txt ",filename," | m4 ")))
  #t<-read_file(pipe(paste0("cat m4defs.txt ",filename," | m4 ")))
  markdown(t)
}
options(scipen=999)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Grid impacts workbook"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
