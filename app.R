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

#-----------------------------------------------------
# Datasets
#-----------------------------------------------------
dataSets<-c(
  "(VIC) WE 25 January 2024"="openNem-VIC-25-01-24-7D.csv",
  "WE 16 May 2024"="openNem-SA-16-05-24-7D.csv",
  "(VIC) WE 16 May 2024"="openNem-VIC-16-05-24-7D.csv",
  "(NEM) WE 16 May 2024"="openNem-NEM-16-05-24-7D.csv",
  "June 2024"="openNEMMerge-June-2024.csv",
  "June 2024 (1st week only)"="openNEMMerge-June-1stWeek-2024.csv",
  "WE 30 January 2024"="openNem-SA-30-01-24-7D.csv",
  "WE 30 November 2023"="opennem-30-11-2023sa5.csv",
  "First heatwave, Dec 2019"="openNem-SA-21-12-19-7D.csv",
  "Second heatwave, Dec 2019"="openNem-SA-28-12-19-7D.csv",
  "March heatwave, 2024"="openNem-SA-12-03-24-7D.csv"
)
dataSetTitles<-c(
  "(VIC) WE 25 January 2024"="Electricity renewable/demand/curtailment/shortfall\n(Victoria) Week ending 25 Jan 2024",
  "WE 16 May 2024"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 16 May 2024",
  "(VIC) WE 16 May 2024"="Electricity renewable/demand/curtailment/shortfall\nVIC Week ending 16 May 2024",
  "(NEM) WE 16 May 2024"="Electricity renewable/demand/curtailment/shortfall\nNEM Week ending 16 May 2024",
  "June 2024"="Electricity renewable/demand/shortfall\nJune 2024",
  "June 2024 (1st week only)"="Electricity renewable/demand/shortfall\n1st Week June 2024",
  "WE 30 January 2024"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 30 Jan 2024",
  "WE 30 November 2023"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 30 November 2023",
  "First heatwave, Dec 2019"="Electricity renewable/demand/curtailment/shortfall\nHeatwave, WE 21 December 2019",
  "Second heatwave, Dec 2019"="Electricity renewable/demand/curtailment/shortfall\nHeatwave, WE 28 December 2019",
  "March heatwave, 2024"="Electricity renewable/demand/curtailment/shortfall\nHeatwave, WE 12 March 2024"
)
#-----------------------------------------------------
# End Datasets
# Check Datasets
#-----------------------------------------------------
for (i in dataSets) {
  if (!file.exists(i)) {
    cat("Missing: ",i,"\n") 
  }
} 



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
