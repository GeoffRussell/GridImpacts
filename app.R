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
#-----------------------------------------------------
# Define various constants and functions used to process the data
#-----------------------------------------------------
fields<-c("Battery (Charging) - MW","Imports - MW","Distillate - MW","Gas (Steam) - MW","Gas (CCGT) - MW", "Gas (OCGT) - MW","Gas (Reciprocating) - MW","Battery (Discharging) - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
fieldsVIC<-c("Battery (Charging) - MW","Imports - MW","Coal (Brown) - MW","Gas (OCGT) - MW",
             "Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
fieldsNEM<-c("Battery (Charging) - MW","Coal (Brown) - MW","Gas (OCGT) - MW",
             "Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")

renewfields<-c("Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
renewfieldsVIC<-c("Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
renewfieldsNEM<-c("Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
annual<-p("SA - annual avg power 1.58GW (2023/4)")
isnem<-function(f) {
  str_detect(f,"NEM\\)")
}
isvic<-function(f) {
  str_detect(f,"VIC\\)")
}
readDataSet<-function(n) {
  print(n)
  flds<-fields
  annual<<-p("SA - annual avg power 1.58GW (2023/4)")
  avgpower<<-1.58
  if (isnem(n)) {
    annual<<-p("NEM- annual avg power 23.9GW (2023/4)")
    avgpower<<-23.9
    flds<-fieldsNEM
  }
  if (isvic(n)) {
    annual<<-p("VIC - annual avg power 5.4GW (2023/4)")
    avgpower<<-5.4
    flds<-fieldsVIC
  }
  dfdata<-read_csv(dataSets[n]) %>% 
    rename_with(~sub('date','Time',.x)) %>% 
    rename_with(~sub('  ',' ',.x))
  dfdata %>% mutate(demand=select(.,all_of(flds)) %>% apply(1,sum)) 
}
dfout<-readDataSet("WE 30 November 2023")
#-----------------------------------------------------------------
# Calc: the main function which calculates the flow of electricity
# between the generators and batteries
#-----------------------------------------------------------------
calc<-function(bmax,ofac,icsize=0,dspick,baseloadsize=0) {
  print(dataSets[dspick])
  dfout<-readDataSet(dspick)
  batteryMaxCapacity<-bmax
  dfsum <- dfout %>% mutate(
    battuse=`Battery (Discharging) - MW`,
    #imports=`Imports - MW`,
    #diesel=`Distillate - MW`,
    wind=`Wind - MW`,
    solar=`Solar (Rooftop) - MW`+`Solar (Utility) - MW`) 
  
  if (isnem(dspick)) {
    dfsum<-dfsum %>% mutate(renew=wind+solar,dblrenew=ofac*renew)
  }
  else if (isvic(dspick)) {
    dfsum<-dfsum %>% mutate(renew=wind+solar,dblrenew=ofac*renew)
  }
  else { # in SA most exports are of excess wind/solar
    dfsum<-dfsum %>% mutate(renew=wind+solar-`Exports - MW`,dblrenew=ofac*renew) 
  }
  dfsum<-dfsum %>% mutate(noBattShortfall=dblrenew-demand,cumNoBattShortfall=cumsum(dblrenew-demand))  
  if (baseloadsize>0) {
    dfsum<-dfsum %>% mutate(supply=dblrenew+baseloadsize,baseloadsize=baseloadsize)
  }
  else {
    dfsum<-dfsum %>% mutate(supply=dblrenew,baseloadsize=0)
  }
  #-------------------
  # start with battery full 
  #-------------------
  batteryStatus<-bmax
  nperiods<-length(dfsum$demand)
  dfsum$shortFall=rep(0,nperiods)
  dfsum$icExpMWh=rep(0,nperiods) # in MWh
  dfsum$batteryStatus=rep(0,nperiods)
  dfsum$batterySupplied=rep(0,nperiods) # MWh
  dfsum$throwOutMWh=rep(0,nperiods)
  dfsum$addedToBattery=rep(0,nperiods)
  lastdfsum<<-dfsum
  totalBattuse<<-sum(lastdfsum$battuse/12)
  maxNeed<-0
  dfsum$minroll20<-roll_sum(dfsum$supply,n=20,fill=0)
  write_csv(dfsum,"xxx.csv")
  for(i in 1:nperiods) {
    dfsum$shortFall[i]=0
    # spareE is in MWh
    spareE=(dfsum$supply[i]-dfsum$demand[i])/12 
    if (spareE>0) {  # Electricity exceeds demand ... add spare to battery if there is any capacity
      if (batteryStatus<batteryMaxCapacity) { # battery isn't full
        spareB=batteryMaxCapacity-batteryStatus
        addE=min(spareE,spareB)
        batteryStatus=batteryStatus+addE
        dfsum$addedToBattery[i]=addE
        leftOver=spareE-spareB
        if (leftOver>0) {
          # send out interconnector
          spareW=12*leftOver # convert to MW 
          if (spareW>icsize) {
            dfsum$icExpMWh[i]=icsize/12
            spareW=spareW-icsize
            leftOver=spareW/12
          } else {
            dfsum$icExpMWh[i]=spareW/12
            spareW=0
            leftOver=0
          }
          dfsum$throwOutMWh[i]=leftOver
        }
      }
      else {   # battery is full, discard energy
        # or send out interconnector 
        spareW=12*spareE # convert to MW 
        if (spareW>icsize) {
          dfsum$icExpMWh[i]=icsize/12
          spareW=spareW-icsize
          spareE=spareW/12
        }
        else {
          dfsum$icExpMWh[i]=spareW/12
          spareE=0
        }
        dfsum$throwOutMWh[i]=spareE
      }
    }
    if (spareE<0) { # demand exceeds generation, get from battery if any available
      needE=-spareE
      if (needE>maxNeed) {
        maxNeed<-needE
      }
      if (batteryStatus>0) {
        if (batteryStatus>=needE) { # extract from battery
          batteryStatus=batteryStatus-needE
          dfsum$batteryStatus[i]=batteryStatus
          dfsum$batterySupplied[i]=needE
        }
        else { # we have some in battery, but not enough
          dfsum$shortFall[i]=needE-batteryStatus
          dfsum$batterySupplied[i]=batteryStatus
          batteryStatus=0
        }
      }
      else { # battery empty
        dfsum$shortFall[i]=needE
      }
    }
    dfsum$batteryStatus[i]=batteryStatus
  }
  dfsum %>% mutate(cumShortMWh=cumsum(shortFall),
                   cumThrowOutMWh=cumsum(throwOutMWh),
                   cumIcExpMWh=cumsum(icExpMWh),
                   maxShortMW=max(shortFall)*12
  )
}



#-----------------------------------------------------------------
# UI
#-----------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("cerulean"),
                tags$head(tags$style(".standout-container {margin: 20px 0; padding: 20px; font-weight: bold; background-color: Teal; color: white;
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                       }")), 
                
                # Application title
                titlePanel("Grid impacts"),
                verticalLayout(
                  mainPanel(
                    tabsetPanel(type="tabs",
                                tabPanel("SA Data",
                                         markdownFile("ob0.txt"),
                                         fluidRow(align="center",imageOutput("weekpng",height=400)),
                                         markdownFile("ob1a.txt"),
                                         fluidRow(
                                           column(width=6,
                                                  sliderInput("bsize",label="Battery size in MWh", min=500,max=20000,step=500,value=500),
                                                  sliderInput("bmult",label="Battery multiplier", min=1,max=10,step=1,value=1),
                                                  checkboxInput("showBatteryStatus",label="Show batteryStatus (%)",value=FALSE),
                                                  sliderInput("icsize",label="Interconnector size (MW)", min=0,max=2000,step=100,value=0),
                                                  #            sliderInput("dfac",label="Electricity expansion factor", min=1,max=2,step=0.2,value=1),
                                                  pickerInput("datasetpick",choices=sort(names(dataSets)),selected=c("WE 30 November 2023"),multiple=FALSE,
                                                              label = 'Alternative datasets',
                                                              options = pickerOptions(
                                                                actionsBox = TRUE,
                                                                selectedTextFormat = 'static',
                                                                noneSelectedText = 'Select',
                                                              )
                                                  ) 
                                           ),
                                           column(width=6,
                                                  sliderInput("ofac",label="Overbuild factor for wind+solar", min=1,max=3,step=0.1,value=1),
                                                  sliderInput("baseloadsize",label="Baseload size (MW)", min=0,max=1800,step=600,value=0),
                                                  sliderInput("blmult",label="Baseload multiplier", min=1,max=20,step=1,value=1),
                                                  checkboxInput("showShort",label="Show shortfall (GWh)",value=FALSE),
                                                  checkboxInput("showCurtailed",label="Show dumped energy (GWh)",value=FALSE),
                                                  checkboxInput("showInterconnector",label="Show interconnector flow (GWh)",value=FALSE),
                                                  checkboxInput("showWindDemand",label="Show wind vs demand",value=FALSE)
                                           )
                                         ),
                                         fluidRow(
                                           column(width=12,
                                                  plotOutput("shortfall")
                                           ),
                                           column(width=12,
                                                  uiOutput("calcresult")
                                           )
                                         ),
                                         markdownFile("ob1b.txt")
                                ),
                                tabPanel("About",
                                         markdownFile("about.txt")
                                )
                    )
                  )
                )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$calcresult <- renderUI({
      print("UIUIUIUI")
    })
    output$shortfall <- renderPlot({
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
