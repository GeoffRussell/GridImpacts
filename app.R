#
# This is a Shiny web application which illustrates the interactions of various
# choices of generation, storage, and baseload capacity.

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(markdown)
library(plotly)
library(RcppRoll)
library(gt)
library(bslib)
library(bsicons)

comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")
markdownFile<-function(filename) {
  t<-read_file(pipe(paste0("cat m4defsnull.txt ",filename," | m4 ")))
  #t<-read_file(pipe(paste0("cat m4defs.txt ",filename," | m4 ")))
  markdown(t)
}
options(scipen=999)
tbgcolor="grey95"
tfgcolor="grey10"

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
#---------------------------------------------------------------------------------------
# Find night time bands 
#---------------------------------------------------------------------------------------
findNightTimeBands<-function(df) {
  sunbreak=0.05*max(df$`Solar (Rooftop) - MW`)  # 5 percent of max is start of day
  dark<-(df$`Solar (Rooftop) - MW`<sunbreak)    # which periods are dark
  ldld<-which(dark[-1] != dark[-length(dark)])  # where are the changes of state
  darkdf<-tribble(~t1,~t2)
  st<-2                                         
  if (dark[1]) {                                # are we starting in the night or the day?
    tmp<-tibble(t1=df$Time[1],t2=df$Time[ldld[1]])
    darkdf<-bind_rows(darkdf,tmp)
    st<-3
  }
  for (i in seq(st,length(ldld),2)) {
    tmp<-tibble(t1=df$Time[ldld[i-1]],t2=df$Time[ldld[i]])
    darkdf<-bind_rows(darkdf,tmp)
  }
  darkdf
}

#-----------------------------------------------------------------
# set up labels and colours for the plots
#-----------------------------------------------------------------
colswind<-c(
  "demand"="brown",
  "wind"="forestgreen"
)
labswind<-c(
  "Demand",
  "Wind"
)
colsshort<-c(
  "renew"="grey40",
  "dblrenew"="cyan",
  "demand"="brown"
)
labsshort<-c(
  "Overbuild",
  "Demand",
  "Wind+Solar"
)
colsbreaks<-c( "dblrenew", "demand", "renew","wind")
colslabels<-c( "Overbuild", "Demand", "Wind+Solar","Wind")
colslevels<-c(
  "dblrenew"="cyan",
  "demand"="brown",
  "renew"="grey40",
  "wind"="forestgreen"
)

colsfacilities<-c(
  "Wind"="forestgreen",
  "Solar (Utility),Wind"="cyan",
  "Solar (Utility)"="yellow",
  "Gas (Steam)"="bisque4",
  "Gas (Reciprocating)"="bisque3",
  "Gas (OCGT)"="bisque2",
  "Gas (CCGT)"="bisque1",
  "Distillate"="grey42",
  "Bioenergy (Biogas)"="brown",
  "Battery (Discharging),Solar(Utility)"="blue",
  "Battery (Discharging)"="blue"
)
cols<-c(
  "Biomass"="brown",
  "Solar"="yellow",
  "Wind"="forestgreen",
  "Nuclear"="purple",
  "Hydro"="blue",
  "kWh/Person"="bisque1",
  "IEA2050Target"="grey70"
)  

#-----------------------------------------------------------------
# Calc: the main function which calculates the flow of electricity
# between the generators and batteries
#-----------------------------------------------------------------
calc<-function(bmax,ofac,icsize=0,dspick,baseloadsize=0,gaspeak=0) {
  print(dataSets[dspick])
  gasmw<-ifelse(gaspeak>0,gaspeak*1000,0)
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
  dfsum$gasEout=rep(0,nperiods)
  dfsum$gasMWout=rep(0,nperiods)
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
      if (needE>maxNeed) { # save this for stats
        maxNeed<-needE
      }
      # Have we got anything in the battery?
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
      else { # battery empty, have we any gas?
        needMW=needE*12 
        if (gasmw) {
          if (gasmw<needMW) {
            dfsum$gasEout[i]<-gasmw/12
            needMW=needMW-gasmw
          }
          else {
            dfsum$gasEout[i]<-needE
            needMW=0
          }
          needE=needMW/12
          dfsum$gasMWout[i]<-dfsum$gasEout[i]*12
        }
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
ui <- function(request) {
    fluidPage(theme = shinytheme("yeti"),
                tags$head(
                  tags$style(
                    ".standout-container {margin: 20px 0; padding: 20px; font-weight: bold; background-color: Teal; 
                color: white; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19); } "
                  ),
                  tags$style(".grey-container {margin: 20px 0; padding: 20px; font-weight: bold; background-color: Grey; 
                color: white; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19); } "
                ),
                  tags$style(".bold-container {margin: 20px 0; padding: 20px; font-weight: bold; } "
                )
                ), 
                
                # Application title
                titlePanel("GridImpacts: Storage, overbuild and baseload  (v0.90)"),
                verticalLayout(
                  mainPanel(
                    fluidRow(
                      column(width=12,align="right",div(style="height: 100px;", bookmarkButton("Save settings as URL for sharing ")))
                    ),
                    fluidRow(
                      column(width=12,div(style="height: 30px;", " " ))
                    ),
                    tabsetPanel(type="tabs",id="tabsetpanel",
                                tabPanel("Dashboard",
                                        fluidRow(
                                          column(width=12,div(style="height: 50px;", " " ))
                                        ),
                                         fluidRow(
                                           column(width=6,
                                                  chooseSliderSkin("Shiny"),
                                                  selectInput("datasetpick",choices=sort(names(dataSets)),
                                                              selected=c("WE 30 November 2023"),
                                                              multiple=FALSE,
                                                              label = 'Datasets'
                                                  ), 
                                                  sliderInput("bsize",label="Battery size in MWh", min=500,max=20000,step=500,value=500),
                                                  sliderInput("bmult",label="Battery multiplier", min=1,max=10,step=1,value=1),
                                                  sliderInput("gaspeak",label="Gas Peakers (GW)", min=0,max=5,step=0.25,value=0.25),
                                                  sliderInput("gasmult",label="Peaker multiplier",min=1,max=5,step=1,value=1)
                                           ),
                                           column(width=6,
                                                  sliderInput("ofac",label="Overbuild factor",min=1,max=3,step=0.1,value=1),
                                                  sliderInput("baseloadsize",label="Baseload size (MW)", min=0,max=1800,step=600,value=0),
                                                  sliderInput("blmult",label="Baseload multiplier", min=1,max=20,step=1,value=1),
                                                  checkboxInput("showShort",label="Show shortfall (GWh)",value=TRUE),
                                                  checkboxInput("showCurtailed",label="Show dumped energy (GWh)",value=FALSE),
                                                  checkboxInput("showWindDemand",label="Show wind vs demand",value=FALSE),
                                                  checkboxInput("showBatteryStatus",label="Show battery charge level (%)",value=FALSE),
                                                  bsTooltip("ofac","Increase current level of wind+solar by this factor",placement="top",trigger="hover"),
                                                  bsTooltip("datasetpick","Select an alternative set of real world data",placement="top",trigger="hover")
                                           )
                                         ),
                                         fluidRow(
                                           column(width=12,
                                                  plotOutput("shortfall",height=600)
                                           )
                                         )
                                ),
                                tabPanel("Stats",
                                         fluidRow(
                                           column(width=12,
                                                  gt_output("calcparms"),
                                                  uiOutput("calcparmsexp"),
                                                  gt_output("calcoutput"),
                                                  plotOutput("onshortages"),
                                                  gt_output("calctable"),
                                                  uiOutput("calctableexp")
                                                  #,
                                                   #uiOutput("calcresult2")
                                           )
                                         )
                                ),
                                tabPanel("Quick Start",
                                         markdownFile("ob2.txt")
                                ),
                                tabPanel("Details",
                                         markdownFile("ob0.txt"),
                                         fluidRow(align="center",imageOutput("weekpng",height=400)),
                                         markdownFile("ob1a.txt"),
                                         markdownFile("ob1b.txt")
                                ),
                                tabPanel("About",
                                         markdownFile("about.txt")
                                )
                    ),width=12
                  )
            )
      )
}



# Define server logic required to draw a histogram
server <- function(ui,input, output) {
    mtheme<-theme(plot.margin=unit(c(5,0,0,0),"mm"))
    ptheme<-theme(plot.title=element_text(color="#008080",size=15,face="bold",family="Helvetica"),
                axis.text=element_text(face="bold",size=12))+mtheme
    runjs('$("#mainPanel").css("width", "1200px");')
    gendfsum<-reactive({
      print(input$datasetpick)
      bstatus<-calc(input$bsize*input$bmult,input$ofac,0,input$datasetpick,input$blmult*input$baseloadsize,input$gaspeak*input$gasmult)
      dfile<-bstatus %>%  mutate(diffE=(dblrenew-demand)/12) %>% select(Time,dblrenew,demand,diffE,batteryStatus,batterySupplied,shortFall,addedToBattery) 
      write_csv(dfile,"bcalc-output.csv")
      bstatus
    })

    output$weekpng<-renderImage(list(src="WeekEnding30-11-2023.png",height=400),deleteFile=FALSE)
    
    output$calctableexp <- renderUI({
        tags$div(
          p("The 8 hour column gives the highest amount of wind energy in any 8 hour period as will as the lowest in any 8 hour period. Similarly for the other periods, 5 minutes and 1 hour.")
        )
    })
    output$calcparmsexp <- renderUI({
        tags$div(
          p("The battery storage size (hrs) calculation uses the average hourly demand during this period, not the annual average hourly demand or the summer peak average hourly demand")
        )
    })
    
    output$calcoutput <- render_gt({
        dfsum<-gendfsum()
        nperiods<-length(dfsum$Time)
        totdemand<-dfsum %>% summarise(totdemand=sum(demand/12))
        sh<-dfsum %>% summarise(max(cumShortMWh))
        curt<-dfsum %>% summarise(max(cumThrowOutMWh))
        bsup<-dfsum %>% summarise(sum(batterySupplied))
        bmax<-dfsum %>% summarise(max(batterySupplied*12))
        totremwh<-dfsum %>% summarise(sum(dblrenew/12))
        shortMW<-dfsum %>% summarise(max(maxShortMW))
        periodAvgDemand<-(dfsum %>% summarise(mean(demand)))/1000
        print(paste0("Avg power: ",avgpower," GW, Period Avg: ",periodAvgDemand," GW"))
        
        hrs<-8
        dfsum$diff<-roll_sum((dfsum$dblrenew-dfsum$demand)/12,n=12*hrs,align="right",fill=0)
        dfsum$sumdblrenew<-roll_sum(dfsum$dblrenew/12,n=12*hrs,align="right",fill=0)
        dfsum$sumdemand<-roll_sum(dfsum$demand/12,n=12*hrs,align="right",fill=0)
        r<-dfsum %>% select(Time,sumdblrenew,sumdemand,diff) %>% slice_min(diff)
        df<-data_frame(
          `Parameter`=c("Demand","Shortfall","Curtailment","Maximum power shortage (MW)","Battery energy supplied (MWh)",
                        "Maximum battery power (MW)",
                        "Battery capacity factor","Max 8hr shortage end time"),
          `Value`=c(paste0(comma(totdemand/1000)," GWh"),
                    paste0(comma(sh/1000)," GWh (wind+solar+batteries=",comma((totdemand-sh)/totdemand*100),"%)"),
                    paste0(comma(curt/1000)," GWh (",comma(100*curt/totremwh),"%)"),
                    paste0(comma(shortMW)," dispatchable MW"),
                    paste0(comma(bsup/1000)," GWh"),
                    paste0(comma(bmax)," MW"),
                    paste0(comma(100*bsup/((bmax/12)*nperiods)),"%"),
                    paste0(r$Time,": ",comma(-r$diff),"MWh")
                    )
        )
        df |> gt() |> tab_header(title="Output results") |> tab_options(table.width=pct(100),
                                                                           table.background.color=tbgcolor,
                                                                           table.font.color=tfgcolor,
                                                                        column_labels.hidden=T,heading.align="left")
    })
    
    output$calctable <- render_gt({
        dfsum<-gendfsum()
        maxwind<-max(dfsum$wind)
        minwind<-min(dfsum$wind)
        dfsum$windhr<-roll_sum(dfsum$wind/12,n=12,fill=0)
        dfsum$wind8hr<-roll_sum(dfsum$wind/12,n=12*8,fill=0)
        minwindhr<-min(dfsum$windhr[dfsum$windhr>0])
        maxwindhr<-max(dfsum$windhr)
        minwind8hr<-min(dfsum$wind8hr[dfsum$wind8hr>0])
        maxwind8hr<-max(dfsum$wind8hr)
        df<-data_frame(
          ` `=c("Max energy in period (MWh)","Min energy over period (MWh)","% min of max"),
          `5 Minutes`=c(comma(maxwind),comma(minwind),paste0(comma(minwind/maxwind*100),"%")),
          `1 Hour`=c(comma(maxwindhr),comma(minwindhr),paste0(comma(minwindhr/maxwindhr*100),"%")),
          `8 Hours`=c(comma(maxwind8hr),comma(minwind8hr),paste0(comma(minwind8hr/maxwind8hr*100),"%"))
        )
        df |> gt() |> tab_header(title="Wind performance") |> tab_options(table.width=pct(100),
                                                                           table.background.color=tbgcolor,
                                                                           table.font.color=tfgcolor,
                                                                          heading.align="left")
    })
    output$calcparms <- render_gt({
        dfsum<-gendfsum()
        nperiods<-length(dfsum$Time)
        periodAvgDemand<-(dfsum %>% summarise(mean(demand)))/1000
        
        df<-data_frame(
          `Parameter`=c("Input Data","Period length","Overbuild factor","Baseload size","Battery energy storage size"),
          `Value`=c(input$datasetpick,paste0(comma((nperiods/12)/24)," days"),comma(input$ofac),paste0(comma(input$blmult*input$baseloadsize)," MW"),
          paste0(comma(input$bmult*input$bsize)," MWh (",comma((input$bmult*input$bsize*1e6)/(periodAvgDemand*1e9))," hrs)")
          )
        )
        df |> gt() |> tab_header(title="Slider parameters") |> tab_options(table.width=pct(100),
                                                                           table.background.color=tbgcolor,
                                                                           table.font.color=tfgcolor,
                                                                           column_labels.hidden=T,heading.align="left")
    })
    output$onshortages <- renderPlot({
      dfsum<-gendfsum()
      
      hrs<-8
      dfsum$diff<-roll_sum((dfsum$dblrenew-dfsum$demand)/12,n=12*hrs,align="right",fill=0)
      dfsum$sumdblrenew<-roll_sum(dfsum$dblrenew/12,n=12*hrs,align="right",fill=0)
      dfsum$sumdemand<-roll_sum(dfsum$demand/12,n=12*hrs,align="right",fill=0)
      rnights<-dfsum %>% select(Time,sumdblrenew,sumdemand,diff) %>% filter(hour(Time)*60+minute(Time)==9*60) 
      write_csv(rnights,"tmp-rnightsplot.csv")
      dfn<-tibble()
      for(i in 1:nrow(rnights)) {
          d<-rnights$diff[i]
          t<-day(rnights$Time[i])
          dfn<-bind_rows(dfn,tibble('Day'=date(rnights$Time[i]),'Shortage'=-d))
#          if (d<0) {
#            onshortages<-paste0(onshortages," ",t,":",comma(-d)," MWh ")
#            onbattmult<-paste0(onbattmult," ",t,":",comma(-d/(input$bmult*input$bsize)),"x ")
#          }
      }
      write_csv(dfn,"tmp-dfn.csv")
      str(dfn)
      p<-dfn |> ggplot() + geom_col(aes(x=ymd(Day),y=Shortage/1000),fill="grey")+
        geom_text(aes(x=ymd(Day),y=ifelse(Shortage/1000>0,Shortage/1000,0),label=comma(Shortage/1000),vjust=-0.1))+
        labs(x="",y="GWh",title="Overnight (9pm-9am) shortage\nDifference between demand and supply\n(Excluding any storage)\nNegative values are when supply exceeds demand")
      p +theme_bw()
    })
    output$shortfall <- renderPlot({
      dfsum<-gendfsum()
      #write_csv(dfsum,"xxx1.csv")
      dfcumshort<-dfsum %>% select(Time,batteryStatus,supply,wind,demand,cumShortMWh,maxShortMW,renew,dblrenew,cumThrowOutMWh)
      maxsupply=max(dfsum$dblrenew)
      bl<-ifelse((input$blmult*input$baseloadsize)>0,paste0("BL",input$blmult*input$baseloadsize,"MW"),"nobl")
      bsz<-ifelse((input$bmult*input$bsize)>0,paste0("BATT",comma(input$bsize*input$bmult),"MW"),"nobatt")
      ovfac<-ifelse(input$ofac>0,paste0("FAC",comma(input$ofac),""),"nooverbuild")
      ff<-gsub(" ","",input$datasetpick)
      fname=paste0("dfsum-",bl,"-",bsz,"-",ovfac,"-",ff,".csv")
      write_csv(dfsum,fname)
      maxshort<-max(dfcumshort$cumShortMWh)
      maxcurt<-max(dfcumshort$cumThrowOutMWh)
      
      thecols=colsshort
      thelabs=labsshort
      dfcs<-dfcumshort %>% pivot_longer(cols=c("demand","renew","dblrenew"),names_to="Level",values_to="MW") 
      thetitle=dataSetTitles[input$datasetpick]
      if (input$showWindDemand) {
        dfcs<-dfcumshort %>% pivot_longer(cols=c("demand","wind"),names_to="Level",values_to="MW") 
        x<-str_split_1(thetitle,"\n")
        print(x)
        thetitle<-paste0("Electricity demand vs wind\n",x[2])
        thecols=colswind
        thelabs=labswind
      }
      nperiods<-length(dfsum$Time)
      lab<-c()
      val<-c()
      if (input$showShort) {
        lab=c("Shortfall (cumulative)")
        val=c("dashed")
      }
      if (input$showCurtailed) {
        lab=c(lab,"Curtailment")
        val=c(val,"dotted")
      }
      if (input$baseloadsize>0) {
        lab=c(lab,"Baseload (MW)")
        val=c(val,"longdash")
      }
      ll<-c()
      vv<-c()
      if (input$gaspeak>0) {
        ll=c(ll,"gas peaker")
        vv=c(vv,"blue")
      }
      if (input$showShort) {
        ll=c(ll,"shortfall")
        vv=c(vv,"orange")
      }
      
      nightbands<-findNightTimeBands(dfsum)
#      for(i in 1:nrow(nightbands)) {
#        cat(paste0())
#      }
      #---------------------------------------------
      # coef is used to scale the RHS y-axis and depends on the size of the 
      # amount of shortfall or curtailment
      # bfac scales the battery status chart 
      #---------------------------------------------
      bfac=input$bsize*input$bmult
      if (bfac==0) bfac=1000
      bfac=maxsupply
      mm=max(maxshort,maxcurt,999)
      if (maxshort==999) {
        coef<-500*(3/28)
      }
      else {
        coef=maxsupply/mm*1000
      }
      print(paste0("MaxShortFall: ",maxshort," MaxSupply: ",maxsupply," Coef: ",coef,"\n"))
      
      
      print(ll)
      p<-dfcs %>% ggplot() + 
        geom_line(aes(x=Time,y=MW,color=Level),linewidth=0.5) +  
        ptheme +
        {if (input$showShort)
          geom_line(aes(x=Time,y=cumShortMWh*coef/1000,linetype="dashed"),data=dfsum)
        }+
        {if (input$showCurtailed)
          geom_line(aes(x=Time,y=cumThrowOutMWh*coef/1000,linetype="dotted"),data=dfsum)
        }+
        {if (input$showBatteryStatus)  
          geom_line(aes(x=Time,y=(batteryStatus/(input$bsize*input$bmult))*bfac),color="red",data=dfsum)
        }+
        {if (input$baseloadsize)  
          geom_hline(aes(yintercept=input$blmult*input$baseloadsize,linetype="longdash"),color="purple",data=dfsum)
        }+
        {if (input$showBatteryStatus)  
          annotate('text',x=dfcs$Time[nperiods/2],y=bfac,label="Battery 100% full",color="red",vjust=-0.2,hjust=0)
        }+
        {if (input$gaspeak>0 & input$showShort)  
          geom_col(aes(x=Time,y=gasMWout,fill="blue"),alpha=0.2,data=dfsum)
        }+
        {if (input$showShort)  
          geom_col(aes(x=Time,y=shortFall*12,fill="orange"),alpha=0.5,data=dfsum)
        }+
        {if (input$showShort)  
          scale_fill_manual(name="Shortfall",labels=ll,values=vv)
        }+
        geom_rect(aes(xmin=t1,xmax=t2,ymin=0,ymax=Inf),data=nightbands,alpha=0.2)+
        labs(color="Supply/Demand (MW)",title=thetitle)+
        scale_color_manual(breaks=colsbreaks,labels=colslabels,values=colslevels)+
        scale_linetype_manual(name="Other-measures",labels=lab,values=val)+
        scale_y_continuous(
          name="Megawatts (supply/demand/overbuild)",
          sec.axis = sec_axis(~./coef, name="Cumulative shortfall/curtailment in GWh")
        )+theme(legend.direction="vertical",legend.box="vertical")
        p+theme_bw()+theme(legend.position="bottom",legend.box="vertical")
    })
}

# Run the application 
enableBookmarking("url")
shinyApp(ui = ui, server = server)
