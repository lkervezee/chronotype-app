####    This code underlies a shiny app that calculates chronotype and social jetlag, to be used for public outreach activities, e.g. see:
####    You are free to modify this code under a GNU General Public License (see below), e.g. translate it or improve its design or functionality
####    For more info and existing translations, see: https://laurakervezee.net/resources/pop-up-science-early-bird-or-night-owl/
####    The activity is intended for purely educational purposes, not for research purposes. The app does not story any participant data.
####    
####    Copyright (C) 2022 Laura Kervezee (l.kervezee@lumc.nl)  
####
####    This program is free software: you can redistribute it and/or modify
####    it under the terms of the GNU General Public License as published by
####    the Free Software Foundation, either version 3 of the License, or
####    any later version.

####    This program is distributed in the hope that it will be useful,
####    but WITHOUT ANY WARRANTY; without even the implied warranty of
####    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
####    GNU General Public License for more details.

####    You should have received a copy of the GNU General Public License
####    along with this program.  If not, see <https://www.gnu.org/licenses/>.

####    REFERENCES: 
####            Chronotype calculated using microMCTQ, see https://pubmed.ncbi.nlm.nih.gov/31791166/ 
####            Social jetlag calculated as difference between mid‐sleep on workdays (MSW) and mid‐sleep on free days (MSF)(see Roenneberg et al 2012 Social jetlag and obesity. Current Biology https://www.ncbi.nlm.nih.gov/pubmed/22578422 )
####            Chronotype classifications based on https://www.mdpi.com/2079-7737/8/3/54 


library(shiny)

# Define UI for application 
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #f0fcff ;
         }

         #main {
         background-color: #ffffff  ;
         }
                  
        body {
        background-color: #ffffff;
        }

         label, input, button, select { 
          font-size: 15pt;
         }')
  )),
  
  tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 30px; background: white; border-top: 1px solid black; border-bottom: 1px solid black;}
             .irs-bar-edge {background: white; border-top: 1px solid black; height: 30px; border-radius: 0px; width: 20px;}
             .irs-line {border: 1px solid black; color: white; height: 30px; border-radius: 0px;}
             .irs-grid-text {font-family: 'arial'; color: #666666; top: 30px; z-index: 1; font-size: 15pt}
             .irs-grid-pol {display: none;}
             .irs-max {font-family: 'arial'; color: black; font-size: 0pt}
             .irs-min {font-family: 'arial'; color: black; font-size: 0pt}
             .irs-single {color:white; background:#002060 ; font-size: 15pt; line-height:15pt}
             .irs-slider {width: 30px; height: 30px; top: 25px}
             "),
   # Application title
   titlePanel(p("Are you an early bird or night owl?", align="center", style="font-weight: bold; font-size=16pt"), windowTitle = "Are you an early bird or night owl?"),
   
   sidebarLayout(
      sidebarPanel(width=12, id="sidebar",
        
        radioButtons("n_work", 
                    "Normally, I go to work or school", 
                    choices = list("0 days/week" = 0, 
                                   "1 days/week" = 1,
                                   "2 days/week" = 2, 
                                   "3 days/week" = 3,
                                   "4 days/week" = 4,
                                   "5 days/week" = 5,
                                   "6 days/week" = 6,
                                   "7 days/week" = 7),
                    selected =5, inline=T),
        br(),br(),
        sliderInput("bedtime_work",
                    "On school or work days, I normally fall asleep at:",
                    min = as.POSIXct("2019-10-04 19:00:00", tz="UTC"),
                    max = as.POSIXct("2019-10-05 14:00:00", tz="UTC"),
                    timezone="UTC",
                    step=600,
                    value = as.POSIXct("2019-10-05 00:00:00", tz="UTC"),
                    timeFormat = "%H:%M"),
        br(),br(),
        sliderInput("waketime_work",
                    "On school or work days, I normally wake up at:",
                    min = as.POSIXct("2019-10-04 19:00:00", tz="UTC"),
                    max = as.POSIXct("2019-10-05 14:00:00", tz="UTC"),
                    timezone="UTC",
                    step=600,
                    value = as.POSIXct("2019-10-05 08:00:00", tz="UTC"),
                    timeFormat = "%H:%M"),
        br(),br(),    
         
         sliderInput("bedtime_free",
                     "On free days, I normally fall asleep at:",
                     min = as.POSIXct("2019-10-04 19:00:00", tz="UTC"),
                     max = as.POSIXct("2019-10-05 14:00:00", tz="UTC"),
                     timezone="UTC",
                     step=600,
                     value = as.POSIXct("2019-10-05 00:00:00", tz="UTC"),
                     timeFormat = "%H:%M"),
         br(),br(),
         sliderInput("waketime_free",
                     "On free days, when I don't use an alarm clock, I normally wake up at:",
                     min = as.POSIXct("2019-10-04 19:00:00", tz="UTC"),
                     max = as.POSIXct("2019-10-05 14:00:00", tz="UTC"),
                     timezone="UTC",
                     step=600,
                     value = as.POSIXct("2019-10-05 08:00:00", tz="UTC"),
                     timeFormat = "%H:%M")),
    

      mainPanel(width=12, id="main",
        h4(textOutput("chronotype"), align="center", style="font-weight: bold")
      )
   )
)

# Define server logic 
server <- function(input, output) {
   
   output$chronotype <- renderText({
     
      start_free <- input$bedtime_free
     end_free <- input$waketime_free
     
     start_work <- input$bedtime_work
     end_work <- input$waketime_work
     
     days_work <- as.numeric(input$n_work)
     
     #start_free <- as.POSIXct("2019-10-04 23:10:00", tz="UTC")
     #end_free <- as.POSIXct("2019-10-05 07:50:00", tz="UTC")
     #
     #start_work <- as.POSIXct("2019-10-05 00:00:00", tz="UTC")
     #end_work <- as.POSIXct("2019-10-05 08:00:00", tz="UTC")
     #
     #days_work <- 5
     
     sleepduration_work <- difftime(end_work, start_work, units="hours")
     sleepduration_free <- difftime(end_free, start_free, units="hours")
     
     sleepduration_av <- (as.numeric(sleepduration_free)*(7-days_work) + as.numeric(sleepduration_work)*days_work)/7
     
     
     midsleep_free <- start_free + sleepduration_free/2
     midsleep_work <- start_work + sleepduration_work/2
     
     if(sleepduration_free > sleepduration_work){ 
       chronotype <-  midsleep_free - (sleepduration_free - sleepduration_av)/2
     } else { 
       chronotype <- midsleep_free
     } 
     
     socialjetlag <- as.numeric(difftime(midsleep_free, midsleep_work, units="hours"))
     
     socialjetlag_text <- paste(strsplit(as.character(socialjetlag), "[.]")[[1]][1], "hours and", round((socialjetlag-floor(socialjetlag))*60), "minutes.")     # 
     chronotype_num <- sapply(strsplit(format(chronotype, "%H:%M"), ":"), FUN=function(x) as.numeric(x[1])+as.numeric(x[2])/60)
     cat <- ifelse(chronotype_num <= 1.25, "an extremely early type",
                   ifelse(chronotype_num > 1.25 & chronotype_num <= 2.25, "a moderately early type",
                          ifelse(chronotype_num > 2.25 & chronotype_num <= 3.25, "a slightly early type",
                                 ifelse(chronotype_num > 3.25 & chronotype_num <= 4.25, "an intermediate type",
                                        ifelse(chronotype_num > 4.25 & chronotype_num <= 5.25, "a slightly late type",
                                        ifelse(chronotype_num > 5.25 & chronotype_num <= 6.25, "a moderately late type",
                                                      ifelse(chronotype_num > 6.25 & chronotype_num <= 12, "an extremely late type",
                                                             ifelse(chronotype_num > 21 & chronotype_num <= 24, "an extremely early type", "***"))))))))
     
    
     paste0("Your chronotype is ", format(chronotype, "%H:%M"), ". This means you are ", cat, ". Each week you have a social jetlag of ", socialjetlag_text)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

