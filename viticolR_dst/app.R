# This is the non-live version, delete when copying
 # cp -TR "/homevol/pmelloy/R/downy_dst/viticolR_dst/" "/homevol/pmelloy/shiny-server/viticolR_dst/"

if("/usr/lib/R/site-library" %in% .libPaths()){
   .libPaths("/homevol/pmelloy/R/x86_64-pc-linux-gnu-library/4.3")}
library(shiny)
library(data.table)
library(viticolaR)
library(shinythemes)
#library(DT)
library(ggplot2)
source("R/ccs_styles.R")

load("/homevol/pmelloy/Weather observations/DM_dst_data.rda")
DMod <- DMod_NT


# Define UI for application that draws a histogram
ui <- fluidPage(
   # # Styling
   theme = shinytheme("superhero"),
   # tags$head(tags$style("{color: white;
   #                               font-size: 20px;
   #                               font-style: italic;
   #                               }")
   #           ),
   tags$style('.container-fluid {
              background-color: #49125c;
              }'),


   titlePanel(div(HTML("Downy Mildew (<i>Plasmodium viticola</i>) decision support tool"))),

   tabsetPanel(
      tabPanel("viticolR",
               h2("viticolR"),
               p(HTML("A mechanistic compartment model to assist in fungicide decision
                 support for downy mildew (<i>Plasmodia viticola</i>) infections in
                 grapevines.")),
               p(""),
               p(""),
               h3("Adapted from scientific literature by Dr Paul Melloy, The University of Queensland"),
               p("Funding for this project was provided through the",
                 a("Agrifood Kickstarter grant",
                   href = "https://agriculture-food-sustainability.uq.edu.au/article/2022/03/uq-launches-kickstarter-grants-facilitate-collaboration-industry"),
                 "funding in partnership with", a(href = "https://www.cauldrondistillery.com.au/", "Cauldron distillery")),
               p(),
               hr(),
               h3("Input vineyard details"),
               selectInput("station", "Station location",
                           choices = c("North Tamborine",
                                       "Applethorpe")),
               dateInput("BudBurst", "Date of Bud burst",value = paste0(year(Sys.Date()),"-08-25")),


               img(src = "InspectingLeaves.jpg",
                   align = "center",
                   height = "50%",
                   width = "50%"),
               p(),
               p("This model is translated and adapted from the published paper 'A
                 mechanistic model simulating primary infections of downy mildew
                 in grapevine' authored by Vittorio Rossi, Tito Caffi,
                 Simona Giosue and Riccardo Bugiani. This paper was first published
                 in Ecological Modelling in **2008**.", a("10.1016/j.ecolmodel.2007.10.046", href = "https://www.sciencedirect.com/science/article/pii/S0304380007005881"))
      ),
      tabPanel("Seasonal progress",
               h2("Seasonal progress of residual oospores germinating"),
               h3(textOutput(outputId = "last_mod_time")),
               plotOutput("HT_Plot")),
      tabPanel("Primary dispersals",
               h2("Primary zoospore dispersals from oospore sporangia"),
               h3(textOutput(outputId = "last_mod_time2")),
               verticalLayout(
                  ccs_style(1),
                  code(textOutput(outputId ="txtOosporeGerm" )),
                  p("The number of germinated oospore cohorts over the whole season.",
                    "Sporangia germinate from oospores following rain and survive for some time."),
                  ccs_style(2),
                  code(textOutput(outputId ="txtDeadSpor")),
                  p("Number or cohorts which have germinated and not survived until
                    conditions were suitable for a dispersal event."),
                  code(textOutput(outputId ="txtCohortSurvival")),
                  p("Sporangia survival time range in hours for this site."),
                  ccs_style(3),
                  code(textOutput(outputId ="txtZooInf")),
                  p("Number of times primary infection events in this season.",
                    "This is when zoospores have successfully been dispersed by
                    rain splash from sporangia onto leaves and conditions have
                    been suitable enough for zoospores to survive and infect."),
                  ccs_style(3),
                  code(textOutput(outputId ="txtSporangReady")),
                  p("Number of germinated sporangia cohorts which could lead to
                    a zoospore spread event if it rains"),
                  code(textOutput(outputId ="mature_zoo_time")),
                  p("Most recent time when surviving sporangia contained mature
                    zoospores ready for dispersal. It is likely that any rain within
                    1 - 48 hours of this time will lead to a dispersal event"),
                  code(textOutput(outputId ="txtlatentPs")),
                  p("When zoospores have been spread and caused infection, however
                    have yet to produce symptoms.",
                    "During this period there is an oppotunitiy to apply fungicide
                    to treat the infection"),
                  plotOutput("PI_plot"),
                  p("This plot shows how many 'cohorts' or events where oospores
                 have germinated sporangia (spo_germination_hour), which have
                 then released zoospores (zoo_release_ind).
                 If a rain event occurs when the zoospores have been released
                 before the zoospores dry out and die then the zoospores have a
                 chance to disperse to grapevine leaves (zoo_dispersal_ind).
                 Zoospores can then infect the leaves if conditions remain suitable
                 for infection (zoo_infection_ind).
                 Then the infections are latent for a period before infections
                 present as oilspots between INC_h_lower and INC_h_upper."),
                 p(""),
                 tableOutput("PI_table")) # end of verticalLayout
      ), # end of tabpanel
      tabPanel("Forecast risk",
               selectInput("Days2forecast_rain",
                           "How many days to the next rain event",
                           choices = c(0:6,"> 7")),
               numericInput("forecast_rain","Forecast rain days in the next week",
                            min = 0, max = 7,value = 1),
               code(textOutput(outputId ="txtRisk")),

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   # Model last modified time (lmt)
   # use a reactive expression to render the standardised text
   lmt <- reactive({
      paste("Model last updated:",
            as.POSIXct(data.table::last(DMod$time_hours),
                       tz = "Australia/Brisbane"))
   })
   # it is output twice, html only allows one output per id so it needs duplication
   output$last_mod_time <- renderText(lmt())
   output$last_mod_time2 <- renderText(lmt())
   last_mod_time <- reactive(max(DMod$w$times, na.rm = TRUE))

   # Format model output into a data.table of dates for each stage
   Ddates <- reactive({
      downy_dates <- viticolaR::get_PI_dates(DMod)

      # arrange categories for plot
      downy_dates[, primary_infection_stage := factor(primary_infection_stage,
                                                 levels = c("spo_germination_hour", "spo_death_hour",
                                                            "zoo_release_ind","zoo_dispersal_ind",
                                                            "zoo_infection_ind","mature_zoopores",
                                                            "INC_h_lower", "INC_h_upper"))]
      downy_dates
      })


   # create a vector of sporangia survival lengths to determine likelihood
   # this could be made faster by casting wide and then calculating
   spo_survival <- reactive({
      for (i in unique(Ddates()$cohort)) {
         if (i == 1)
            spo_sur <- vector(mode = "numeric")
         spo_sur <- c(spo_sur,
                      difftime(Ddates()[cohort == i &
                                         primary_infection_stage == "spo_death_hour", hour],
                               Ddates()[cohort == i &
                                         primary_infection_stage == "spo_germination_hour", hour],
                               units = "hours"))
      }
      spo_sur
   })

   # What are the surviving cohort numbers
   surviving_zoospore <- reactive({
      Ddates()[primary_infection_stage == "mature_zoopores" &
                hour >= (last_mod_time() - 3600),.N]})

   # Get the germination times of the cohorts
   output$mature_zoo_time <- renderText({
      if(surviving_zoospore() > 0){
         surviving_zoo_time <-
            Ddates()[primary_infection_stage == "mature_zoopores" &
                      hour >= (last_mod_time() - 3600),
                   hour]}else{
                      surviving_zoo_time <- NULL
                   }

      if(is.null(surviving_zoo_time)){
         "No recently matured zoospores"
      }else{
         paste0("Zoospores recently matured at: ",
                surviving_zoo_time)
      }
   })


   output$img_leaf <- renderImage({
      list(src = "www/grapeleaf_DM_infected.jpg",
           alt = "Grapevine leaf infected with downy mildew")
   }, deleteFile = FALSE)

   # output$w_table <- renderDataTable(NT_weather)
   output$PI_table <-
      renderTable(Ddates()[(
         primary_infection_stage == "zoo_dispersal_ind" |
            primary_infection_stage == "zoo_infection_ind"
      ) &
         is.na(hour) == FALSE, list(cohort,
                                    primary_infection_stage,
                                    Time = as.character(hour))])

   # render plot of hydrothermal time
   output$HT_Plot <- renderPlot({
      plot(
         x = DMod$time_hours,
         y = DMod$Hyd_t,
         ylim = c(0, 13),
         ylab = "Hydrothemal time",
         xlab = "date",
         type = "l",
         col = "red",
         lwd = 5
      )
      rect(xleft = DMod$time_hours[1]-30,
           xright = last(DMod$time_hours)+30,
           ybottom = 1.3,
           ytop = 8.6,
           col = rgb(0,0,1,alpha = 0.2),
           border = NA)
      text(x = DMod$time_hours[300],
           y = 5,labels = "Downy Mildew season",
           col = "blue",
           srt = 270)
      abline(v = as.POSIXct(input$BudBurst),
             col = "forestgreen")
      text(x = as.POSIXct(input$BudBurst)+100000,
           y = 11, labels = "Bud Burst Date",
           col = "forestgreen",
           srt = 270)

   })

   output$PI_plot <- renderPlot({
      Ddates()[primary_infection_stage != "spo_death_hour" &
                primary_infection_stage != "INC_h_lower" &
                primary_infection_stage != "INC_h_upper" &
                is.na(hour) == FALSE &
                hour >= as.POSIXct(input$BudBurst)
             ,] |>
         ggplot(aes(x = hour,
                    y = primary_infection_stage,
                    group = factor(cohort)))+
         geom_line()+
         theme_minimal()
   })

   # text descriptions of model output
   output$txtOosporeGerm <- renderText({
      paste("Total sporangia germinations this season:",
            Ddates()[hour >= as.POSIXct(input$BudBurst), max(cohort)])
   })

   zoo_dispersals <- reactive({
      Ddates()[primary_infection_stage == "zoo_dispersal_ind" &
                hour >= as.POSIXct(input$BudBurst) &
                is.na(hour)==FALSE,hour]
   })


   output$txtDeadSpor <- renderText({
      paste("Sporagia deaths before dispersal:",
            max(Ddates()$cohort) - length(zoo_dispersals()))
   })
   output$txtZooDisp <- renderText({
      paste("Zoospore dispersals to vine leaves:",
            length(zoo_dispersals()))
   })
   output$txtZooInf <- renderText({
      paste("Sucessful zoospore infections:",
            length(Ddates()[primary_infection_stage == "zoo_dispersal_ind" &
                             is.na(hour)==FALSE &
                             hour >= as.POSIXct(input$BudBurst),hour]))
   })
   output$txtSporangReady <- renderText({
      paste("Current surviving Sporangia cohorts:",
            surviving_zoospore())
   })
   output$txtlatentPs <- renderText({
      #get number of zoospore infections with no infection symptom estimation
      infected_cohorts <- Ddates()[primary_infection_stage == "zoo_infection_ind" &
                                    is.na(hour)==FALSE,cohort]
      cohorts_in_latent <-
         Ddates()[infected_cohorts %in% cohort &
                   (primary_infection_stage == "INC_h_upper" |
                       primary_infection_stage == "INC_h_lower")&
                   hour > last(DMod$time_hours),unique(cohort)]

      paste("Infections in latent period:",length(cohorts_in_latent))
   })

   # get time cohorts have been surviving for
   surviving_cz_time <-
      reactive({
         abs(difftime(max(Ddates()[primary_infection_stage == "mature_zoopores",
                                 max(hour, na.rm = TRUE)]),
                      data.table::last(DMod$time_hours),
                      units = "hours"))
      })

   output$txtCohortSurvival <- renderText({
      paste("Sporangia survial range in hours: ",
         round(quantile(spo_survival(),0.025, na.rm = TRUE)),
            " - ",
            round(quantile(spo_survival(),0.975, na.rm = TRUE)))
   })

   # Calculate the risk of primary infection
   output$txtRisk <- renderText({
      days2rain <- ifelse(input$Days2forecast_rain== "> 7",7,
                          as.numeric(input$Days2forecast_rain))

      dry_out_factor <-
         1 - ecdf(spo_survival())(surviving_cz_time() + (days2rain * 24))

      risk_val <- surviving_zoospore() * input$forecast_rain * dry_out_factor
      risk_txt <- data.table::fcase(risk_val < 1, "Low",
                                    risk_val >=1 &
                                       risk_val <4 , "Medium",
                                    risk_val >=4 &
                                       risk_val <6, "High",
                                    risk_val >=6, "Very high",
                                    default = "Error"
      )
      paste("Risk of new primary infections:", risk_txt)
   })



}

# Run the application
shinyApp(ui = ui, server = server)
