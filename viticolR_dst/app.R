# This is the non-live version, delete when copying
 # cp -r "/homevol/pmelloy/R/downy_dst/viticolR_dst/"
 #       "/homevol/pmelloy/shiny-server/viticolR_dst/"

if("/usr/lib/R/site-library" %in% .libPaths()){
   .libPaths("/homevol/pmelloy/R/x86_64-pc-linux-gnu-library/4.3")}
library(shiny)
library(data.table)
library(viticolaR)
#library(DT)
library(ggplot2)
source("R/ccs_styles.R")

load("/homevol/pmelloy/Weather observations/DM_dst_data.rda")
Ddates <- viticolaR::get_PI_dates(DMod)



# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel(div(HTML("Downy Mildew (<i>Plasmodium viticola</i>) decision support tool"))),

   tabsetPanel(
      tabPanel("viticolR",
               h2("Splash page"),
               p("A mechanistic compartment model to assist in fungicide decision
                 support for downy mildew (_Plasmodia viticola_) infections in
                 grapevines."),
               h4("Developed from the literature by Dr Paul Melloy, The University of Queensland"),
               p("Funding for this project was provided through the"),
               a(href = "https://agriculture-food-sustainability.uq.edu.au/article/2022/03/uq-launches-kickstarter-grants-facilitate-collaboration-industry",
                 "Agrifood Kickstarter grant"),
               p("funding in partnership with"),
               a(href = "https://www.cauldrondistillery.com.au/", "Cauldron distillery"),
               p(),
               img(src = "grapeleaf_DM_infected.jpg",
                   align = "center",
                   height = "70%",
                   width = "70%"),
               p("This model is translated and adapted from the published paper 'A
                 mechanistic model simulating primary infections of downy mildew
                 in grapevine' authored by Vittorio Rossi, Tito Caffi,
                 Simona Giosue and Riccardo Bugiani. This paper was first published
                 in Ecological Modelling in **2008**.")
      ),
      tabPanel("Seasonal progress",
               h2("Seasonal progress of residual oospores germinating"),
               h3(paste("North Tamborine mountain, last updated:",
                        as.POSIXct(data.table::last(DMod$time_hours),tz = "Australia/Brisbane"))),
               plotOutput("HT_Plot")),
      tabPanel("Primary disepersals",
               h2("Primary zoospore dispersals from oospore sporangia"),
               h3(paste("North Tamborine mountain, last updated:",
                        as.POSIXct(data.table::last(DMod$time_hours),tz = "Australia/Brisbane"))),
               verticalLayout(
                  ccs_style(1),
                  code(textOutput(outputId ="txtOosporeGerm" )),
                  ccs_style(2),
                  code(textOutput(outputId ="txtDeadSpor")),
                  ccs_style(3),
                  code(textOutput(outputId ="txtZooInf")),
                  ccs_style(3),
                  code(textOutput(outputId ="txtSporangReady")),
                  code(textOutput(outputId ="txtlatentPs")),
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
               numericInput("forecast_rain","Forecast rain days in the next week",
                            min = 0, max = 7,value = 1),
               code(textOutput(outputId ="txtRisk")),

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   # create a vector of sporangia survival lengths to determine likelihood
   # this could be made faster by casting wide and then calculating
   for (i in unique(Ddates$cohort)) {
      if (i == 1)
         spo_survival <- vector(mode = "numeric")
      spo_survival <- c(spo_survival,
                        difftime(Ddates[cohort == i &
                                           primary_infection_stage == "spo_death_hour", hour],
                                 Ddates[cohort == i &
                                           primary_infection_stage == "spo_germination_hour", hour],
                                 units = "hours"))
   }
   # What are the surviving cohort numbers
   surviving_cohorts <-
      Ddates[primary_infection_stage == "spo_death_hour" &
                is.na(hour),cohort]
   # Get the germination times of the cohorts
   surviving_co_time <-
      Ddates[primary_infection_stage == "spo_germination_hour " &
                surviving_cohorts %in% cohort,hour]


   output$img_leaf <- renderImage({
      list(src = "www/grapeleaf_DM_infected.jpg",
           alt = "Grapevine leaf infected with downy mildew")
   },deleteFile = FALSE)

   # output$w_table <- renderDataTable(NT_weather)
   output$PI_table <-
      renderTable(Ddates[(primary_infection_stage == "zoo_dispersal_ind" |
                             primary_infection_stage == "zoo_infection_ind") &
                            is.na(hour) == FALSE,list(cohort,
                                                      primary_infection_stage,
                                                      Time = as.character(hour))])

   # render plot of hydrothermal time
   output$HT_Plot <- renderPlot({
      plot(
         x = DMod$time_hours,
         y = DMod$Hyd_t,
         ylim = c(0, 9),
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

   })

   output$PI_plot <- renderPlot({
      Ddates[primary_infection_stage != "spo_death_hour" &
                is.na(hour) == FALSE] |>
         ggplot(aes(x = hour,
                    y = primary_infection_stage,
                    group = factor(cohort)))+
         geom_line()+
         theme_minimal()
   })

   # text descriptions of model output
   output$txtOosporeGerm <- renderText({
      paste("Total sporangia germinations this season:",
            max(Ddates$cohort))
   })

   zoo_dispersals <- Ddates[primary_infection_stage == "zoo_dispersal_ind" &
                               is.na(hour)==FALSE,hour]
   output$txtDeadSpor <- renderText({
      paste("Sporagia deaths before dispersal:",
            max(Ddates$cohort) - length(zoo_dispersals))
   })
   output$txtZooDisp <- renderText({
      paste("Zoospore dispersals to vine leaves:",
            length(zoo_dispersals))
   })
   output$txtZooInf <- renderText({
      paste("Sucessful zoospore infections:",
            length(Ddates[primary_infection_stage == "zoo_dispersal_ind" &
                             is.na(hour)==FALSE,hour]))
   })
   output$txtSporangReady <- renderText({
      paste("Current surviving Sporangia cohorts:",
            length(surviving_cohorts))
   })
   output$txtlatentPs <- renderText({
      #get number of zoospore infections with no infection symptom estimation
      infected_cohorts <- Ddates[primary_infection_stage == "zoo_infection_ind" &
                                    is.na(hour)==FALSE,cohort]
      cohorts_in_latent <-
         Ddates[infected_cohorts %in% cohort &
                   (primary_infection_stage == "INC_h_upper" |
                       primary_infection_stage == "INC_h_lower")&
                   hour > last(DMod$time_hours),unique(cohort)]

      paste("Infections in latent period:",length(cohorts_in_latent))
   })
   output$txtRisk <- renderText({
      Sprgia <- length(Ddates[primary_infection_stage == "spo_death_hour" &
                                 is.na(hour),hour])
      risk_val <- Sprgia * input$forecast_rain
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
