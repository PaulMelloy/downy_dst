# This is the non-live version, delete when copying
 # cp -TR "/homevol/pmelloy/R/downy_dst/viticolR_dst/" "/homevol/pmelloy/shiny-server/viticolR_dst/"
message(.libPaths())
if("/usr/lib/R/site-library" %in% .libPaths()){
   .libPaths("/homevol/pmelloy/R/x86_64-pc-linux-gnu-library/4.4")}
local_library <- switch(Sys.info()["nodename"],
                        "PURPLE-DP" = "C:/Users/mel096/AppData/Local/R/win-library/4.4",
                        "viticola" = "/homevol/pmelloy/R/x86_64-pc-linux-gnu-library/4.4")

switch(Sys.info()["nodename"],
       "PURPLE-DP" = load("C:/Users/mel096/OneDrive - CSIRO/Data/DM_dst_data.rda"),
       "viticola" = load("/homevol/pmelloy/Weather observations/DM_dst_data.rda"))

#library(fastmap,lib.loc = "/homevol/pmelloy/R/x86_64-pc-linux-gnu-library/4.4")
library(shiny,lib.loc = local_library)
library(data.table,lib.loc = local_library)
library(viticolaR,lib.loc = local_library)
library(shinythemes,lib.loc = local_library)
#library(DT)
library(ggplot2,lib.loc = local_library)
source("R/ccs_styles.R")

# if(Sys.info()["nodename"] == "viticola"){
#    load("/homevol/pmelloy/Weather observations/DM_dst_data.rda")
# }else{
#    load("C:/R/downy_dst/data/DM_dst_data.rda")
# }
# Load the last model run

# assign default model as North Tamborine
DMod <- DMod_NT

plot_width <- ifelse(length(DMod$time_hours) < 1000,
                     "auto",
                     paste0(length(DMod$time_hours),"px"))

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
              background-color: #25052e;}'),
   tags$style('.navbar-default {
              background-color:#1e043b}
              '),

   titlePanel(div(HTML("Downy Mildew (<i>Plasmodium viticola</i>) decision support tool"))),

   tabsetPanel(
      tabPanel("viticolR",
               h2("viticolR"),
               p(HTML("A mechanistic compartment model to assist in fungicide decision
                 support for downy mildew (<i>Plasmodia viticola</i>) primary
                 infections in grapevines.")),
               p(""),
               p(""),
               h4("Adapted from scientific literature by",
                  a("Dr Paul Melloy, while working at The University of Queensland",
                    href = "https://paul.melloy.com.au/")),
               p("Funding for this project was provided through the",
                 a("Agrifood Kickstarter grant",
                   href = "https://agriculture-food-sustainability.uq.edu.au/article/2022/03/uq-launches-kickstarter-grants-facilitate-collaboration-industry"),
                 "funding in partnership with", a(href = "https://www.cauldrondistillery.com.au/", "Cauldron distillery")),
               p(),
               img(src = "InspectingLeaves.jpg",
                   align = "center",
                   height = "50%",
                   width = "50%"),
               h3("Disclaimer:"),
               p("This app is to be used as a guide in making more informed fungicide
                 management decisions.",
                 "Individual circumstances and conditions may vary, and local conditions
                 should be also concidered when deciding on the appropriate fungicide
                 application"),
               p("Always apply fungicide according to the label recommendataions"),
               h4("Model estimates primary innoculum dispersal only:"),
               p("    This model uses weather inputs to estimate the steps in primary
                 inoculum dispersal and therefore is influenced by the AWS accuracy.",
                 "The closer the weather station to the vineyard, the more accurate
                 the estimations.",
                 "By default missing rainfall observations are filled with 0"),
               p("This model does not account for secondary infections.",
                 HTML("<b>If downy mildew is already present on the leaves, an alternate
                 fungicide approach should be considered as this decision support
                 tool would be misleading</b>"),
                 "Once downy mildew is established in the vineyard disease can develop
                 rapidly.",
                 "Use of this model should be used to prevent primary leaf infections"),
               hr(),
               p(),
               p(HTML("This model is translated and adapted from the published paper 'A
                 mechanistic model simulating primary infections of downy mildew
                 in grapevine' authored by Vittorio Rossi, Tito Caffi,
                 Simona Giosue and Riccardo Bugiani. This paper was first published
                 in Ecological Modelling in <b>2008</b>."),
                 a("10.1016/j.ecolmodel.2007.10.046",
                   href = "https://www.sciencedirect.com/science/article/pii/S0304380007005881")),
               hr()
      ),

      #------------------------------------------------------------------------
      tabPanel("Getting started",

               h3("Input vineyard details"),
               selectInput("station", "Weather Station location",
                           choices = c("North Tamborine (QLD)",
                                       "Applethorpe (QLD)",
                                       "Mildura Airport (VIC)",
                                       "Loxton Research station (SA)",
                                       "Walpuep Research station (VIC)")),
               dateInput("BudBurst", "Date of Bud burst",value = paste0(year(Sys.Date()),"-08-25")),
               hr(),
               h4("Downy mildew infection process:"),
               p("  Primary innoculum is the source of the first infections in a crop.
                 The source of which is usually from downy mildew resting structures
                 called oospores residing in leaf litter from the previous season.",
                 "Overwintered oospores germinate 'sporangia' progressivly through
                 grape growing season when there is suffifient moisture.",
                 "If conditions remain suitable for sporangia development and
                 survival, zoospore mature within the head of the sporangia.",
                 "When zoospores are mature rainfall events are required for
                 dispersal to a living host leaf.",
                 "Environmental conditions, and leaf moisture, need to remain suitable
                 for infection following dispersal for infection to be successful.",
                 "Following a successful infection the pathogen incubates during the
                 'latent' period', the time between infection and visible symptoms
                 in the form of 'oilspots'"),
               img(src = "2008_Rossi_DownyPrimaryInfection.png",
                   align = "center"),
               p(),
               p(HTML("Figure reproduced from Rossi et al. (2008),  <i>'A mechanistic model simulating primary infections of downy mildew in grapevine'</i>"))),
      #-------------------------------------------------------------------
      tabPanel("Seasonal progress",
               h2("Seasonal progress of residual oospores germinating"),
               h3(textOutput(outputId = "last_mod_time")),
               plotOutput("HT_Plot")),
      #------------------------------------------------------------------------
      tabPanel("Primary dispersals",
               h2("Primary zoospore dispersals from oospore sporangia"),
               h3(textOutput(outputId = "last_mod_time2")),
               verticalLayout(
                  ccs_style(1),
                  code(textOutput(outputId ="txtOosporeGerm" )),
                  column(12,div(style='width:auto;overflow-x: scroll;height:320px;',
                     plotOutput("PI_SPO_plot",width = plot_width,height = "300px"))),
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
                  p("\n"),
                  p("This plot shows how many 'cohorts' or events where oospores
                 have germinated sporangia (GEO_h), which have
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
                           choices = c(0:6,"> 7"),
                           selected = 4),
               numericInput("forecast_rain","Forecast rain days in the next week",
                            min = 0, max = 7,value = 1),
               code(textOutput(outputId ="txtRisk"))

      ) # end risk panel
   )# end risk panel
)# end tabset panel

# Define server logic required to draw a histogram
server <- function(input, output) {

   # Set data source according to weather station selected
   downy_model <- reactive({
      if(input$station == "North Tamborine (QLD)"){
         downy_mod <- DMod_NT
      }
      if(input$station == "Applethorpe (QLD)"){
         downy_mod <- DMod_AT
      }
      if(input$station == "Mildura Airport (VIC)"){
         downy_mod <- DMod_MI
      }
      if(input$station == "Loxton Research station (SA)"){
         downy_mod <- DMod_LX
      }
      if(input$station == "Walpuep Research station (VIC)"){
         downy_mod <- DMod_WA
      }
      downy_mod
   })

   # Model last modified time (lmt)
   # use a reactive expression to render the standardised text
   lmt <- reactive({
      paste("Model last updated for",
            input$station,":",
            as.POSIXct(data.table::last(downy_model()$time_hours),
                       tz = "Australia/Brisbane"))
   })
   # it is output twice, html only allows one output per id so it needs duplication
   output$last_mod_time <- renderText(lmt())
   output$last_mod_time2 <- renderText(lmt())
   last_mod_time <- reactive(max(downy_model()$w$times, na.rm = TRUE))

   # Format model output into a data.table of dates for each stage
   Ddates <- reactive({
      downy_dates <- viticolaR::get_PI_dates(downy_model())

      # arrange categories for plot
      downy_dates[, primary_infection_stage := factor(primary_infection_stage,
                                                 levels = c("GEO_h", "ZRE_ind",
                                                            "ZDI_ind", "ZIN_ind",
                                                            "SUZ_death_ind", "SUS_death_h",
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
                                         primary_infection_stage == "SUS_death_h", hour],
                               Ddates()[cohort == i &
                                         primary_infection_stage == "GEO_h", hour],
                               units = "hours"))
      }
      spo_sur
   })

   # What are the surviving cohort numbers
   surviving_zoospore <- reactive({
      Ddates()[primary_infection_stage == "ZRE_ind" &
                hour >= (last_mod_time() - 3600),.N]})

   # Are there any current mature zoospores
   output$mature_zoo_time <- renderText({
      if(surviving_zoospore() > 0){
         surviving_zoo_time <-
            Ddates()[primary_infection_stage == "SUZ_death_ind" &
                      hour >= (last_mod_time() - 3600),
                   hour]}else{
                      surviving_zoo_time <- NULL
                   }

      if(is.null(surviving_zoo_time)){
         "No recently matured zoospores"
      }else{
         paste0("Zoospores recently matured at: ",
                surviving_zoo_time,"\n")
      }
   })


   output$img_leaf <- renderImage({
      list(src = "www/grapeleaf_DM_infected.jpg",
           alt = "Grapevine leaf infected with downy mildew")
   }, deleteFile = FALSE)

   # output$w_table <- renderDataTable(NT_weather)
   output$PI_table <-
      renderTable(Ddates()[(
         primary_infection_stage == "ZDI_ind" |
            primary_infection_stage == "ZIN_ind"
      ) &
         is.na(hour) == FALSE, list(cohort,
                                    primary_infection_stage,
                                    Time = as.character(hour))])

   # render plot of hydrothermal time
   output$HT_Plot <- renderPlot({
      plot(
         x = downy_model()$time_hours,
         y = downy_model()$Hyd_t,
         ylim = c(0, 13),
         ylab = "Hydrothemal time",
         xlab = "date",
         type = "l",
         col = "red",
         lwd = 5
      )
      rect(xleft = downy_model()$time_hours[1]-30,
           xright = last(downy_model()$time_hours)+30,
           ybottom = 1.3,
           ytop = 8.6,
           col = rgb(0,0,1,alpha = 0.2),
           border = NA)
      text(x = downy_model()$time_hours[300],
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

   output$PI_SPO_plot <- renderPlot({
      ggplot() +
         geom_ribbon_viticolaR(downy_model())+
         geom_line_viticolaR(downy_model())+
         scale_fill_gradient(name = "Mature Sporangia\ncohorts",
                             low = "#EBE9CF",
                             high = "#ADA205")+
         scale_color_continuous(name = "Immature sporangia\ncohorts")+
         theme_minimal()+
         coord_cartesian(ylim = c(0,1.2))+
         ylab("Progress towards sporangia maturity")+
         theme(legend.position="bottom")+
         geom_rect(aes(xmin = head(downy_model()$time_hours,n = 1),
                       xmax = tail(downy_model()$time_hours,n = 1),
                       ymin = 1,
                       ymax = 2),
                       fill = "grey",
                       alpha = 0.6)+
         scale_x_continuous(breaks = seq(min(downy_model()$time_hours),
                                                 max(downy_model()$time_hours),
                                                 by = 60*60*24*2))+
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


      # Ddates()[primary_infection_stage != "SUS_death_h" &
      #           primary_infection_stage != "INC_h_lower" &
      #           primary_infection_stage != "INC_h_upper" &
      #           is.na(hour) == FALSE &
      #           hour >= as.POSIXct(input$BudBurst)
      #        ,] |>
      #    ggplot(aes(x = hour,
      #               y = primary_infection_stage,
      #               group = factor(cohort)))+
      #    geom_line()+
      #    theme_minimal()
   })

   # text descriptions of model output
   output$txtOosporeGerm <- renderText({
      paste("Total sporangia germinations this season:",
            max(Ddates()$cohort))
   })
   output$txtDeadSpor <- renderText({
      paste("Sporagia deaths before dispersal:",
            max(Ddates()$cohort) - length(zoo_dispersals()))
   }) # is this needed or informative?

   zoo_dispersals <- reactive({
      Ddates()[primary_infection_stage == "ZDI_ind" &
                hour >= as.POSIXct(input$BudBurst) &
                is.na(hour)==FALSE,hour]
   })
   output$txtZooDisp <- renderText({
      paste("Zoospore dispersals to vine leaves:",
            length(zoo_dispersals()))
   })
   output$txtZooInf <- renderText({
      paste("Sucessful zoospore infections:",
            length(Ddates()[primary_infection_stage == "ZDI_ind" &
                             is.na(hour)==FALSE &
                             hour >= as.POSIXct(input$BudBurst),hour]))
   })
   output$txtSporangReady <- renderText({
      paste("Current surviving Sporangia cohorts:",
            surviving_zoospore())
   })
   output$txtlatentPs <- renderText({
      #get number of zoospore infections with no infection symptom estimation
      infected_cohorts <- Ddates()[primary_infection_stage == "ZIN_ind" &
                                    is.na(hour)==FALSE,cohort]
      cohorts_in_latent <-
         Ddates()[cohort %in% infected_cohorts &
                   (primary_infection_stage == "INC_h_upper" |
                       primary_infection_stage == "INC_h_lower")&
                   hour >= last(downy_model()$time_hours),unique(cohort)]

      paste("Infections in latent period:",length(cohorts_in_latent))
   })

   # get time cohorts have been surviving for
   surviving_cz_time <-
      reactive({
         abs(difftime(max(Ddates()[primary_infection_stage == "SUS_death_h",
                                 max(hour, na.rm = TRUE)]),
                      data.table::last(downy_model()$time_hours),
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

      if(surviving_zoospore() == 0){
         # if ther are no surviviing zoospores, estimate the risk based on rainfall
         risk_val <- fcase(quantile(spo_survival(),0,na.rm = TRUE) > (days2rain * 24),4 + (input$forecast_rain*0.7),
                           quantile(spo_survival(),1,na.rm = TRUE) > (days2rain * 24), (input$forecast_rain*0.7),
                           default = 0)
      }else{
      risk_val <- surviving_zoospore() * input$forecast_rain * dry_out_factor}

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
