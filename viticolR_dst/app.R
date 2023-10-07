library(shiny)
library(data.table)
library(viticolaR)
library(DT)
library(ggplot2)

load("~/Weather observations/DM_dst_data.rda")


# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel(div(HTML("Downy Mildew (<i>Plasmodium viticola</i>) decision support tool"))),

   tabsetPanel(
      tabPanel("viticolR",
               h2("Splash page"),
               p("A mechanistic compartment model to assist in fungicide decision
                 support for downy mildew (_Plasmodia viticola_) infections in
                 grapevines."),

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
               h3("North Tamborine mountain"),
               plotOutput("HT_Plot")),
      tabPanel("Primary disepersals",
               h2("Primary zoospore dispersals from oospore sporangia"),
               h3("North Tamborine mountain"),
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
                 present as oilspots between INC_h_lower and INC_h_upper.")))


    # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    #
    #     # Show a plot of the generated distribution
    #     mainPanel("text")
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$img_leaf <- renderImage({
      list(src = "www/grapeleaf_DM_infected.jpg",
           alt = "Grapevine leaf infected with downy mildew")
   },deleteFile = FALSE)

   output$w_table <- renderDataTable(NT_weather)
   output$PI_table <- renderDataTable(Ddates)

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


}

# Run the application
shinyApp(ui = ui, server = server)
