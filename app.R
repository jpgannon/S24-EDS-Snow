################################################################################
# Snow Depth App for New Hampshire
#
# Virginia Tech EDS Capstone
################################################################################

library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)

#Import Data
dataSites <- read.csv("averages_by_hour_allsites.csv")

#Getting different levels
sites <- ordered(dataSites$Site_Name, levels = c("A4", "C3", "D2", "E1"))
timeVars <- ordered(dataSites$Site_Name, levels = c("A4", "C3", "D2", "E1"))

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# UI Code Start
ui <- fluidPage(
  
  # Navbar structure for UI
  navbarPage("SNOW DEPTH NEW HAMPSHIRE", theme = shinytheme("lumen"),
             tabPanel("Snow Depth By Station", fluid = TRUE, icon = icon("snowflake"),
                      tags$style(button_color_css)
             ),
             
             # Sidebar layout for main page
             sidebarLayout(
               sidebarPanel(
                 
                 # Turn on/off the sites
                 titlePanel("Site Characteristics"),
                 fluidRow(
                   column(3,
                          # Select the sites to plot
                          checkboxGroupInput(inputId = "SiteDisplay",
                                             label = "Sites of Interest:",
                                             choices = c("A4" = "A4", "C3" = "C3", "D2" = "D2", "E1" = "E1"),
                                             selected = "A4"),
                          # Select which Division(s) to plot
                          checkboxGroupInput(inputId = "DivisionFinder",
                                             label = "Select Division(s):",
                                             choices = c("DI", "DII", "DIII"),
                                             selected = "DI")
                   )
                 )),
               
                 #Main Panel
                 mainPanel(
                   fluidRow(
                     column(3, offset = 9,
                            radioButtons(inputId = "show_NamesFinder",
                                         label = "Display:",
                                         choices = c("School Names", "City Names", "Neither"),
                                         selected = "School Names")
                   )),
                   
                   #Spinner
                   withSpinner(
                     plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder")
                   ),
                   hr(),
                   fluidRow(
                     column(7,
                            helpText("Tip: Click locations to populate table below with information on schools in a specific area")
                            #actionButton(inputId = "draw", label = "Input Event and Times")
                     ),
                     column(width = 2, offset = 2, conditionalPanel(
                       condition = "output.schoolstableFinder",
                       actionButton(inputId = "FinderClear", label = "Clear Table")
                     ))
                   ),
                   br(),
                   fluidRow(
                     withSpinner(
                       dataTableOutput(outputId = "schoolstableFinder")
                     ) #End of dataTableOutput
                   ) #End of br() fluid row
                 ) #End of Main Pannel
               ) #End of sidebar layout
             ) #End of navbar
  ) #End of UI Code
  
  # Define server
  server <- function(input, output, session) {
    
    BigTop100_finder <- reactive({
      req(input$SiteDisplay)
    })
    
    
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
