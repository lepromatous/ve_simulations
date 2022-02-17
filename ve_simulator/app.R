
library(shinyjs)
library(shiny)
library(gt)
library(shinythemes)
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("Simulations for Choosing Statistical Methodologies for VE Studies"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "n.vax.case",
                        label = "Total Number of Vaccinated Cases (Vaccine with COVID)",
                        value = 314),
            textInput(inputId = "n.case",
                      label = "Total Number of Patients with COVID-19 in your Sample (Cases)",
                      value = 1983),
            textInput(inputId = "n.vax.control",
                      label = "Total Number of Vaccinated Controls (Vaccine without COVID)",
                      value = 1386),
            textInput(inputId = "n.control",
                      label = "Total Number of Patients without COVID-19 in your Sample (Controls)",
                      value = 2530),
            actionButton("run", "Click to Run")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           gt_output("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  mod <- eventReactive(input$run, {

      makedata(
        n.vaccinated.case=as.numeric(input$n.vax.case), 
        n.case=as.numeric(input$n.case),
        n.vaccinated.control=as.numeric(input$n.vax.control), 
        n.control=as.numeric(input$n.control)
         )[[2]]
      
    })
  
  output$table <- render_gt({
    mod()
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
