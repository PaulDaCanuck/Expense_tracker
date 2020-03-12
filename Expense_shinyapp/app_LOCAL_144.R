#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

# Define UI for application that ingests a file and displays it
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            
        ),
        mainPanel(
            DT::dataTableOutput("contents")
        )
    )
)



server <- function(input, output) {
    
    output$contents <- DT::renderDataTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # df <- 
        
        read_csv(input$file1$datapath,
                        col_names = TRUE,
                        col_types = "ccc"
                       #skip = 8
                        )
        
        
        # return(df)
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
