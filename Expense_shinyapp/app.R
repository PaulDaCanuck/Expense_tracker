#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that ingests a file and displays it
ui <- fluidPage(

    # Application title
    titlePanel("Upload and Explore Expense File"),

    # Sidebar layout with input and output definitions 
    sidebarLayout(
        
        #Sidebar panel for inputs
        sidebarPanel(
            
            #Input: Select a file
            fileInput("file1", "Choose CSV Expense File", 
                      multiple = FALSE,
                      accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
            ),

        # Main panel for displaying outputs
        mainPanel(
            
            # Output: Data file
            tableOutput("contents")
        )
    )
)

server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath)
        
        return(df)
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
