library(shiny)

ui <- fluidPage(
  
  selectizeInput(inputId = "test", label = "This is a test", choices = c("Hello", "My", "Name", "is", "elder", "Smith"),
                 multiple = T),
  textOutput("mormon")
  
)

server <- function(input, output, session) {
  
  output$mormon <- renderText({
    
    input$test
    
  })
  
}

shinyApp(ui = ui, server = server)