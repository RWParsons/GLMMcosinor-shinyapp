library(shiny)

ui <- fluidPage(
  titlePanel("Real-time Formula Display"),
  
  sidebarLayout(
    sidebarPanel(
      # Add input elements here (e.g., selectInput, numericInput, etc.)
      selectInput("outcome_var", "Outcome Variable", choices = colnames(mtcars)),
      selectInput("independent_var", "Independent Variable", choices = colnames(mtcars)),
      numericInput("num_components", "Number of Components", value = 1, min = 1, max = 10)
    ),
    
    mainPanel(
      # Display the formula in real-time
      textOutput("formula_text")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to generate the formula based on user inputs
  formula_text <- reactive({
    outcome_var <- input$outcome_var
    independent_var <- input$independent_var
    num_components <- input$num_components
    
    # Create the formula based on user inputs
    formula_str <- paste(outcome_var, "~", paste(rep(paste("pc", 1:num_components), collapse = " + "), "+"), independent_var)
    
    # You can customize the formula creation based on your specific requirements
    
    return(formula_str)
  })
  
  # Render the formula text in the UI
  output$formula_text <- renderText({
    formula_text()
  })
}

shinyApp(ui, server)

