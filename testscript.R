library(shiny)
library(shinyjs)

# Initialize a reactiveValues to store the sets of options
options_list <- reactiveValues(options = list())
counter <- reactiveVal(0)  # Initialize a counter

# Define your UI
ui <- fluidPage(
  useShinyjs(),
  actionButton("add_ranef", "Add Random Effect"),
  uiOutput("random_effect_inputs")
)

# Define your server
server <- function(input, output, session) {
  # Create the initial set of options
  options_list$options <- list()
  
  observeEvent(input$add_ranef, {
    # Get the current set of options
    current_options <- options_list$options
    counter(counter() + 1)  # Increment the counter
    
    # Create a new set of random effect and categorical variable options together
    new_options <- list(
      lapply(1:(length(current_options) + 1), function(i) {
        checkboxInput(paste0("amp_acro", i), label = paste("Add component", i, "as random effect"))
      }),
      lapply(1:(length(current_options) + 1), function(i) {
        selectInput(paste0("mixed_mod_var", i), paste("For mixed models, select a categorical variable", i, ":"), cols())
      }),
      actionButton(paste0("remove_button_", counter()), "Remove")
    )
    
    # Update the options list with the new set
    options_list$options <- c(current_options, new_options)
  })
  
  observe({
    # Handle the removal of sets of options
    for (i in 1:counter()) {
      observeEvent(input[[paste0("remove_button_", i)]], {
        current_options <- options_list$options
        current_options <- current_options[-((i - 1) * 3 + 1):(i * 3)]
        options_list$options <- current_options
        counter(counter() - 1)
      })
    }
  })
  
  output$random_effect_inputs <- renderUI({
    # Render all the sets of options stored in the options_list
    options <- options_list$options
    do.call(tagList, options)
  })
}

shinyApp(ui, server)
