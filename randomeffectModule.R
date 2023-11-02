# Define a module to manage random effect and categorical variable options
randomEffectModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("add_ranef"), "Add Random Effect"),
    uiOutput(ns("random_effect_inputs"))
  )
}

randomEffectModule <- function(input, output, session) {
  options_list <- reactiveVal(list())
  
  observeEvent(input$add_ranef, {
    current_options <- options_list()
    
    new_options <- list(
      lapply(1:(length(current_options) + 1), function(i) {
        checkboxInput(paste0("amp_acro", i), label = paste("Add component", i, "as random effect"))
      }),
      lapply(1:(length(current_options) + 1), function(i) {
        selectInput(paste0("mixed_mod_var", i), paste("For mixed models, select a categorical variable", i, ":"), cols())
      }),
      actionButton(paste0("remove_button", length(current_options) + 1), "Remove")
    )
    
    options_list(c(current_options, new_options))
  })
  
  for (i in 1:length(options_list())) {
    observeEvent(input[[paste0("remove_button", i)]], {
      current_options <- options_list()
      current_options <- current_options[-((i - 1) * 3 + 1):(i * 3)]
      options_list(current_options)
    })
  }
  
  output$random_effect_inputs <- renderUI({
    options <- options_list()
    do.call(tagList, options)
  })
}

