add_ranef_term <- function(component_num){
  output$categorical_var_mixed_mod <- renderUI({
    if(is.null(cols())) {
      return(NULL)
    }
    selectInput("mixed_mod_var", "For mixed models, select a categorical variable from:", cols())
  })
  
  
  random_effect_inputs <- lapply(1:component_num,function(i){
    checkboxInput(paste0("amp_acro",i),label = paste("Add component",i, "as random effect"))
  })
  
  # output$random_effect_inputs <- renderUI({
  #   if(is.null(cols())) {
  #     return(NULL)
  #   }
  #   random_effect_inputs
  # })
  
  
  if(!is.null(input$random_effect_inputs)) {
    ranef_components <- input$random_effect_inputs
    
  } else {
    ranef_components <- NULL
  }
  
  return(ranef_components)
}