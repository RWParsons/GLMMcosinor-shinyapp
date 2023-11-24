get_formula <- function(component_num, 
                        df,
                        family, 
                        group, 
                        add_interaction,
                        outcome, 
                        time, 
                        period_values, 
                        ranef_components, 
                        categorical_var
                        ) {


  #Get the family argument as a function 
  family <- eval(parse(text = family))
  
  
  
  if(!is.null(add_interaction)){
    group_label_1 <- paste0(group)
    group_label_2 <- paste0("group ='",group,"',")
  } else {
    group_label_1 <- NULL 
    group_label_2 <- paste0("group ='",group,"',")
  }
  
  if(group == "None (default)") {
    group_label_2 <- NULL 
  }
  
  if(!is.null(ranef_components)) {
  ranef_bit <- paste0("+(",paste(unlist(ranef_components), collapse = "+"), "|",categorical_var, ")")
  } else {
    ranef_bit <- NULL
  }
  # Define the formula as a string to be evaluated 
  form_obj <- paste0(outcome,"~",group_label_1, "+", "amp_acro(time_col = ",time, ", n_components =", component_num ,",",group_label_2,"period =c(",paste(period_values, collapse = ", "), "))",ranef_bit)
  
  # Convert string into formula
  formula <- as.formula(form_obj)
  
  browser()
  #generate the cglmm object 
  cc_obj <- 
    GLMMcosinor::cglmm(
      data = df, 
      formula = eval(formula),
      family = family
    )
  
  # ensure that autoplot() has access to the formula 
  cc_obj$cglmm.calls$cglmm$formula = formula 
  
  return(cc_obj)
}


get_UI_formula <- function(component_num, 
                           df,
                           family_arg, 
                           group, 
                           add_interaction,
                           outcome, 
                           time, 
                           period_values, 
                           file_name
                           ) {
  
  if(is.null(outcome)) {
    outcome <- "Y" 
  } else {
    outcome <- outcome
  }
  
  if(is.null(group)|| group == "None (default)") {
    group <- NULL 
    group_label_1 <- NULL 
    group_label_2 <- paste0('group = NULL')
  } else {
    #since 'group' appears at two different points in the formula, the 
    #formatting is slightly different: 
    group_label_1 <- paste0(group,"+") 
    group_label_2 <- paste0("group ='",group,"'")
  }
  
  
  
  group <- group 
  if(!is.null(add_interaction)) {
    if(!add_interaction){
      group_label_1 <- NULL 
    } 
  }
  
  if(is.null(component_num)) {
    component_num <- 1
    period_values <- 1
  } else {
    component_num <- component_num
  }
  
  if(length(period_values)>1) {
    period_values <- paste0("c(",paste(period_values, collapse = ", "),")")
  }
  
  if(is.null(time)) {
    time <- "time"
  } else  {
    time <- time
  }
  
  if(is.null(family_arg)) {
    family_arg <- "gaussian"
  } else {
    family_arg <- family_arg
  }

  # Define the formula as a string to be evaluated 
  form_obj <- paste0(outcome," ~ ",group_label_1, " amp_acro(time_col = ",time, ",   n_components =", component_num ,",  ",group_label_2,", period =",period_values, ")")
  #formula <- as.formula(form_obj)
  
  
  
  if (!is.null(file_name)) {
    file_name <- sub(".csv$", "", file_name)
  } else {
    file_name <- "NULL"
  }
  
  if(!is.null(family_arg)) {
    family_name <-  family_arg
  } else {
    family_name <- "NULL"
  }
  form_obj <- paste0("cglmm(formula = ",form_obj, ", data = ",file_name, ", family = ", family_name, ")")
  # You can customize the formula creation based on your specific requirements
  return(form_obj)
}
