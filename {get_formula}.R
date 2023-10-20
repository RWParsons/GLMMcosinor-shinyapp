get_formula <- function(component_num, 
                        df,
                        family, 
                        group, 
                        add_interaction,
                        outcome, 
                        time, 
                        period_values
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
  
  # Define the formula as a string to be evaluated 
  form_obj <- paste0(outcome,"~",group_label_1, "+", "amp_acro(time_col = ",time, ", n_components =", component_num ,",",group_label_2,"period =c(",paste(period_values, collapse = ", "), "))")
  # Convert string into formula
  formula <- as.formula(form_obj)
  
  
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
