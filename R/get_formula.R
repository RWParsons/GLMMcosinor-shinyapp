get_formula <- function(component_num,
                        df,
                        family,
                        group,
                        add_interaction,
                        add_interaction_time,
                        outcome,
                        time,
                        period_values,
                        ranef_components,
                        categorical_var,
                        ranef_int) {
  # Get the family argument as a function
  family <- eval(parse(text = family))

  if (!is.null(add_interaction)) {
    group_label_1 <- paste0(group)
    group_label_2 <- paste0("group ='", group, "',")
  } else {
    group_label_1 <- NULL
    group_label_2 <- paste0("group ='", group, "',")
  }

  if (add_interaction_time) {
    time_interaction <- paste0(time, "+")
  } else {
    time_interaction <- NULL
  }

  if (group == "None (default)") {
    group_label_2 <- NULL
  }

  if (!is.null(ranef_int)) {
    ranef_int_paste <- paste0("+", ranef_int)
  } else {
    ranef_int_paste <- NULL
  }

  if (!is.null(ranef_components)) {
    ranef_bit <- paste0("+(1", ranef_int_paste, paste("+", unlist(ranef_components), collapse = "+"), "|", categorical_var, ")")
  } else {
    ranef_bit <- NULL
  }

  # Define the formula as a string to be evaluated
  form_obj <- paste0(outcome, "~", group_label_1, "+", time_interaction, "amp_acro(time_col = ", time, ", n_components =", component_num, ",", group_label_2, "period =c(", paste(period_values, collapse = ", "), "))", ranef_bit)

  # Convert string into formula
  formula <- as.formula(form_obj)
  # generate the cglmm object
  cc_obj <-
    GLMMcosinor::cglmm(
      data = df,
      formula = eval(formula),
      family = family
    )

  # ensure that autoplot() has access to the formula
  cc_obj$cglmm.calls$cglmm$formula <- formula

  return(cc_obj)
}
