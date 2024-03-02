get_time_plot_inputs <- function(
    superimpose.data,
    predict.ribbon,
    xmin,
    xmax,
    prediction_length,
    add_ranef,
    categorical_var,
    ci_level,
    cc_obj) {
  ###

  if (is.null(superimpose.data)) {
    superimpose.data_arg <- FALSE
  } else {
    superimpose.data_arg <- superimpose.data
  }

  if (is.null(predict.ribbon)) {
    predict.ribbon_arg <- FALSE
  } else {
    predict.ribbon_arg <- predict.ribbon
  }

  if (is.null(xmin) || is.na(xmin)) {
    xmin_arg <- round(min(cc_obj$newdata[cc_obj$time_name]), digits = 5)
  } else {
    xmin_arg <- xmin
  }

  if (is.null(xmax) || is.na(xmax)) {
    xmax_arg <- round(max(cc_obj$newdata[cc_obj$time_name]), digits = 5)
  } else {
    xmax_arg <- xmax
  }

  if (is.null(prediction_length)) {
    prediction_length_arg <- get_pred_length_out(cc_obj)
  } else {
    prediction_length_arg <- prediction_length
  }

  if (is.null(add_ranef)) {
    ranef_bit <- NULL
  } else {
    ranef_bit <- categorical_var
  }

  autoplot(cc_obj,
    superimpose.data = superimpose.data_arg,
    predict.ribbon = predict.ribbon_arg,
    ranef_plot = ranef_bit,
    xlims = c(xmin_arg, xmax_arg),
    pred.length.out = prediction_length_arg,
    ci_level = ci_level
  )
}
