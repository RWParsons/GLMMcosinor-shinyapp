# This function mimics the data processing that occurs in autoplot() in GLMMcosinor
# The purpose is to get a sensible predicted length out for the fitted model
get_pred_length_out <- function(cc_obj) {
  # Vector of time values from the original dataset
  time_vec <- cc_obj$newdata[[cc_obj$time_name]]
  min_period_cycle_count <- round(
    (max(time_vec) - min(time_vec)) / min(cc_obj$period)
  )


  # By default, the predicted length out is calculated to give sufficient
  # resolution to the smallest period.
  pred.length.out <- max(
    min_period_cycle_count * 20,
    400
  )

  return(pred.length.out)
}
