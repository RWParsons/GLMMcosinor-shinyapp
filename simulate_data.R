library(GLMMcosinor)

f_sample_id_2 <- function(id_num,
                          n = 30,
                          mesor,
                          amp,
                          acro,
                          family = "gaussian",
                          sd,
                          beta.sd,
                          period,
                          n_components,
                          beta.mesor,
                          beta.amp,
                          beta.acro,
                          beta.group = TRUE
) {
  data <- simulate_cosinor(
    n = n,
    mesor = mesor,
    amp = amp,
    acro = acro,
    family = family,
    sd = sd,
    beta.sd = beta.sd,
    period = period,
    n_components = n_components,
    beta.mesor = beta.mesor,
    beta.amp = beta.amp,
    beta.acro = beta.acro,
    beta.group = beta.group
    
  )
  if(id_num %% 2 == 0) {
    # data$group = 1
    rows_to_keep <- seq_len(nrow(data) %/% 2)
    data <- data[-rows_to_keep, ]
  } else {
    # data$group =0
    rows_to_delete <- seq_len(nrow(data) %/% 2)
    data <- data[rows_to_delete, ]
  }
  data$subject <- id_num
  data
}

dat_mixed_2 <- do.call(
  "rbind",
  lapply(1:15, function(x) {
    f_sample_id_2(
      id_num = x,
      mesor = rnorm(1, mean = 0, sd = 1),
      amp = rnorm(1, mean = 3, sd = 0.5),
      acro = rnorm(1, mean = 1.5, sd = 0.2),
      beta.mesor = rnorm(1, mean = 10, sd = 1),
      beta.amp = rnorm(1, mean = 5, sd = 0.5),
      beta.acro = rnorm(1, mean = 1.5, sd = 0.2),
      period = 24,
      n_components = 1,
      sd = 0.2,
      beta.sd = 0.2
    )
  })
) |>
  dplyr::mutate(subject = as.factor(subject))

mixed_mod_2 <- cglmm(
  Y ~ group + amp_acro(times,
                       n_components = 1,
                       period = 24,
                       group = "group"
  ) + (1 + amp_acro1 | subject),
  data = dat_mixed_2
)
