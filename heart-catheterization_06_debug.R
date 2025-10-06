library(dplyr)
library(readr)
library(survival)
library(survminer)
library(tidyr)

recode_yes <- function(x, yes = "Yes") {
  if_else(x == yes, 1, 0)
}

read_rhc <- function(file) {
  read_csv(file) |>
    select( # provide the renaming
      ptid,
      age = age,
      sex = sex,
      race = race,
      education = edu,
      income = income,
      med_ins = ninsclas,
      disease = cat1,
      cancer = ca,
      weight = wtkilo1,
      temp = temp1,
      bp = meanbp1,
      resp = resp1,
      hrt = hrt1,
      pf = pafi1,
      ph = ph1,
      death = dth30,
      time = t3d30,
      rhc = swang1
    ) |>
    mutate(across(c(death, cancer, rhc), recode_yes))
}

summarize_diagnosis <- function(data, diagnosis_data, dx) {
  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  }

  if (!is.data.frame(diagnosis_data)) {
    cli::cli_abort("`diagnosis_data` must be a data frame.")
  }

  if (!is_scalar_character(dx)) {
    cli::cli_abort("`dx` must be a character vector of length 1.")
  }

  combined <-
    data |>
    left_join(diagnosis_data, by = "ptid") |>
    pivot_longer(
      cols = cardiohx:amihx,
      names_to = "diagnosis",
      values_to = "present"
    )

  if (!dx %in% combined$diagnosis) {
    cli::cli_abort("{dx} is not a diagnosis included in `diagnosis_data`.")
  }

  combined |>
    group_by(rhc, diagnosis) |>
    summarize(
      total = sum(present),
      total_cat = n()
    ) |>
    ungroup() |>
    mutate(percentage = total / total_cat * 100) |>
    filter(diagnosis == dx) |>
    select(-total_cat)
}

plot_rhc <- function(data, var) {

  col_var <-
    data |>
    pull({{ var }})

  if (is.numeric(col_var)) {
    data |>
      ggplot(aes(x = as.factor(rhc), y = {{ var }})) +
      geom_boxplot() +
      labs(x = "RHC")
  }

  else {
    data |>
      ggplot(aes(x = as.factor(rhc), fill = {{ var }})) +
      geom_bar(position = "dodge") +
      labs(x = "RHC")
  }
}

kaplan_meier <- function(
    data,
    title = "Survival by RHC",
    legend_title = "",
    x = "Survival time",
    y = "Survival probability"
) {

  data <-
    data |>
    mutate(surv_object = Surv(time = time, event = death))

  fit <- survfit(surv_object ~ rhc, data = data)

  fit |>
    ggsurvplot(
      data = data,
      legend.title = legend_title,
      legend.labs = c("No RHC", "RHC"),
      ylab = y,
      xlab = x,
      title = title
    )
}
