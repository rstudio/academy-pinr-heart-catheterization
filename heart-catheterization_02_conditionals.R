library(dplyr)
library(readr)
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

