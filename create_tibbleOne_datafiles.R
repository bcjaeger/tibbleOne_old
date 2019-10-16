
library(labelled)
library(dplyr)
library(forcats)
library(survival)
library(kableExtra)
library(flextable)
library(tibbleOne)

pbc_tbl1 = pbc %>%
  dplyr::select(
    age, sex, status, trt, stage, ascites, bili, edema, albumin
  ) %>%
  dplyr::mutate(
    status=factor(
      status, levels = c(0:2),
      labels = c("Censored", "Transplant", "Dead")
    ),
    stage = factor(
      stage, levels = c(1:4),
      labels = c("One", "Two", "Three", "Four")
    ),
    trt=factor(
      trt, levels=c(1:2),
      labels = c("D-penicillmain", "Placebo")
    ),
    ascites=factor(
      ascites, levels=c(0:1),
      labels = c("No", "Yes")
    ),
    sex = fct_recode(
      sex,
      'Male'='m',
      'Female'='f'
    ),
    edema = factor(
      edema,
      levels=c(0, 0.5, 1),
      labels=c(
        "None",
        "Untreated or successfully treated",
        "Treatment resistant"
      )
    )
  )

usethis::use_data(pbc_tbl1, overwrite = TRUE)
