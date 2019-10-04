
#' Mayo Clinic Primary Biliary Cirrhosis
#'
#' @description These data contain a subset of the columns found in
#'   the `pbc` data in the `survival` package. Also, the description
#'   below is taken from the description provided by the `survival`
#'   package.
#'
#    This data is from the Mayo Clinic trial in primary biliary
#'   cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total
#'   of 424 PBC patients, referred to Mayo Clinic during that ten-year
#'   interval, met eligibility criteria for the randomized placebo controlled
#'   trial of the drug D-penicillamine. The first 312 cases in the data set
#'   participated in the randomized trial and contain largely complete data.
#'   The additional 112 cases did not participate in the clinical trial,
#'   but consented to have basic measurements recorded and to be followed
#'   for survival. Six of those cases were lost to follow-up shortly after
#'   diagnosis, so the data here are on an additional 106 cases as well as
#'   the 312 randomized participants.
#'
#' @format a data frame with 418 rows and 9 variables.
#'  - age: in years
#'  - sex: male or female
#'  - status death, censor, or transplant.
#'  - trt D-penicillmain, placebo, and not randomized (NA).
#'  - stage: histologic stage of disease (needs biopsy)
#'  - ascites: presence of ascites
#'  - bili: serum bilirunbin (mg/dl)
#'  - edema: no edema, untreated or successfully treated edema, or
#'    edema despite diuretic therapy
#'  - albumin: serum albumin (g/dl)
#'
#' @source
#'   T Therneau and P Grambsch (2000),
#'   Modeling Survival Data:
#'   Extending the Cox Model,
#'   Springer-Verlag, New York.
#'   ISBN: 0-387-98784-3.
"pbc_tbl1"

