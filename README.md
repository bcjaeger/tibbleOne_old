
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tibbleOne

The goal of `tibbleOne` is to make it easy for analysts to include a
Table 1 object in both LaTeX and html markdown documents. I made this
package because I was unable to get the html tables I wanted from the
existing table one R packages. Notably, this package has far fewer
features than the outstanding TableOne package, but it can get a
readable Table 1 object into a markdown document with less effort.

## Installation

You can install the latest version of tibbleOne from github with:

``` r
devtools::install_github('bcjaeger/tibbleOne')
```

## Example

For a more detailed example, see the ‘start here’ vignette. This example
shows basic elements of `tibbleOne`.

``` r

library(knitr)
library(kableExtra)
library(tibbleOne)
library(tidyverse)
```

The first step should be setting labels for variables that will be in
the table. This can be done using `set_variable_labels` and then
building a `meta` data set. You may also just pipe the labelled dataset
into `tibble_one()`, but it is generally more useful to keep the `meta`
data object in case you need to use the labels for other tables in your
analysis.

``` r

meta <- pbc_tbl1 %>% 
  set_variable_labels(
    status = "Status at last contact",
    trt = "Treatment group",
    age = 'Age',
    sex = 'Sex at birth',
    ascites = 'Ascites',
    bili = 'Bilirubin levels',
    edema = 'Edema',
    albumin = 'Serum Albumin'
  ) %>%
  build_meta()

tbl_one <- tibble_one(
  data = pbc_tbl1,
  meta_data = meta,
  formula = ~ . | trt,
  include_pval = TRUE
)
```

Last step, we pass `tbl_one` into the `to_kable()` function, which
provides a couple of nice formatting procedures to make the data look
like the type of Table 1 that you may see in a published article.

``` r

cap <- 'Characteristics of patients with primary biliarry cirrhosis.'

tbl_one %>% 
  to_kable(caption = cap) %>%
  kable_styling(
    position = 'center',
    bootstrap_options = c('striped')
  )
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<caption>

Characteristics of patients with primary biliarry
cirrhosis.

</caption>

<thead>

<tr>

<th style="border-bottom:hidden" colspan="2">

</th>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; font-weight: bold; " colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Treatment group

</div>

</th>

<th style="border-bottom:hidden" colspan="1">

</th>

</tr>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:center;">

Overall<br>(N = 418)

</th>

<th style="text-align:center;">

D-penicillmain<br>(N = 158)

</th>

<th style="text-align:center;">

Placebo<br>(N = 154)

</th>

<th style="text-align:center;">

P-value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Age

</td>

<td style="text-align:center;">

50.7 (10.4)

</td>

<td style="text-align:center;">

51.4 (11.0)

</td>

<td style="text-align:center;">

48.6 (9.96)

</td>

<td style="text-align:center;">

0.018

</td>

</tr>

<tr>

<td style="text-align:left;">

Female, %

</td>

<td style="text-align:center;">

89.5

</td>

<td style="text-align:center;">

86.7

</td>

<td style="text-align:center;">

90.3

</td>

<td style="text-align:center;">

0.421

</td>

</tr>

<tr>

<td style="text-align:left;">

Status at last contact, %

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

0.894

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Censored

</td>

<td style="text-align:center;">

55.5

</td>

<td style="text-align:center;">

52.5

</td>

<td style="text-align:center;">

55.2

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Transplant

</td>

<td style="text-align:center;">

5.98

</td>

<td style="text-align:center;">

6.33

</td>

<td style="text-align:center;">

5.84

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Dead

</td>

<td style="text-align:center;">

38.5

</td>

<td style="text-align:center;">

41.1

</td>

<td style="text-align:center;">

39.0

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Stage, %

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

0.201

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

One

</td>

<td style="text-align:center;">

5.10

</td>

<td style="text-align:center;">

7.59

</td>

<td style="text-align:center;">

2.60

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Two

</td>

<td style="text-align:center;">

22.3

</td>

<td style="text-align:center;">

22.2

</td>

<td style="text-align:center;">

20.8

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Three

</td>

<td style="text-align:center;">

37.6

</td>

<td style="text-align:center;">

35.4

</td>

<td style="text-align:center;">

41.6

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Four

</td>

<td style="text-align:center;">

35.0

</td>

<td style="text-align:center;">

34.8

</td>

<td style="text-align:center;">

35.1

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Ascites, %

</td>

<td style="text-align:center;">

7.69

</td>

<td style="text-align:center;">

8.86

</td>

<td style="text-align:center;">

6.49

</td>

<td style="text-align:center;">

0.567

</td>

</tr>

<tr>

<td style="text-align:left;">

Bilirubin levels

</td>

<td style="text-align:center;">

3.22 (4.41)

</td>

<td style="text-align:center;">

2.87 (3.63)

</td>

<td style="text-align:center;">

3.65 (5.28)

</td>

<td style="text-align:center;">

0.133

</td>

</tr>

<tr>

<td style="text-align:left;">

Edema, %

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

0.877

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

None

</td>

<td style="text-align:center;">

84.7

</td>

<td style="text-align:center;">

83.5

</td>

<td style="text-align:center;">

85.1

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Untreated or successfully treated

</td>

<td style="text-align:center;">

10.5

</td>

<td style="text-align:center;">

10.1

</td>

<td style="text-align:center;">

8.44

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Treatment resistant

</td>

<td style="text-align:center;">

4.78

</td>

<td style="text-align:center;">

6.33

</td>

<td style="text-align:center;">

6.49

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Serum Albumin

</td>

<td style="text-align:center;">

3.50 (0.42)

</td>

<td style="text-align:center;">

3.52 (0.44)

</td>

<td style="text-align:center;">

3.52 (0.40)

</td>

<td style="text-align:center;">

0.874

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border:0;" colspan="100%">

<sup></sup>

</td>

</tr>

</tfoot>

<tfoot>

<tr>

<td style="padding: 0; border:0;" colspan="100%">

<sup>\*</sup> Table values are mean (standard deviation) and percent for
continuous and categorical variables, respectively.

</td>

</tr>

</tfoot>

</table>
