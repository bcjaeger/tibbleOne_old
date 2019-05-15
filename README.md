
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KableOne

The goal of KableOne is to make it easy for analysts to include a Table
1 object in both LaTeX and html markdown documents. I made this package
because I was unable to get the html tables I wanted from the existing
table one R packages. Notably, this package has far fewer features than
the outstanding TableOne package, but it can get a readable Table 1
object into a markdown document with less effort.

## Installation

You can install the latest version of KableOne from github with:

``` r
devtools::install_github('bcjaeger/KableOne')
```

## Example

This example shows the basic elements of KableOne. First, we apply some
aesthetic changes to the data:

``` r

library(labelled)
library(tidyverse)
#> -- Attaching packages ------------------------------------------------ tidyverse 1.2.1 --
#> v tibble  2.1.1          v purrr   0.3.2     
#> v tidyr   0.8.3.9000     v dplyr   0.8.0.1   
#> v readr   1.3.1          v stringr 1.4.0     
#> v tibble  2.1.1          v forcats 0.4.0
#> -- Conflicts --------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(survival)
library(kableExtra)
#> 
#> Attaching package: 'kableExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     group_rows
library(KableOne)

data = pbc %>%
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
      labels = c("Drug A", "Drug B")
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
      labels=c("None", "A little", "Lots")
    )
  ) 
```

Next, we label the columns of the data. These labels will be passed
along when our table is made.

``` r

data = data %>%
  set_variable_labels(
    status = "Status at last contact",
    trt = "Treatment group",
    age = 'Age, years',
    sex = 'Sex at birth',
    ascites = 'Ascites',
    bili = 'Bilirubin levels, mg/dl',
    edema = 'Is there Edema?'
  ) %>%
  set_variable_groups(
    Outcomes = c('status'),
    Exposures = c('ascites','bili','edema','trt','albumin','stage')
  ) 
```

Now we can apply the `tibble_one` function to get an object that we can
pass along to an Rmarkdown document.

``` r

tbl_one = data %>%
  tibble_one(
    strat='trt',
    include.allcats = TRUE,
    include.freq = FALSE,
    include.pval = TRUE
  )
```

Last step, we pass `tbl_one` into the `kibble_one` function, which
provides a couple of nice formatting procedures to make the data look
like the type of Table 1 that you may see in a published article.

``` r

kibble_one(
  tbl_one,
  use.groups = TRUE
) %>%
  kable_styling(
    position = 'center',
    bootstrap_options = c('striped')
  )
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

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

Drug A<br>(N = 158)

</th>

<th style="text-align:center;">

Drug B<br>(N = 154)

</th>

<th style="text-align:center;">

P-value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Age, years

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

Sex at birth

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

0.421

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Male

</td>

<td style="text-align:center;">

10.5

</td>

<td style="text-align:center;">

13.3

</td>

<td style="text-align:center;">

9.74

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Female

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

</td>

</tr>

<tr grouplength="14">

<td colspan="5" style="border-bottom: 1px solid;">

<strong>Exposures</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Stage

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Ascites

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

0.567

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

No

</td>

<td style="text-align:center;">

92.3

</td>

<td style="text-align:center;">

91.1

</td>

<td style="text-align:center;">

93.5

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

Yes

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

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Bilirubin levels, mg/dl

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

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Is there Edema?

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

A little

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

Lots

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

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Albumin

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

<tr grouplength="4">

<td colspan="5" style="border-bottom: 1px solid;">

<strong>Outcomes</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Status at last contact

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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

<td style="text-align:left; padding-left: 4em;" indentlevel="2">

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
