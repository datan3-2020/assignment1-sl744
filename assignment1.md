Statistical assignment 1
================
Simone Long\_135288
03/02/2020

## Open data (10 points)

``` r
library(tidyverse)
Data <- read_tsv("data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

## Select variables (10 points)

``` r
Data <- Data %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

## Filter observations (10 points)

``` r
Data <- Data %>%
        filter(h_memorig == 1)
```

## Recode data (20 points)

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

``` r
Data <- Data %>%
        mutate(EU = ifelse(h_eumem == 1, 1,
                           ifelse(h_eumem == 2, 0, NA))) %>%
        mutate(sex = recode(h_sex_dv,
                `1` = "male",
                `2` = "female")) %>%
        mutate(agegr = case_when(
          between(h_age_dv, 16, 25) ~ "16 to 25",
          between(h_age_dv, 26, 40) ~ "26 to 40",
          between(h_age_dv, 41, 55) ~ "41 to 55",
          between(h_age_dv, 56, 70) ~ "56 to 70",
          h_age_dv > 70 ~ "over 70"
        ))
```

``` r
EUSupport2 <- table(Data$EU)
knitr::kable(EUSupport2,
             caption = "*EU Support: Recoded*")
```

| Var1 |  Freq |
| :--- | ----: |
| 0    |  9338 |
| 1    | 11118 |

*EU Support: Recoded*

``` r
Sex2 <- table(Data$sex)
knitr::kable(Sex2,
             caption = "*Sex: Recoded*")
```

| Var1   |  Freq |
| :----- | ----: |
| female | 12486 |
| male   | 10470 |

*Sex: Recoded*

``` r
Age2 <- table(Data$agegr)
knitr::kable(Age2,
             caption = "*Age: Recoded*")
```

| Var1     | Freq |
| :------- | ---: |
| 16 to 25 | 2885 |
| 26 to 40 | 4384 |
| 41 to 55 | 6150 |
| 56 to 70 | 5941 |
| over 70  | 3597 |

*Age: Recoded*

## Summarise data (20 points)

``` r
Data %>%
        count(EU) %>%
        mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 3 x 3
    ##      EU     n  perc
    ##   <dbl> <int> <dbl>
    ## 1     0  9338  40.7
    ## 2     1 11118  48.4
    ## 3    NA  2501  10.9

Based on this sample, the majority of UK voters should have voted to
Remain in the UK in the 2016 Referendum, which, of course, did not
happen. One reason for this error in prediction could be the percentage
of missing responses (which, in this sample, amounts to nearly 11%). It
is also likely that, because the data collection spanned 3 years, the
opinions of some sample members changed since their initial vote in the
Referendum. But the sampling technique used in this study could be to
blame; participation is completely voluntary, time-consuming, and
necessitates the ownership of a residence/phone/internet/etc for
completion. Therefore, it is probable that the sample is not entirely
representative of the UK, which could explain the difference between the
study’s findings and the actual result of the Referendum.

## Summarise data by sex and age (30 points)

``` r
Data %>%
        group_by(sex, agegr) %>%
        summarise(
          propLeave = mean(EU == 0, na.rm = TRUE) *  100, 
          propRemain = mean(EU == 1, na.rm = TRUE) * 100, 
          totalLeave = sum(EU == 0, na.rm = TRUE),
          totalRemain = sum(EU == 1, na.rm = TRUE)
        )
```

    ## # A tibble: 11 x 6
    ## # Groups:   sex [3]
    ##    sex    agegr    propLeave propRemain totalLeave totalRemain
    ##    <chr>  <chr>        <dbl>      <dbl>      <int>       <int>
    ##  1 female 16 to 25      27.4       72.6        371         985
    ##  2 female 26 to 40      36.0       64.0        817        1455
    ##  3 female 41 to 55      43.1       56.9       1313        1733
    ##  4 female 56 to 70      49.5       50.5       1448        1476
    ##  5 female over 70       55.8       44.2        910         722
    ##  6 male   16 to 25      33.9       66.1        392         763
    ##  7 male   26 to 40      41.2       58.8        691         985
    ##  8 male   41 to 55      48.3       51.7       1196        1280
    ##  9 male   56 to 70      52.4       47.6       1289        1170
    ## 10 male   over 70       62.4       37.6        911         548
    ## 11 <NA>   16 to 25       0        100            0           1

Based on this sample in Great Britain, it appears that women and men
aged 16-25 were most likely to have voted to remain in the EU, with
women generally expressing a greater likelihood of voting to remain in
all age groups. However, age and voting to leave the EU seem to be
positively correlated, meaning that those aged over 70 tended to choose
to leave, with older men showing consistently greater interest in
leaving than older women.
