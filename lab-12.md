Lab 12 - Smoking during pregnancy
================
Fiona Wang
Apr 2 2025

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

``` r
data <- ncbirths
```

``` r
glimpse(data)
```

    ## Rows: 1,000
    ## Columns: 13
    ## $ fage           <int> NA, NA, 19, 21, NA, NA, 18, 17, NA, 20, 30, NA, NA, NA,…
    ## $ mage           <int> 13, 14, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16,…
    ## $ mature         <fct> younger mom, younger mom, younger mom, younger mom, you…
    ## $ weeks          <int> 39, 42, 37, 41, 39, 38, 37, 35, 38, 37, 45, 42, 40, 38,…
    ## $ premie         <fct> full term, full term, full term, full term, full term, …
    ## $ visits         <int> 10, 15, 11, 6, 9, 19, 12, 5, 9, 13, 9, 8, 4, 12, 15, 7,…
    ## $ marital        <fct> not married, not married, not married, not married, not…
    ## $ gained         <int> 38, 20, 38, 34, 27, 22, 76, 15, NA, 52, 28, 34, 12, 30,…
    ## $ weight         <dbl> 7.63, 7.88, 6.63, 8.00, 6.38, 5.38, 8.44, 4.69, 8.81, 6…
    ## $ lowbirthweight <fct> not low, not low, not low, not low, not low, low, not l…
    ## $ gender         <fct> male, male, female, male, female, male, male, male, mal…
    ## $ habit          <fct> nonsmoker, nonsmoker, nonsmoker, nonsmoker, nonsmoker, …
    ## $ whitemom       <fct> not white, not white, white, white, not white, not whit…

``` r
boxplot.stats(data$fage)$out
```

    ## [1] 53 55

``` r
boxplot.stats(data$mage)$out
```

    ## [1] 50

``` r
boxplot.stats(data$weeks)$out
```

    ##  [1] 45 24 29 29 31 30 22 32 32 45 29 32 30 22 25 32 24 29 31 32 25 32 25 29 32
    ## [26] 31 22 31 45 26 30 26 26 45 28 28 28 31 30 26 20 32 31

``` r
boxplot.stats(data$visits)$out
```

    ##  [1]  0  2  2  0  0  0 23  2 30  0  0  2  2 30  0 30 24 30 25 26 30

``` r
boxplot.stats(data$gained)$out
```

    ##  [1] 76 75 68 72 70 70 80 85 70 68 72 70 85 77 75 70

``` r
boxplot.stats(data$weight)$out
```

    ##  [1]  1.50  2.63  1.56  1.69  2.88  1.38  2.69  2.50  3.75  2.69  1.00  1.19
    ## [13]  1.44  3.75  1.69  2.25  3.44  1.38  1.88  3.56  3.19  3.31  1.00 11.75
    ## [25]  2.94  1.31  2.88 11.63  3.00  1.44  1.50  3.81  2.88  2.25  1.63  3.25
    ## [37]  3.63  1.38  2.19  3.63

Looking at this tibble, there are a couple numerical variables: father’s
age, mother’s age, length of pregnancy in weeks, number of hospital
visits during pregnancy, weight gained by mother during pregnancy in
pounds, and weight of the baby at birth in pounds. The others are all
categorical. For the numerical variables, all of them have outliers.

### Exercise 1

There are 1000 cases in this dataset. THe cases are the information on
birth record in the state of North Carolina from 2004.

### Exercise 2

``` r
ncbirths_white <- data %>% 
  filter(whitemom == "white")
ncbirths_white %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   mean_weight
    ##         <dbl>
    ## 1        7.25

The mean weights of the white babies is 7.25 pounds.
