Flu Data Analysis
================
Nikhil Gupta
2020-10-07 05:38:06

  - [Response Variable](#response-variable)
  - [Modeling](#modeling)
      - [Stationarity](#stationarity)
      - [Seasonal ARIMA Model](#seasonal-arima-model)
          - [Model ID](#model-id)
          - [Model Fit](#model-fit)
              - [Evaluation of the
                Residuals](#evaluation-of-the-residuals)
              - [Model Characterisics](#model-characterisics)
      - [Conclusion](#conclusion)

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.0     v purrr   0.3.4
    ## v tibble  3.0.0     v dplyr   0.8.5
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tswge)
library(tswgewrapped)
```

``` r
data = read.csv("../../data/FluNetInteractiveReport.csv", skip = 2)
data %>% glimpse()
```

    ## Rows: 261
    ## Columns: 22
    ## $ Country           <fct> United States of America, United States of Americ...
    ## $ WHOREGION         <fct> Region of the Americas of WHO, Region of the Amer...
    ## $ FLUREGION         <fct> North America, North America, North America, Nort...
    ## $ Year              <int> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2...
    ## $ Week              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15...
    ## $ SDATE             <fct> 2014-12-29, 2015-01-05, 2015-01-12, 2015-01-19, 2...
    ## $ EDATE             <fct> 2015-01-04, 2015-01-11, 2015-01-18, 2015-01-25, 2...
    ## $ SPEC_RECEIVED_NB  <int> 41579, 38493, 35593, 34191, 32001, 29578, 26986, ...
    ## $ SPEC_PROCESSED_NB <int> 41579, 38493, 35593, 34191, 32001, 29578, 26986, ...
    ## $ AH1               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ AH1N12009         <int> 10, 10, 8, 4, 10, 10, 7, 10, 18, 5, 7, 2, 7, 4, 7...
    ## $ AH3               <int> 5292, 4901, 4584, 3917, 3475, 2719, 1744, 1242, 9...
    ## $ AH5               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ ANOTSUBTYPED      <int> 5695, 3955, 3580, 3513, 2789, 2170, 1575, 1108, 7...
    ## $ INF_A             <int> 10997, 8866, 8172, 7435, 6274, 4899, 3326, 2360, ...
    ## $ BYAMAGATA         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ BVICTORIA         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ BNOTDETERMINED    <int> 409, 372, 473, 548, 606, 723, 869, 1009, 1127, 13...
    ## $ INF_B             <int> 409, 372, 473, 548, 606, 723, 869, 1009, 1127, 13...
    ## $ ALL_INF           <int> 11406, 9238, 8645, 7983, 6880, 5622, 4195, 3369, ...
    ## $ ALL_INF2          <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ TITLE             <fct> Widespread Outbreak, Widespread Outbreak, Widespr...

# Response Variable

``` r
flu = data$ALL_INF
```

``` r
px = plotts.sample.wge(flu, lag.max = 125, trunc = 150)
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

It may be worthwhile to take the log to smooth out the peaks

``` r
log_flu = log(flu)
```

# Modeling

## Stationarity

``` r
tswgewrapped::check_stationarity(log_flu, ylab = 'Flu Infections (log)', title = 'Flu Infections over Time in US')
```

    ## Loading required namespace: ggfortify

    ## Loading required namespace: patchwork

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## [1] TRUE

**Condition 1: Constant Mean**

The mean does not appear to be constant over time. Therefore, the
assumption of constant mean appears to be violated.

**Condition 2: Constant Variance**

Hard to say with just one realization. But if we superimpose year over
year, there may seem to be some periods (especially during peaks) where
the variance might be higher than the non-peak periods. Therefore, the
assumption of constant variance may be violated.

**Condition 3: Constant Autocorrelation**

The ACF of the first and second half of the realization appear to
exhibit similar behavior. Therefore, the assumption of constant
autocorrelation does not appear to be violated.

**Conclusion**

Given the above analysis, there does appear to be sufficient evidence to
suggest that the process generating the realization is not stationary.
We will continue the ananlysis assuming the process generating the
realization is not stationary.

## Seasonal ARIMA Model

### Model ID

There appears to be a seasonality of 52 weeks in the data (can be seen
from the peak at roughly 0.02 in the Parzen Window and from the
periodicity in the ACF plots as well).

There also seems to be a slight upward trend in the data which may be
removed by differencing

Lets remove that to try to make the data stationary

``` r
flu_s52 = tswge::artrans.wge(log_flu, phi.tr = c(rep(0,51), 1))
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
px = plotts.sample.wge(flu_s52, lag.max = 125)
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
flu_s52_d1 = tswge::artrans.wge(flu_s52, phi.tr = 1)
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
px = plotts.sample.wge(flu_s52_d1, lag.max = 125)
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
aicbic.tables = tswgewrapped::aicbic(flu_s52_d1, 0:5, 0:5, silent = TRUE, merge = TRUE)
aicbic.tables
```

    ##   p q       aic       bic
    ## 1 3 3 -3.067786        NA
    ## 2 2 0 -3.046056 -2.997918
    ## 3 2 1 -3.038405 -2.974222
    ## 4 3 0 -3.037543 -2.973360
    ## 5 2 2 -3.036931        NA
    ## 6 1 1        NA -2.983055
    ## 7 0 2        NA -2.973138

``` r
aicbic.tables = tswgewrapped::aicbic(flu_s52, 0:5, 0:5, silent = TRUE, merge = TRUE)
aicbic.tables
```

    ##   p q       aic       bic
    ## 1 3 2 -3.117055        NA
    ## 2 3 1 -3.114287 -3.034327
    ## 3 5 0 -3.111197        NA
    ## 4 2 1 -3.110710 -3.046741
    ## 5 3 0 -3.108697 -3.044729
    ## 6 4 0        NA -3.026004
    ## 7 2 2        NA -3.022328

ARMA(2,0) seems to be on the top of the list using both AIC and BIC for
flu\_s52\_d1. We will use this going forward ARMA(3,1) seems to be on
the top of the list using both AIC and BIC for est\_s52. We will use
this going forward

``` r
est_s52_d1 = tswge::est.arma.wge(flu_s52_d1, p = 2, q = 0)
```

    ## 
    ## Coefficients of Original polynomial:  
    ## 0.1765 0.2515 
    ## 
    ## Factor                 Roots                Abs Recip    System Freq 
    ## 1-0.5974B              1.6739               0.5974       0.0000
    ## 1+0.4209B             -2.3758               0.4209       0.5000
    ##   
    ## 

``` r
est_s52    = tswge::est.arma.wge(flu_s52, p = 3, q = 1)
```

    ## 
    ## Coefficients of Original polynomial:  
    ## 1.5591 -0.4153 -0.1786 
    ## 
    ## Factor                 Roots                Abs Recip    System Freq 
    ## 1-1.7801B+0.8086B^2    1.1008+-0.1583i      0.8992       0.0227
    ## 1+0.2209B             -4.5260               0.2209       0.5000
    ##   
    ## 

### Model Fit

``` r
# setup object with unitvariate model
models = list(
  "ARUMA(2,1,0) s=52" = list(phi = est_s52_d1$phi, theta = est_s52_d1$theta, s=52, d=1, vara = est_s52_d1$avar, res = est_s52_d1$res, sliding_ase = TRUE),
  "ARUMA(3,0,1) s=52" = list(phi = est_s52$phi, theta = est_s52$theta, s=52, d=0, vara = est_s52$avar, res = est_s52$res, sliding_ase = TRUE)
)
```

``` r
log_flu = data.frame(log_flu)
head(log_flu)
```

    ##    log_flu
    ## 1 9.341895
    ## 2 9.131081
    ## 3 9.064736
    ## 4 8.985070
    ## 5 8.836374
    ## 6 8.634443

``` r
var_interest = 'log_flu'
n.ahead = 52
batch_size = 208
```

``` r
mdl_compare_uni = tswgewrapped::ModelCompareUnivariate$new(
  data = log_flu$log_flu,
  var_interest = var_interest,
  mdl_list = models,
  n.ahead = n.ahead,
  batch_size = batch_size
)
```

    ## NULL

#### Evaluation of the Residuals

The residuals appear to be consisent with white noise. As secondary
evaluation, the Ljung-Box test does not reject the null hypothesis that
residuals are not white noise.

``` r
tbl = mdl_compare_uni$evaluate_residuals()
```

    ## 
    ## 
    ## Evaluating residuals for model: 'ARUMA(2,1,0) s=52'

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

    ## None of the 'ljung_box' tests rejected the null hypothesis that the data is consistent with white noise at an significance level of  0.05  
    ## 
    ## 
    ## Evaluating residuals for model: 'ARUMA(3,0,1) s=52'

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

    ## None of the 'ljung_box' tests rejected the null hypothesis that the data is consistent with white noise at an significance level of  0.05

#### Model Characterisics

``` r
# mdl_compare_uni$plot_multiple_realizations()
```

``` r
# show sliding window forecasts
tbl = mdl_compare_uni$plot_batch_forecasts(only_sliding = TRUE)
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](flu_data_analysis_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
# show ASE over time (windows)
tbl = mdl_compare_uni$plot_batch_ases(only_sliding = TRUE)
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
tbl <- mdl_compare_uni$plot_boxplot_ases()
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
p = mdl_compare_uni$plot_simple_forecasts()
```

![](flu_data_analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## Conclusion

In conclusion, it seems that the Seasonal ARIMA model without
differencing seems to be performing better in general. The trend is not
appreciable in the logged data to warrant differencing (adding the
Integrated term in the model).
