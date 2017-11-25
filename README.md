summarizer
==========

The `summarizer` package provides functions that help you create elegant final results tables and charts when modelling. 
Its design follows Hadley Wickham's [tidy tool manifesto](http://tidyverse.tidyverse.org/articles/manifesto.html).

Installation and Documentation
------------------------------

You can install `summarizer` from github with:

``` r
# install.packages("devtools")
devtools::install_github("ewenharrison/summarizer")
```

It is not a dependent, but it is recommended that this package is used together with 
[the tidyverse package](http://blog.revolutionanalytics.com/2016/09/tidyverse.html) which can be installed via:

``` r
install.packages("tidyverse")
```

Main Features
-------------

### 1. Summarise variables/factors by a categorical variable

`summary.factorlist` is a simple wrapper used to summarise any number of variables by a single categorical variable. 
This is usually "Table 1" of a study report. 

``` r
library(summarizer)
library(tidyverse)

# Load dataset
data(colon_s)

# Table 1 - Patient demographics ----
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary.factorlist(dependent, explanatory, p=T)

```

`summary.factorlist` is also commonly used to summarise any number of variables by an *outcome variable* (say dead yes/no).  

``` r
# Table 2 - Mortality ----
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  summary.factorlist(dependent, explanatory)
```

### 2. Summarise regression model results in final table format

The second main feature is the ability to create final tables for logistic (`glm`), hierarchical logistic (`lme4::glmer`) and 
Cox proprotional hazard (`survival::coxph`) regression models.

The `summarizer` takes a single dependent variable with a vector of explanatory variable names 
(continuous or categorical variables) to produce a final table for publication including summary statistics, 
univariable and multivariable logistic regression analyses. The first columns are those produced by 
`summary.factorist`. 

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory)
```

Where a multivariable model contains a subset of the variables specified in the full univariable set, this can be specified. 

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi)
```

Random effects.

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi, random.effect)
```

`metrics=TRUE` provides common model metrics. 

Any number of subset models can be manually added on to a `summary.factorlist` table using `summarizer.merge`. 
Note requirement for `glm.id=TRUE`. `fit2df` is a subfunction extracting most common models to a dataframe. 


``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'

# Separate tables
colon_s %>%
  summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example.summary

colon_s %>%
  glmuni(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (univariable)") -> example.univariable

colon_s %>%
  glmmulti(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (multivariable)") -> example.multivariable


colon_s %>%
  glmmixed(dependent, explanatory, random.effect) %>%
  fit2df(estimate.suffix=" (multilevel") -> example.multilevel

# Pipe together
example.summary %>% 
  summarizer.merge(example.univariable) %>% 
  summarizer.merge(example.multivariable) %>% 
  summarizer.merge(example.multilevel) %>% 
  select(-c(glm.id, index)) -> example.final
example.final

```

Note wrapper `summary.missing` can be useful.

``` r
colon_s %>%
  summary.missing(dependent, explanatory)
```

### 3. Summarise regression model results in plot

Models can be summarized with odds ratio/hazard ratio plots using `or.plot`. 

``` r
# OR plot
colon_s %>%
  or.plot(dependent, explanatory)
```

Previously fitted models can be provided directly using `glmfit`. `Rstan` models are also supported. 
