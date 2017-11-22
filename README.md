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
# Table 1 - Patient demographics ----
explanatory = c("age", "gender.factor")
dependent = 'hdi_tertile'
data %>%
  summary.factorlist(dependent, explanatory, column = T, p = T, na.include = T, total=T)
```

`summary.factorlist` is also commonly used to summarise any number of variables by an *outcome variable* (say dead yes/no).  

``` r
# Table 2 - Mortality ----
explanatory = c("age", "gender.factor", "hdi_tertile")
dependent = 'mort.factor'
data %>%
  summary.factorlist(dependent, explanatory, column = T, p = T, na.include = T, total=T)
```

### 2. Summarise regression model results in final table format

The second main feature is the ability to create final tables for logistic (`glm`), hierarchical logistic (`lme4::glmer`) and 
Cox proprotional hazard (`survival::coxph`) regression models.

The `summarizer` takes a single dependent variable with a vector of explanatory variable names 
(continuous or categorical variables) to produce a final table for publication including summary statistics, 
univariable and multivariable logistic regression analyses. The first columns are those produced by 
`summary.factorist`. 

``` r
explanatory = c("age", "gender.factor", "hdi_tertile")
dependent = 'mort.factor'
data %>%
  summarizer(dependent, explanatory)
```

Where a multivariable model contains a subset of the variables specified in the full univariable set, this can be specified. 

``` r
explanatory = c("age", "gender.factor", "hdi_tertile")
explanatory.multi = c("age","hdi_tertile")
dependent = 'mort.factor'
data %>%
  summarizer(dependent, explanatory, explanatory.multi)
```

Random effects.

``` r
explanatory = c("age", "gender.factor", "hdi_tertile")
explanatory.multi = c("age","hdi_tertile")
dependent = 'mort.factor'
random_effect = "hdi_countries"

data %>%
  summarizer(dependent, explanatory, explanatory.multi, random.effect)
```

`metrics=TRUE` provides common model metrics. 

Any number of subset models can be manually added on to a `summary.factorlist` table using `summarizer.merge`. 
Note requirement for `glm.id=TRUE`. `fit2df` is a subfunction extracting most common models to a dataframe. 


``` r
explanatory = c("age", "gender.factor", "hdi_tertile")
explanatory.multi = c("age","hdi_tertile")
dependent = 'mort.factor'
random_effect = "hdi_countries"

# Separate tables
data %>%
  summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example.summary
  
data %>%
  glmuni(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (univariable)") -> example.univariable
  
data %>%
  glmmulti(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (multivariable)") -> example.multivariable


data %>%
  glmmixed(dependent, explanatory, random_effect) %>%
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
data %>%
  summary.missing(dependent, explanatory)
```

### 3. Summarise regression model results in plot

Models can be summarized with odds ratio/hazard ratio plots using `or.plot`. 

``` r
# OR plot
data %>%
  or.plot(dependent, explanatory)
```

Previously fitted models can be provided directly using `glmfit`. `Rstan` models are also supported. 
