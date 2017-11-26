\name{summarizer}
\alias{summarizer}
\title{Summarize the analysis of a binary dependent variable or survival object}
\usage{
summarizer(df, dependent, explanatory, explanatory.multi=NULL,
	random_effect=NULL, metrics=FALSE)
}
\description{
A function that takes a single dependent variable with a vector of explanatory variable names (continuous or categorical variables) to produce a final table for publication including summary statistics, univariable and multivariable logistic or Cox Proportional Hazards regression analyses.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  name of depdendent variable. Can be survival object of form \code{Surv(time, status)}}
\item{explanatory}{Character vector of any length: name(s) of explanatory variables}
\item{explanatory.multi}{Character vector of any length: name of subset of explanatory variables for multivariable analysis only (must only contain variables contained in \code{explanatory})}
\item{random_effect}{Character vector of length 1: name of random effects variable. When included mixed effects model generated (\code{lme4::glmer})}
\item{metrics}{Logical: include useful model metrics in output in publication format}
}
\examples{
library(tidyverse)
library(summarizer)

# Summary, univariable and multivariable analyses of the form:
# glm(depdendent ~ explanatory, family="binomial")
# glmuni(), glmmulti(), glmmixed(), glmmultiboot(), coxphuni(), coxphmulti()

data(colon_s) # Modified from survival::colon
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s \%>\%
  summarizer(dependent, explanatory)

# Multivariable analysis with subset of explanatory variable set used in univariable analysis
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s \%>\%
  summarizer(dependent, explanatory, explanatory.multi)

# Summary, univariable and multivariable analyses of the form:
lme4::glmer(dependent ~ explanatory + (1 | random_effect), family="binomial")

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'
colon_s \%>\%
  summarizer(dependent, explanatory, explanatory.multi, random.effect)

# Include model metrics:
colon_s \%>\%
  summarizer(dependent, explanatory, explanatory.multi,  metrics=TRUE)

# Summary, univariable and multivariable analyses of the form:
survival::coxph(dependent ~ explanatory)

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"

colon_s \%>\%
	summarizer(dependent, explanatory)

# Rather than going all-in-one, any number of subset models can be manually added on to a
# summary.factorlist() table using summarizer.merge().
# This is particularly useful when models take a long-time to run or are complicated.

# Note requirement for glm.id=TRUE. `fit2df` is a subfunction extracting most common models to a dataframe.

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'

# Separate tables
colon_s \%>\%
  summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example.summary

colon_s \%>\%
  glmuni(dependent, explanatory) \%>\%
  fit2df(estimate.suffix=" (univariable)") -> example.univariable

colon_s \%>\%
  glmmulti(dependent, explanatory) \%>\%
  fit2df(estimate.suffix=" (multivariable)") -> example.multivariable

colon_s \%>\%
  glmmixed(dependent, explanatory, random.effect) \%>\%
  fit2df(estimate.suffix=" (multilevel") -> example.multilevel

# Pipe together
example.summary \%>\%
  summarizer.merge(example.univariable) \%>\%
  summarizer.merge(example.multivariable) \%>\%
  summarizer.merge(example.multilevel) \%>\%
  select(-c(glm.id, index)) -> example.final
example.final


# Cox Proportional Hazards example with separate tables merged together.

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
dependent = "Surv(time, status)"

# Separate tables
colon_s \%>\%
	summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example2.summary

colon_s \%>\%
	coxphuni(dependent, explanatory) \%>\%
	fit2df(estimate.suffix=" (univariable)") -> example2.univariable

colon_s \%>\%
  coxphmulti(dependent, explanatory.multi) \%>\%
  fit2df(estimate.suffix=" (multivariable)") -> example2.multivariable

# Pipe together
example2.summary \%>\%
	summarizer.merge(example2.univariable) \%>\%
	summarizer.merge(example2.multivariable) \%>\%
	select(-c(glm.id, index)) -> example2.final
example2.final
}
