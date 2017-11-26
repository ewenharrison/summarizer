\name{summarizer.merge}
\alias{summarizer.merge}
\title{Merge a summary.factorlist() table with any number of glm / coxph model tables.}
\usage{
summarizer.merge = function(factorlist, glm.df, ref.symbol = "-")
}
\description{
A function that takes the output from \code{summary.factorlist(..., glm.id=TRUE)} and merges with any number of model dataframes, usually produced with a model wrapper followed by \code{fit2df()} function (see examples).
}
\arguments{
\item{factorlist}{Output from \code{summary.factorlist(..., glm.id=TRUE)}}
\item{glm.df}{From model wrappers followed by \code{fit2df()}}
\item{ref.symbol}{Reference symbol for model reference level, typically "-" or "1.0"}
}
\examples{
library(tidyverse)
library(summarizer)

data(colon_s)
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
}
