\name{summary.factorlist}
\alias{summary.factorlist}
\title{Summarize a set of factors (or continuous variables) by a dependent variable}
\usage{
summary.factorlist(df, dependent, explanatory, cont=c("mean","median"),
	p=FALSE, na.include=FALSE, column=FALSE, total_col=FALSE,
	orderbytotal=FALSE, glm.id=FALSE, na.to.missing = TRUE)
}
\description{
A function that takes a single dependent variable with a vector of explanatory variable names (continuous or categorical variables) to produce a summary table.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  name of depdendent variable (2 to 5 factor levels)}
\item{explanatory}{Character vector of any length: name(s) of explanatory variables}
\item{cont}{Summary for continuous variables: mean (standard deviation) or median (interquartile range)}
\item{p}{Logical: Include statistical test (see \code{library(Hmisc)})}
\item{na.include}{Logical: include missing data in summary (\code{NA})}
\item{column}{Logical: Compute margins by column rather than row}
\item{total_col}{Logical: include a total column summing across factor levels}
\item{orderbytotal}{Logical: order final table by total column high to low}
\item{glm.id}{Logical: not used directly, allows merging via \code{summary.merge}}
\item{na.to.missing}{Logical: convert \code{NA} to 'Missing' when \code{na.include=TRUE}}
}
\examples{
# summary.factorlist() wraps `Hmisc::summary.formula` to summarise any number of variables by a single categorical
# variable. This is usually "Table 1" of a study report.

library(summarizer)
library(tidyverse)

# Load example dataset, modified version of survival::colon
data(colon_s)

# Table 1 - Patient demographics ----
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s \%>\%
  summary.factorlist(dependent, explanatory, p=T)

# summary.factorlist() is also commonly used to summarise any number of variables by an outcome variable (say dead yes/no).

# Table 2 - 5 yr mortality ----
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s \%>\%
  summary.factorlist(dependent, explanatory)
}
