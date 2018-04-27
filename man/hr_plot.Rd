\name{hr.plot}
\alias{hr.plot}
\title{Produce hazard ratio table and plot}
\usage{
hr.plot = function(df, dependent, explanatory, dependent_label = NULL, factorlist=NULL, coxfit=NULL, breaks=NULL, column_space=c(-0.5, 0, 0.5))
}
\description{
Produce hazard ratio table and plot from a Cox Proportional Hazards analysis, \code{survival::coxph()}.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  Survival object of the form \code{Surv(time, status)}}
\item{explanatory}{Character vector of any length: name(s) of explanatory variables}
\item{dependent-label}{Label on plot for survival object, e.g. "Overall survival"}
\item{factorlist}{Option to provide output directly from \code{summary.factorlist()}}
\item{coxfit}{Option to provide output directly from \code{coxphmulti()}}
\item{breaks}{Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}}
\item{column_space}{Adjust table column spacing}
}

\examples{
# HR plot
data(colon_s)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s \%>\%
  hr.plot(dependent, explanatory, dependent_label = "Survival")
# Previously fitted models (`coxphmulti`) can be provided directly using `coxfit`
}
