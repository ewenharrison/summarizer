\name{or.plot}
\alias{or.plot}
\title{Produce odds ratio table and plot}
\usage{
or.plot = function(df, dependent, explanatory, factorlist=NULL, glmfit=NULL, breaks=NULL, column_space=c(-0.5, 0, 0.5))
}
\description{
Produce an odds ratio table and plot from a \code{glm()} or \code{lme4::glmer()} model.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  name of depdendent variable (must have 2 factor levels)}
\item{explanatory}{Character vector of any length: name(s) of explanatory variables}
\item{factorlist}{Option to provide output directly from \code{summary.factorlist()}}
\item{glmfit}{Option to provide output directly from \code{glmmulti()}}
\item{breaks}{Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}}
\item{column_space}{Adjust table column spacing}
}

\examples{
# OR plot
data(colon_s)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s \%>\%
  or.plot(dependent, explanatory)
# Previously fitted models (`glmmulti`) can be provided directly to `glmfit`
}
