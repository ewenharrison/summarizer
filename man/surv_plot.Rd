\name{surv.plot}
\alias{surv.plot}
\title{Plot survival curves with number-at-risk table}
\usage{
surv.plot = function(df, dependent, explanatory, ...)
}
\description{
Produce a survival curve plot and number-at-risk table using \code{survminer::ggsurvplot} and \code{summarizer} conventions.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  Survival object of the form \code{Surv(time, status)}}
\item{explanatory}{Character vector of max length 2: quoted name(s) of explanatory variables}
\item{...}{Arguments passed to \code{survminer::ggsurvplot}}
}

\examples{
# Survival plot
data(colon_s)
explanatory = c("perfor.factor")
dependent = "Surv(time, status)"
colon_s \%>\%
  surv.plot(dependent, explanatory, xlab="Time (days)", pval=TRUE, legend="none")
}
