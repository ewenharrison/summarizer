\name{{or.plot}
\alias{or.plot}
\title{Produce odds ratio table and plot.}
\usage{
or.plot = function(df, dependent, explanatory, factorlist=NULL, glmfit=NULL, column_space=c(-0.5, 0, 0.5)){
}
\description{
Produce odds ratio table and plot.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  name of depdendent variable (must have 2 factor levels)}
\item{explanatory}{Character vector of any length: name(s) of explanatory variables}
}

\examples{
dependent = "ssi_yn.collapsed"
explanatory = c('hdi_tertile', 'age_centred', 'asa.1', 'smoker.2')
or.plot(data_gs2, dependent, explanatory)
}
