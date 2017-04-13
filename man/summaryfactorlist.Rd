\name{summary.factorlist}
\alias{summary.factorlist}
\title{Summarize a set of factors (or continuous variables) by a dependent variable}
\usage{
summary.factorlist(df, dependent, explanatory, cont=c("mean","median"),
	p=FALSE, na.include=FALSE, column=FALSE, total_col=FALSE,
	orderbytotal=FALSE, glm.id=FALSE)
}
\description{
A function that takes a single dependent variable with a vector of explanatory variable names (continuous or categorical variables) to produce summary table.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  name of depdendent variable (2 to 5 factor levels)}
\item{explanatory}{Character vector of any length: name(s) of explanatory variables}
\item{cont}{Summary for continuous variables: mean (standard deviation) or median (interquartile range)}
\item{p}{Logical: Include statistical test (see Hmisc)}
\item{na.include}{Logical: include missing data in summary (NAs)}
\item{column}{Logical: Compute margins by column rather than row}
\item{total_col}{Logical: include a total column summing across factor levels}
\item{orderbytotal}{Logical: order final table by total column high to low}
\item{glm.id}{Logical: not used directly, allows merging via summary.merge}
}
\examples{
explanatory = c('ssi_yn.factor', 'ssi_yn.collapsed', 'abcess_yn.factor',
	'reintervention_yn.factor', 'mort30.factor', 'los')
out.sum = summary.factorlist(data_gs2, 'hdi_tertile', explanatory,
	cont = "median", p = TRUE, na.include = TRUE, column = TRUE)
out.sum = rm_duplicate_labels(out.sum, na.to.missing = TRUE)
out.sum
}
