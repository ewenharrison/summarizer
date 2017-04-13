\name{summarizer}
\alias{summarizer}
\title{Summarize the analysis of a binary dependent variable}
\usage{
	summarizer(df, dependent, explanatory, explanatory.multi=NULL,
	random_effect=NULL, metrics=FALSE)
}
\description{
	A function that takes a single dependent variable with a vector of explanatory variable names (continuous or categorical variables) to produce a final table for publication including summary statistics, univariable and multivariable logistic regression analyses.
}
\arguments{
\item{df}{Dataframe}
\item{dependent}{Character vector of length 1:  name of depdendent variable}
\item{explanatory}{Character vector of any length: name(s) of explanatory variables}
\item{explanatory.multi}{Character vector of any length: name of subset of explanatory variables for multivariable analysis only (must only contain variables contained in explanatory)}
\item{random_effect}{Character vector of length 1: name of random effects variable. When included mixed effects model generated (lme4::glmer)}
\item{metrics}{Logical: include in output useful model metrics in publication format}
}
\examples{
	explanatory = c('age_years', 'gender.factor', 'asa.2.1', 'immsupp.disease',
		'immsupp.med', 'smoker.2', 'path.system.1', 'knife2skin.3groups',
		'time2op.hours.factor', 'urgency.1', 'checklist.1', 'laparoscopic.1',
		'abx_pr_ph.factor', 'contamination.3')
	explanatory.multi = c('age_years', 'gender', 'asa.2.1', 'immsupp.disease',
		'immsupp.med', 'smoker.2', 'path.system.1', 'knife2skin.3groups',
		'urgency.1', 'checklist.1', 'laparoscopic.1',
		'abx_pr_ph.factor', 'contamination.3')
	dependent = "ssi_yn.collapsed"
	summarizer.out = summarizer(data_gs2, dependent, explanatory, metrics=TRUE)
}
