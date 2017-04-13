\name{rm_duplicate_labels}
\alias{rm_duplicate_labels}
\title{Format a code{summary.factorlist} \object directly}
\usage{
rm_duplicate_labels(summary.factorlist.output, na.to.missing = FALSE)
}
\description{
Not normally called directly.
}
\arguments{
\item{summary.factorlist.output}{Output from code{summary.factorlist}}
\item{na.to.missing}{Label \code{NA} as "Missing" in table}

\examples{
library(dplyr)
data_gs2 %>%
	filter(contamination.3 == 'Contaminated' | contamination.3 == 'Dirty') %>%
	summary.factorlist('hdi_tertile', 'abx_proph_yn.factor', col = T, p = T) %>%
	rm_duplicate_labels -> proph2.sum
}
