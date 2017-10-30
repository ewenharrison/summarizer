\name{rm_duplicate_labels}
\alias{rm_duplicate_labels}
\title{Format a \code{factorlist} object directly}
\usage{
rm_duplicate_labels(factorlist, na.to.missing = FALSE)
}
\description{
Not normally called directly.
}
\arguments{
\item{factorlist}{Output from \code{summary.factorlist}}
\item{na.to.missing}{Label \code{NA} as "Missing" in table}
}

\examples{
library(dplyr)
data_gs2 \%>\%
	filter(contamination.3 == 'Contaminated' | contamination.3 == 'Dirty') \%>\%
	summary.factorlist('hdi_tertile', 'abx_proph_yn.factor', col = T, p = T) \%>\%
	rm_duplicate_labels -> proph2.sum
}
