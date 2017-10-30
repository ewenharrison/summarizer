# Continuous dependent.
summary.factorlist0 <- function(df, dependent, explanatory,  cont="mean", p=FALSE, na.include=FALSE,
															 column=FALSE, total_col=FALSE, orderbytotal=FALSE, glm.id=FALSE,
																na.to.missing = TRUE){

	s = Hmisc::summary.formula(as.formula(paste(dependent, "~", paste(explanatory, collapse="+"))), data = df,
															overall=FALSE, method="response", na.include=na.include,
															fun=function(x) {
																mean = mean(x)
																sd = sd(x)
																L = quantile(x, probs=c(0.25))[[1]]
																median = quantile(x, probs=c(0.5))[[1]]
																U = quantile(x, probs=c(0.75))[[1]]
																return(data.frame(mean, sd, L, median, U))
															}
	)

	# Dataframe
	df.out = data.frame(label=attr(s, "vname"), level=attr(s, "dimnames")[[1]])

	if (cont=="mean"){
		mean.out = sprintf("%.2f", matrix(s[,2]))
		sd.out = sprintf("%.2f", matrix(s[,3]))
		result.out = data.frame(paste0(mean.out, " (", sd.out, ")"))
		colnames(result.out) = "Mean (sd)"
	}

	if (cont=="median"){
		median.out = sprintf("%.2f", matrix(s[,5]))
		L_IQR = sprintf("%.2f", matrix(s[,4]))
		U_IQR = sprintf("%.2f", matrix(s[,6]))
		result.out = data.frame(paste0(median.out, " (", L_IQR, " to ", U_IQR, ")"))
		colnames(result.out) = "Median (IQR)"
	}

	df.out = cbind(df.out, result.out)
	return(df.out)
}
