# Using glmmulti format, run analysis for bootstrap CI ------------------------------------------------
glmmulti.boot <- function(df.in, dependent, explanatory, R=1000){
	require(boot)
	formula <- paste(dependent, "~", paste(explanatory, collapse="+"))
	# function to get coefficients
	ci <- function(formula, data, indices) {
		d <- data[indices,]
		fit <- glm(formula, family="binomial", data=d)
		return(fit$coefficients)
	}
	bs.out <- boot(data=df.in, statistic=ci,
								 R=R, formula=formula)
	return(bs.out)
}
