# Using glmmulti format, run analysis for bootstrap CI ------------------------------------------------
glmmulti.boot <- function(df.in, dependent, explanatory, R=1000){
	formula <- paste(dependent, "~", paste(explanatory, collapse="+"))
	# function to get coefficients
	ci <- function(formula, data, indices) {
		d <- data[indices,]
		fit <- glm(formula, family="binomial", data=d)
		return(fit$coefficients)
	}
	bs.out <- boot::boot(data=df.in, statistic=ci,
								 R=R, formula=formula)
	class(bs.out) = "glmboot"
	return(bs.out)
}
