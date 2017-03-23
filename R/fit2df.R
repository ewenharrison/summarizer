fit2df <- function(fit, condense=FALSE){
	require(plyr)
	df.out <- ldply(fit, function(x) {
		explanatory = names(coef(x))
		or = round(exp(coef(x)), 2)
		ci = round(exp(confint(x)), 2)
		p = round(summary(x)$coef[,"Pr(>|z|)"], 3)
		df.out = data.frame(
			"explanatory" = explanatory,
			"OR" = or,
			"L95" = ci[,1],
			"U95" = ci[,2],
			p = p)
		return(df.out)
	})
	if (condense==TRUE){
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"OR" = paste0(sprintf("%.2f", df.out$OR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95), ", p=", sprintf("%.3f", df.out$p), ")"))
	}
	# remove intercepts
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]
	return(df.out)
}
