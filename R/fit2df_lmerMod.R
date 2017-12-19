# Wrapper for lmmixed
fit2df.lmerMod = function(fit, condense=TRUE, metrics=FALSE, estimate.suffix=""){
	x = fit
	explanatory = names(lme4::fixef(x))
	coef = round(lme4::fixef(x), 2)
	ci = round(lme4::confint.merMod(x, method='Wald'), 2)
	ci = ci[-grep("sig", rownames(ci)),]
	p = round(1-pnorm(abs(summary(x)$coefficients[,3])), 3) # WARNING! Simple conversion of t- to p-values assuming infinite df
	warning("P-value for lmer is estimate assuming t-distribution is normal. Bootstrap for final publication.")
	df.out = data.frame(explanatory, coef, ci[,1], ci[,2], p)
	colnames(df.out) = c("explanatory", paste0("Coefficient", estimate.suffix), "L95", "U95", "p")

	# Remove intercepts
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

	if (condense==TRUE){
		p = paste0("=", sprintf("%.3f", df.out$p))
		p[p == "=0.000"] = "<0.001"
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"Coefficient" = paste0(sprintf("%.2f", df.out$Coefficient), " (", sprintf("%.2f", df.out$L95), " to ",
														 sprintf("%.2f", df.out$U95), ", p", p, ")"))
		colnames(df.out) = c("explanatory", paste0("Coefficient", estimate.suffix))
	}
	# Extract model metrics
	if (metrics==TRUE){
		x = fit
		n_model = length(x@resp$mu)
		n_groups = summary(x)$ngrps
		loglik = round(summary(x)$logLik, 2)
		aic = round(summary(x)$AICtab[[1]], 1)
		metrics.out = paste0(
			"Number in model = ", n_model,
			", Number of groups = ", paste(n_groups, collapse="/"),
			", Log likelihood = ", loglik,
			", REML criterion = ", aic)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}
