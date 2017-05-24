# Wrapper for glmmixed
fit2df.glmerMod = function(fit, condense=TRUE, metrics=FALSE, estimate.suffix=""){
	x = fit
	explanatory = names(lme4::fixef(x))
	or = round(exp(lme4::fixef(x)), 2)
	ci = round(exp(lme4::confint.merMod(x, method='Wald')), 2)
	ci = ci[-grep("sig", rownames(ci)),]
	p = round(summary(x)$coef[,"Pr(>|z|)"], 3)
	df.out = data.frame(explanatory, or, ci[,1], ci[,2], p)
	colnames(df.out) = c("explanatory", paste0("OR", estimate.suffix), "L95", "U95", "p")

	# Remove intercepts
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

	if (condense==TRUE){
		p = paste0("=", sprintf("%.3f", df.out$p))
		p[p == "=0.000"] = "<0.001"
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"OR" = paste0(sprintf("%.2f", df.out$OR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95), ", p", p, ")"))
		colnames(df.out) = c("explanatory", paste0("OR", estimate.suffix))
	}
	# Extract model metrics
	if (metrics==TRUE){
		x = model
		n_model = length(x@resp$mu)
		n_groups = summary(x)$ngrps
		aic = round(summary(x)$AICtab[[1]], 1)
		auc = round(pROC::roc(x@resp$y, x@resp$mu)$auc[1], 3)
		metrics.out = paste0(
			"Number in model = ", n_model,
			", Number of groups = ", paste(n_groups, collapse="/"),
			", AIC = ", aic,
			", C-statistic = ", auc)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}
