fit2df.glmlist <- function(fit, condense=TRUE, metrics=FALSE, na.to.missing = TRUE, estimate.suffix="", ...){
	x = fit

	if (metrics==TRUE && length(x)>1){
		stop("Metrics only generated for single models: multiple models supplied to function")
	}

	df.out <- plyr::ldply(x, .id = NULL, function(x) {
		explanatory = names(coef(x))
		or = round(exp(coef(x)), 2)
		ci = round(exp(confint(x)), 2)
		p = round(summary(x)$coef[,"Pr(>|z|)"], 3)
		df.out = data.frame(explanatory, or, ci[,1], ci[,2], p)
		colnames(df.out) = c("explanatory", paste0("OR", estimate.suffix), "L95", "U95", "p")
		return(df.out)
	})

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
		x = fit[[1]]
		n_data = dim(x$data)[1]
		n_model = dim(x$model)[1]
		aic = round(x$aic, 1)
		auc = round(pROC::roc(x$y, x$fitted)$auc[1], 3)
		metrics.out = paste0(
			"Number in dataframe = ", n_data,
			", Number in model = ", n_model,
			", Missing = ", n_data-n_model,
			", AIC = ", aic,
			", C-statistic = ", auc)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}
