fit2df.stanfit = function(fit, X, condense=TRUE, metrics=FALSE, na.to.missing = TRUE, estimate.name="OR"){
	stanfit = fit
	pars = "beta"
	quantiles =  c(0.025, 0.50, 0.975)

	# Extract model
	explanatory = attr(X, "dimnames")[[2]]
	model = rstan::summary(stanfit,
									pars = pars,
									probs = quantiles)$summary
	or = round(exp(model[, 1]), 2)
	L95 = round(exp(model[, 4]), 2)
	U95 = round(exp(model[, 6]), 2)

	# Determine a p-value based on two-sided examination of chains
	chains = rstan::extract(stanfit, pars=pars, permuted = TRUE, inc_warmup = FALSE,
									 include = TRUE)
	p1.out = apply(chains[[1]], 2, function(x)mean(x<0))
	p2.out = apply(chains[[1]], 2, function(x)mean(x>0))
	p1.out = p1.out*2
	p2.out = p2.out*2
	p.out = ifelse(p1.out < 1, p1.out, p2.out)
	p = round(p.out, 3)
	df.out = data.frame(explanatory, or, L95, U95, p)
	colnames(df.out) = c("explanatory", estimate.name, "L95", "U95", "p")

	# Remove intercept
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

	# Condensed output (now made default)
	if (condense==TRUE){
		p = paste0("=", sprintf("%.3f", df.out$p))
		p[p == "=0.000"] = "<0.001"
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"OR" = paste0(sprintf("%.2f", df.out$OR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95), ", p", p, ")"))
		colnames(df.out) = c("explanatory", estimate.name)
	}

	# Extract model metrics
	if (metrics==TRUE){
		# n_data = dim(x$data)[1] # no equivalent here
		n_model = dim(X)[1]
		# aic = round(x$aic, 1) # add WAIC later?
		# auc = round(roc(x$y, x$fitted)$auc[1], 3) # Add predicted mu later?
		metrics.out = paste0(
		#	"Number in dataframe = ", n_data,
			", Number in model = ", n_model)
		#	", Missing = ", n_data-n_model,
		#	", AIC = ", aic,
		#	", C-statistic = ", auc)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
	return(df.out)
}
