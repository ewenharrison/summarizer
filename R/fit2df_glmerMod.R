# Wrapper for glmmixed
fit2df.glmerMod = function(model, condense=TRUE, metrics=FALSE){
	{
		sum.lem=data.frame(summary(model)$coef)
		sum.lem<-sum.lem[-which(rownames(sum.lem)=="(Intercept)"),]
		sum.lem.p=sum.lem$Pr...z..
		model.or=data.frame(exp(lme4::fixef(model)))
		model.or<-model.or[-which(rownames(model.or)=="(Intercept)"),]
		model.ci = data.frame(exp(confint(model, method="Wald")))
		model.ci<-model.ci[-which(rownames(model.ci)=="(Intercept)"),]
		model.ci<-model.ci[-grep("sig", rownames(model.ci)),]
		df=cbind(round(model.or, digits=2), round(model.ci, digits = 2), round(sum.lem.p, digits=3))
		colnames(df)=c('OR', 'L95', 'U95', 'p')
	}
	df.out=data.frame(df)
	if (condense==TRUE){
		p = paste0("=", sprintf("%.3f", df.out$p))
		p[p == "=0.000"] = "<0.001"
		df.out = data.frame(
			"explanatory" = row.names(df.out),
			"OR" = paste0(sprintf("%.2f", df.out$OR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95), ", p", p, ")"))
	}
	# Extract model metrics
	if (metrics==TRUE){
		x = model
		n_model = length(x@resp$mu)
		n_groups = summary(x)$ngrps
		aic = round(summary(x)$AICtab[[1]], 1)
		auc = round(roc(x@resp$y, x@resp$mu)$auc[1], 3)
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
