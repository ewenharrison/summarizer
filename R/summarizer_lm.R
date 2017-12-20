summarizer.lm = function(df, dependent, explanatory, explanatory.multi=NULL, random_effect=NULL, metrics=FALSE){

	# Linear regression model ------------------------------------------
	# Summary table
	summary.out = suppressWarnings(
		summary.factorlist(df, dependent, explanatory, p=FALSE, na.include=FALSE,
											 column=TRUE, total_col=FALSE, orderbytotal=FALSE, glm.id=TRUE)
	)

	# Univariable
	lmuni.out = lmuni(df, dependent, explanatory)
	lmuni.df = fit2df(lmuni.out, estimate.suffix = " (univariable)")

	# Multivariable/Mixed
	if (is.null(random_effect)){
		if (is.null(explanatory.multi)){
			lmmulti.out = lmmulti(df, dependent, explanatory)
		} else {
			lmmulti.out = lmmulti(df, dependent, explanatory.multi)
		}
		lmmulti.df = fit2df(lmmulti.out, metrics=metrics, estimate.suffix = " (multivariable)")
	} else if (!is.null(random_effect)){
		if (is.null(explanatory.multi)){
			lmmulti.out = lmmixed(df, dependent, explanatory, random_effect)
		} else {
			lmmulti.out = lmmixed(df, dependent, explanatory.multi, random_effect)
		}
		lmmulti.df = fit2df(lmmulti.out, metrics=metrics, estimate.suffix = " (multilevel)")
	}

	# Merge dataframes
	# Uni
	df.out = summarizer.merge(summary.out, lmuni.df)

	# Multi
	if (metrics == FALSE){
		df.out = summarizer.merge(df.out, lmmulti.df)
	} else {
		df.out = summarizer.merge(df.out, lmmulti.df[[1]])
	}

	# Label interactions
	na.label = which(is.na(df.out$label))
	df.out$label[na.label] = df.out$glm.id[na.label]
	df.out$levels = as.character(df.out$levels)
	df.out$levels[na.label] = "Interaction"

	# Tidy up
	index_glm.id = which(names(df.out)=="glm.id")
	index_index = which(names(df.out)=="index")
	df.out = df.out[,-c(index_glm.id, index_index)]

	# Add metrics
	if (metrics == TRUE){
		return(list(df.out, lmmulti.df[[2]]))
	} else {
		return(df.out)
	}
}
