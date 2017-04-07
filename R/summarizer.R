summarizer = function(df, dependent=dependent, explanatory=explanatory, random_effect=NULL){
	# Summary table
	summary.out = summary.factorlist(df, dependent, explanatory, p=FALSE, na.include=FALSE,
																	 column=TRUE, total_col=FALSE, orderbytotal=FALSE, glm.id=TRUE)

	# Remove duplicate labels
	summary.out = rm_duplicate_labels(summary.out, na.to.missing = TRUE)

	# Univariable
	glmuni.out = fit2df(
		glmuni(df, dependent, explanatory),
		condense = TRUE
	)

	# Multivariable/Mixed
	if (is.null(random_effect)){
		glmmulti.out = fit2df(
			glmmulti(df, dependent, explanatory),
			condense = TRUE
		)
	} else {
		glmmixed.out = fit2df.mixed(
			glmmixed(df, dependent, explanatory, random_effect),
			condense = TRUE
		)
	}

	# Merge dataframes
	df.out = summarizer.merge(summary.out, glmuni.out)
	names(df.out)[which(names(df.out)=="OR")] = "OR (univariable)"
	if (is.null(random_effect)){
		df.out = summarizer.merge(df.out, glmmulti.out)
		names(df.out)[which(names(df.out)=="OR")] = "OR (multivariable)"
	} else {
		df.out = summarizer.merge(df.out, glmmixed.out)
		names(df.out)[which(names(df.out)=="OR")] = "OR (multilevel)"
	}

	# Tidy up
	index_glm.id = which(names(df.out)=="glm.id")
	index_index = which(names(df.out)=="index")
	df.out = df.out[,-c(index_glm.id, index_index)]
	return(df.out)
}
