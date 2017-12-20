summarizer.glm = function(df, dependent, explanatory, explanatory.multi=NULL, random_effect=NULL, metrics=FALSE){

	# Logistic regression ----
		# Summary table
		summary.out = summary.factorlist(df, dependent, explanatory, p=FALSE, na.include=FALSE,
																		 column=TRUE, total_col=FALSE, orderbytotal=FALSE, glm.id=TRUE)

		# Univariable
		glmuni.out = glmuni(df, dependent, explanatory)
		glmuni.df = fit2df(glmuni.out, estimate.suffix = " (univariable)")

		# Multivariable/Mixed
		if (is.null(random_effect)){
			if (is.null(explanatory.multi)){
				glmmulti.out = glmmulti(df, dependent, explanatory)
			} else {
				glmmulti.out = glmmulti(df, dependent, explanatory.multi)
			}
			glmmulti.df = fit2df(glmmulti.out, metrics=metrics, estimate.suffix = " (multivariable)")
		} else if (is.null(random_effect) == FALSE){
			if (is.null(explanatory.multi)){
				glmmulti.out = glmmixed(df, dependent, explanatory, random_effect)
			} else {
				glmmulti.out = glmmixed(df, dependent, explanatory.multi, random_effect)
			}
			glmmulti.df = fit2df(glmmulti.out, metrics=metrics, estimate.suffix = " (multilevel)")
		}

		# Merge dataframes
		# Uni
		df.out = summarizer.merge(summary.out, glmuni.df)

		# Multi
		if (metrics == FALSE){
			df.out = summarizer.merge(df.out, glmmulti.df)
		} else {
			df.out = summarizer.merge(df.out, glmmulti.df[[1]])
		}

		# Label interactions
		na.label = which(is.na(df.out$label))
		df.out$label[na.label] = df.out$glm.id[na.label]
		df.out$levels[na.label] = "Interaction"

		# Tidy up
		index_glm.id = which(names(df.out)=="glm.id")
		index_index = which(names(df.out)=="index")
		df.out = df.out[,-c(index_glm.id, index_index)]

		# Add metrics
		if (metrics == TRUE){
			return(list(df.out, glmmulti.df[[2]]))
		} else {
			return(df.out)
		}
}
