# Needs names/suffix methods updated if working ok

summarizer = function(df, dependent, explanatory, explanatory.multi=NULL, random_effect=NULL, metrics=FALSE){
	if(is.data.frame(df)==FALSE) stop("df is not dataframe")
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)) stop("No dependent variable provided")

	# Logistic regression or CPH ----
	coxfit = grepl("^Surv[(].*[)]", dependent)
	if (!coxfit){
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



	} else if (coxfit) {
		# Cox proprotional hazards model -----------------------------------------------------------
		# Summary table
		summary.out = suppressWarnings(
			summary.factorlist(df, dependent=NULL, explanatory, glm.id=TRUE)
		)
		summary.out = summary.out[,-3] # Remove 'all' column with total counts

		# Univariable
		coxphuni.out = coxphuni(df, dependent, explanatory)
		coxphuni.df = fit2df(coxphuni.out, estimate.suffix = " (univariable)")

		# Multivariable/Mixed
		if (is.null(explanatory.multi)){
			coxphmulti.out = coxphmulti(df, dependent, explanatory)
		} else {
			coxphmulti.out = coxphmulti(df, dependent, explanatory.multi)
		}
		coxphmulti.df = fit2df(coxphmulti.out, estimate.suffix = " (multivariable)") #, metrics=metrics)

		# Merge dataframes
		# Uni
		df.out = summarizer.merge(summary.out, coxphuni.df)

		# Multi
		df.out = summarizer.merge(df.out, coxphmulti.df)

		# # Multi
		# if (metrics == FALSE){
		# 	df.out = summarizer.merge(df.out, glmmulti.df)
		# } else {
		# 	df.out = summarizer.merge(df.out, glmmulti.df[[1]])
		# }
		#
		# if (is.null(random_effect)){
		# 	names(df.out)[which(names(df.out)=="OR")] = "OR (multivariable)"
		# } else {
		# 	names(df.out)[which(names(df.out)=="OR")] = "OR (multilevel)"
		# }

		# Label interactions
		na.label = which(is.na(df.out$label))
		df.out$label[na.label] = df.out$glm.id[na.label]
		df.out$levels[na.label] = "Interaction"

		# Tidy up
		index_glm.id = which(names(df.out)=="glm.id")
		index_index = which(names(df.out)=="index")
		df.out = df.out[,-c(index_glm.id, index_index)]
		return(df.out)
		# Add metrics
		# if (metrics == TRUE){
		# 	return(list(df.out, glmmulti.df[[2]]))
		# } else {
		# 	return(df.out)
		# }
	}
}
