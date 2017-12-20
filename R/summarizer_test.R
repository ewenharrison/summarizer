# Needs names/suffix methods updated if working ok

summarizer.test = function(df, dependent, explanatory, explanatory.multi=NULL, random_effect=NULL, metrics=FALSE){
	if(is.data.frame(df)==FALSE) stop("df is not dataframe")
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)) stop("No dependent variable provided")

	args = list(df=df, dependent=dependent, explanatory=explanatory, explanatory.multi=explanatory.multi,
							random_effect=random_effect, metrics=metrics)

	# What is dependent variable
	d.variable = df[,names(df) %in% dependent]
	d.is.factor = is.factor(d.variable) |
		is.character(d.variable)
	d.is.surv = grepl("^Surv[(].*[)]", dependent)

	# Send to method
	if (!d.is.factor & !d.is.surv){
		do.call(summarizer.lm, args)
	} else if (d.is.factor & !d.is.surv){
		do.call(summarizer.glm, args)
	} else if (!d.is.factor & d.is.surv){
		do.call(summarizer.cph, args)
	}
}
