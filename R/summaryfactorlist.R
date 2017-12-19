summary.factorlist <- function(df, dependent=NULL, explanatory, cont="mean", p=FALSE, na.include=FALSE,
															 column=FALSE, total_col=FALSE, orderbytotal=FALSE, glm.id=FALSE,
															 na.to.missing = TRUE){
	if(is.data.frame(df)==FALSE) stop("df is not dataframe")
	if(any(class(df) %in% c("tbl_df", "tbl"))) df = data.frame(df) # tbl work different, convert to data.frame
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)){
		warning("No dependent variable(s) provided; defaulting to single-level factor")
		dependent = "all"
		df$all = factor(1, label="all")
	}

	args = list(df=df, dependent=dependent, explanatory=explanatory, cont=cont, p=p, na.include=na.include,
							column=column, total_col=total_col, orderbytotal=orderbytotal, glm.id=glm.id,
							na.to.missing=na.to.missing)

	# Survival object
	d.surv = grepl("Surv[(].*[)]", dependent)

	if(d.surv){
		warning("Dependent variable is a survival object")
		df$all = factor(1, label="all")
		suppressWarnings(
			do.call(summary.factorlist1, args=list(df=df, dependent = "all",  explanatory=explanatory, glm.id=glm.id))
		)
	} else {

		# Extract dependent variable
		d.variable = df[,names(df) %in% dependent]

		if(length(d.variable)==0){
			stop("Dependent variable length is 0")
		}

		# Logical is.factor
		d.isfactor = is.factor(d.variable) |
			is.character(d.variable)

		# Number of levels of dependent
		d.len = length(levels(d.variable))

		# Non-factor case
		if(!d.isfactor){
			warning("Dependent is not a factor and will be treated as a continuous variable")
			do.call(summary.factorlist0, args)
		} else {

			# Factor case
			if (d.len == 1){
				do.call(summary.factorlist1, args)
			} else if (d.len == 2){
				do.call(summary.factorlist2, args)
			} else if (d.len == 3){
				do.call(summary.factorlist3, args)
			} else if (d.len == 4){
				do.call(summary.factorlist4, args)
			} else if (d.len == 5){
				do.call(summary.factorlist5, args)
			}
		}
	}
}
