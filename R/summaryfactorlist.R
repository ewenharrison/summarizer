summary.factorlist <- function(df, dependent, explanatory, cont="mean", p=FALSE, na.include=FALSE,
															 column=FALSE, total_col=FALSE, orderbytotal=FALSE, glm.id=FALSE,
															 na.to.missing = TRUE){
	try(if(is.data.frame(df)==FALSE) stop("df is not dataframe"))
	try(if(is.null(dependent)) stop("No dependent variable(s) provided"))
	try(if(is.null(explanatory)) stop("No explanatory variable(s) provided"))

	args = list(df, dependent=dependent, explanatory=explanatory, cont=cont, p=p, na.include=na.include,
							column=column, total_col=total_col, orderbytotal=orderbytotal, glm.id=glm.id,
							na.to.missing = na.to.missing)

	d.len = length(levels(df[,names(df) %in% dependent]))
	if (d.len == 2){
		do.call(summary.factorlist2, args)
	} else if (d.len == 3){
		do.call(summary.factorlist3, args)
	} else if (d.len == 4){
		do.call(summary.factorlist4, args)
	} else if (d.len == 5){
		do.call(summary.factorlist5, args)
	}
}
