summary.factorlist <- function(df, dependent, explanatory, p=FALSE, na.include=FALSE,
															 column=FALSE, total_col=FALSE, orderbytotal=FALSE, glm.id=FALSE){
	try(if(is.data.frame(df)==FALSE) stop("df is not dataframe"))
	try(if(is.null(dependent)) stop("No dependent variable(s) provided"))
	try(if(is.null(explanatory)) stop("No explanatory variable(s) provided"))

	args = list(df, dependent, explanatory, p=FALSE, na.include=FALSE,
							column=FALSE, total_col=FALSE, orderbytotal=FALSE, glm.id=FALSE)

	if (length(levels(df[,names(df) %in% dependent])) == 2){
		do.call(summary.factorlist2, args)
	}
	if (length(levels(df[,names(df) %in% dependent])) == 3){
		do.call(summary.factorlist3, args)
	}
	if (length(levels(df[,names(df) %in% dependent])) == 4){
		do.call(summary.factorlist4, args)
	}
	if (length(levels(df[,names(df) %in% dependent])) == 5){
		do.call(summary.factorlist5, args)
	}
}
