summarizer.merge = function(factorlist, glm.df, ref.symbol = "-"){
	if(is.null(factorlist$glm.id)) stop("Include glm.id=TRUE in summary.factorlist()")
	or_col = grep("Coefficient|OR|HR", names(glm.df), value=TRUE)
	df.out = merge(factorlist, glm.df, by.x = "glm.id", by.y = "explanatory", all = TRUE)
	df.out[,or_col] = as.character(df.out[,or_col])
	df.out[is.na(df.out[,or_col]),or_col] = ref.symbol
	df.out = df.out[order(df.out$index),]
	return(df.out)
}
