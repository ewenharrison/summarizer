summarizer.merge = function(factorlist, glm.df, ref.symbol = "-"){
	df.out = merge(factorlist, glm.df, by.x = "glm.id", by.y = "explanatory", all.x = TRUE)
	or_col = grepl("OR|HR", names(df.out))
	df.out[,or_col] = as.character(df.out[,or_col])
	df.out[is.na(df.out[,or_col]),or_col] = ref.symbol
	df.out = df.out[order(df.out$index),]
	return(df.out)
}
