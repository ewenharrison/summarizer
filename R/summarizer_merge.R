summarizer.merge = function(summary.out, glm.out, ref.symbol = "-"){
	df.out = merge(summary.out, glm.out, by.x = "glm.id", by.y = "explanatory", all.x = TRUE)
	df.out[, apply(df.out, 2, anyNA)] = as.character(df.out[, apply(df.out, 2, anyNA)]) # Change OR column with missings to character
	df.out[is.na(df.out)] = ref.symbol
	df.out = df.out[order(df.out$index),]
	return(df.out)
}
