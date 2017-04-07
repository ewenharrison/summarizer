summarizer_merge = function(summary.out, glm.out){
	df.out = merge(summary.out, glm.out, by.x = "glm.id", by.y = "explanatory", all.x = TRUE)
	df.out$OR = as.character(df.out$OR)
	df.out$OR[is.na(df.out$OR)] = "-"
	df.out = df.out[order(df.out$index),]
	return(df.out)
}
