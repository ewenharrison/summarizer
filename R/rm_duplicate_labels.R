# Remove duplicate levels from summary.factorlist ----
rm_duplicate_labels = function(summary.factorlist.output, na.to.missing = FALSE){
	x = summary.factorlist.output
	duplicate_rows = duplicated(x$label)
	x$label = as.character(x$label)
	x$label[duplicate_rows] = ""
	x$pvalue[duplicate_rows] = ""
	if (na.to.missing == TRUE){
		x$levels = as.character(x$levels)
		x$levels[which(x$levels == "NA")] = "Missing"
	}
	return(x)
}
