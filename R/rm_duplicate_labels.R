# Remove duplicate levels from summary.factorlist ----
rm_duplicate_labels = function(factorlist, na.to.missing = FALSE){
	x = factorlist
	duplicate_rows = duplicated(x$label)
	x$label = as.character(x$label)
	x$label[duplicate_rows] = ""
	if (any(names(x) %in% "pvalue")){
		x$pvalue[duplicate_rows] = ""
		x$pvalue[x$pvalue == "0.000"] = "<0.001"
	}
	if (na.to.missing == TRUE){
		x$levels = as.character(x$levels)
		x$levels[which(x$levels == "NA")] = "Missing"
	}
	return(x)
}
