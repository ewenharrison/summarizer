# Missing data function using mice package. Rows and columns enumerate missing data
summary.missing = function(df, dependent, explanatory){
	keep = names(df) %in% c(dependent, explanatory)
	d = df[keep]
	mice::md.pattern(d)
}
