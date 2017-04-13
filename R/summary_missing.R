# Missing data function using mice package. Rows and columns enumerate missing data
summary.missing = function(df, dependent, explanatory){
	require(mice)
	keep = names(df) %in% c(dependent, explanatory)
	d = df[keep]
	md.pattern(d)
}
