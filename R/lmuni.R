# Function multiple univariable linear regressions against a list of explanatory variables -----------
lmuni <- function(df.in, dependent, explanatory){
	result <- list()
	for (i in 1:length(explanatory)){
		result[[i]] <- lm(paste(dependent, "~", explanatory[i]), data=df.in)
	}
	class(result) = "lmlist"
	return(result)
}
