# Survival
coxphuni <- function(df.in, dependent, explanatory){
	require(survival)
	result <- list()
	for (i in 1:length(explanatory)){
		result[[i]] <- coxph(as.formula(paste0(dependent, "~", explanatory[i])), data=df.in)
	}
	class(result) = "coxphlist"
	return(result)
}
