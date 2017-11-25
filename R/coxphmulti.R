# Survival
coxphmulti <- function(df.in, dependent, explanatory){
	require(survival)
	result = list()
	for (i in 1:length(dependent)){
		result[[i]] = coxph(as.formula(paste0(dependent, "~", paste(explanatory, collapse="+"))), data=df.in)
	}
	result = setNames(result, dependent)
	class(result) = "coxphlist"
	return(result)
}
