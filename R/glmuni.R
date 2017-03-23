# Function multiple univariable logistic regressions against a list of explanatory variables -----------
glmuni <- function(df.in, dependent, explanatory){
	result <- list()
	for (i in 1:length(explanatory)){
		result[[i]] <- glm(paste(dependent, "~", explanatory[i]), data=df.in, family="binomial")
	}
	return(result)
}
