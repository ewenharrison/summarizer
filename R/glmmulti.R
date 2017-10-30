# Function to run standard multivariable logistic regression ----
# Note, takes multiple explanatory AND dependent variables
# Dependent variables are run indiviudally, and models returned as a names list (setNames)
glmmulti <- function(df.in, dependent, explanatory){
	result = list()
	for (i in 1:length(dependent)){
		result[[i]] = glm(paste(dependent[i], "~", paste(explanatory, collapse="+")), data=df.in, family="binomial")
	}
	result = setNames(result, dependent)
	class(result) = "glmlist"
	return(result)
}
