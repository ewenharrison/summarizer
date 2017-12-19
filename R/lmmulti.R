# Function to run standard multivariable linear regression ----
# Note, takes multiple explanatory AND dependent variables
# Dependent variables are run indiviudally, and models returned as a names list (setNames)
lmmulti <- function(df.in, dependent, explanatory){
	result = list()
	for (i in 1:length(dependent)){
		result[[i]] = lm(paste(dependent[i], "~", paste(explanatory, collapse="+")), data=df.in)
	}
	result = setNames(result, dependent)
	class(result) = "lmlist"
	return(result)
}
