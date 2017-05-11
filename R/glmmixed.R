# Function to run multilevel logistic regression ----
glmmixed <- function(df.in, dependent, explanatory, random_effect){
	lme4::glmer(paste0(dependent, "~", paste(explanatory, collapse="+"), " + (1|", random_effect, ")"),
				data=df.in, family="binomial", control=glmerControl(optimizer="bobyqa",
																														optCtrl=list(maxfun=200000)))
}
