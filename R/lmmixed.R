# Function to run multilevel linear regression ----
lmmixed <- function(df.in, dependent, explanatory, random_effect){
	lme4::lmer(paste0(dependent, "~", paste(explanatory, collapse="+"), " + (1|", random_effect, ")"),
				data=df.in)
}
