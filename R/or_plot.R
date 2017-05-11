or.plot = function(df, dependent, explanatory, factorlist=NULL, glmfit=NULL, column_space=c(-0.5, 0, 0.5), X=X){
	require(ggplot2)
	require(scales)
	# Generate or format factorlist object
	if(is.null(factorlist)){
		factorlist = summary.factorlist(df, dependent, explanatory, total=TRUE, glm.id=TRUE)
	}

	# Generate or format glm
	if(is.null(glmfit)){
		glmfit = glmmulti(df, dependent, explanatory)
	}
	df_fit_c = fit2df(glmfit, condense = TRUE, X=X)
	df_fit = fit2df(glmfit, condense = FALSE, X=X)

	# Merge
	df.out = summarizer.merge(factorlist, df_fit_c)
	names(df.out)[which(names(df.out) %in% "OR")] = "OR (multivariate)"
	df.out = summarizer.merge(df.out, df_fit, ref.symbol = "1.0")

	# Fill in total for continuous variables (NA by default)
	df.out$Total[is.na(df.out$Total)] = dim(df)[1]

	# Fix order
	df.out$levels = as.character(df.out$levels)
	df.out$glm.id = factor(df.out$glm.id, levels = df.out$glm.id[order(-df.out$index)])

	# Plot
	g1 = ggplot(df.out, aes(x = as.numeric(OR), xmin = as.numeric(L95), xmax  = as.numeric(U95),
													y = glm.id))+
		geom_point(aes(size = Total), shape=22, fill="darkblue")+
		geom_errorbarh(height=0.2) +
		geom_vline(xintercept = 1, linetype = "longdash", colour = "black")+
		scale_x_continuous(name="Odds ratio (95% CI, log scale)", trans="log10", breaks= pretty_breaks())+
		theme_classic(14)+
		theme(axis.title.x = element_text(),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.line.y = element_blank(),
					axis.ticks.y = element_blank(),
					legend.position="none")

	t1 = ggplot(df.out, aes(x = as.numeric(OR), y = glm.id))+
		annotate("text", x = column_space[1], y =  df.out$glm.id, label=df.out[,2], hjust=0, size=5)+
		annotate("text", x = column_space[2], y =  df.out$glm.id, label=df.out[,3], hjust=1, size=5)+
		annotate("text", x = column_space[3], y =  df.out$glm.id, label=df.out[,8], hjust=1, size=5)+
		theme_classic(14)+
		theme(axis.title.x = element_text(colour = "white"),
					axis.text.x = element_text(colour = "white"),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					line = element_blank())

	dependent_label =attr(df[,which(names(df) %in% dependent)], "label")

	if (is.null(dependent_label)){
		title = paste0(dependent, ": ", "(OR, 95% CI, p-value)")
	} else {
		title = paste0(dependent_label, ": ", "(OR, 95% CI, p-value)")
	}

	gridExtra::grid.arrange(t1, g1, ncol=2, widths = c(3,2),
							 top=grid::textGrob(title, x=0.02, y=0.2, gp=gpar(fontsize=18), just="left"))
}
