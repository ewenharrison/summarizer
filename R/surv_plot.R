surv.plot = function(df.in, dependent, explanatory, ...){
	args = list(...)
	args$fit = survival::survfit(
		as.formula(paste0(dependent, "~", paste(explanatory, collapse="+"))), data=df.in)
	args$data=df.in

	# Defaults which can be modified via ...
	if (is.null(args$ylab)) args$ylab="Probability"
	if (is.null(args$conf.int)) args$conf.int=FALSE
	if (is.null(args$risk.table)) args$risk.table=TRUE
	if (is.null(args$linetype)) args$linetype="strata"
	if (is.null(args$palette)) args$palette="Set1"
	if (is.null(args$legend.title)) args$legend.title=""
	if (is.null(args$font.x)) args$font.x=14
	if (is.null(args$font.y)) args$font.y=14
	if (is.null(args$ggtheme)) args$ggtheme=theme_classic()

	ggsurv = do.call(
		survminer::ggsurvplot, args
	)
	ggsurv$table = ggsurv$table + theme_cleantable()
	return(ggsurv)
}
