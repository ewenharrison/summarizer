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
	ggsurv$table = ggsurv$table + survminer::theme_cleantable()
	return(ggsurv)
}


# ggsurv <- ggsurvplot(fit3, data = colon,
# 										 fun = "cumhaz", conf.int = TRUE,
# 										 risk.table = TRUE, risk.table.col="strata",
# 										 ggtheme = theme_bw())
# # Faceting survival curves
# curv_facet <- ggsurv$plot + facet_grid(rx ~ adhere)
# curv_facet
# # Faceting risk tables:
# # Generate risk table for each facet plot item
# ggsurv$table + facet_grid(rx ~ adhere, scales = "free")+
# 	theme(legend.position = "none")
# # Generate risk table for each facet columns
# tbl_facet <- ggsurv$table + facet_grid(.~ adhere, scales = "free")
# tbl_facet + theme(legend.position = "none")
# # Arrange faceted survival curves and risk tables
# g2 <- ggplotGrob(curv_facet)
# g3 <- ggplotGrob(tbl_facet)
# min_ncol <- min(ncol(g2), ncol(g3))
# g <- gridExtra::rbind.gtable(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
# g$widths <- grid::unit.pmax(g2$widths, g3$widths)
# grid::grid.newpage()
# grid::grid.draw(g)
