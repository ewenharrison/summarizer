fit2df.coxphlist <- function(fit, condense=TRUE, estimate.suffix=""){
	x = fit
	df.out <- plyr::ldply(x, function(x) {
		conf.int = summary(x)$conf.int
		explanatory = row.names(conf.int)
		hr = conf.int[,1]
		L95 = conf.int[,3]
		U95 = conf.int[,4]
		p = summary(x)$coefficients[row.names(conf.int),
																max(dim(summary(x)$coefficients)[2])] # Hack to get p fe and re
		df.out = data.frame(explanatory, hr, L95, U95, p)
		colnames(df.out) = c("explanatory", paste0("HR", estimate.suffix), "L95", "U95", "p")
		return(df.out)
	})
	if (condense==TRUE){
		p = paste0("=", sprintf("%.3f", df.out$p))
		p[p == "=0.000"] = "<0.001"

		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"HR" = paste0(sprintf("%.2f", df.out$HR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95),
										", p", p, ")"))
		colnames(df.out) = c("explanatory", paste0("HR", estimate.suffix))
	}
	return(df.out)
}
