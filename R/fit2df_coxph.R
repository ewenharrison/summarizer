fit2df.coxph <- function(fit, condense=FALSE, X=X){
	require(plyr)
	df.out <- ldply(fit, function(x) {
		conf.int = summary(x)$conf.int
		explanatory = row.names(conf.int)
		hr = conf.int[,1]
		L95 = conf.int[,3]
		U95 = conf.int[,4]
		p = summary(x)$coefficients[row.names(conf.int),
																max(dim(summary(x)$coefficients)[2])] # Hack to get p fe and re
		df = data.frame(
			'explanatory' = explanatory,
			'HR' = hr,
			'L95' = L95,
			'U95' = U95,
			'p' = p)
		return(df)
	})
	if (condense==TRUE){
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"HR" = paste0(sprintf("%.2f", df.out$HR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95),
										", p=", sprintf("%.3f", df.out$p), ")"))
	}
	return(df.out)
}
