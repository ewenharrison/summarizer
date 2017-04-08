# Wrapper for glmmulti.boot which extracts CI from bootstrap. ---------------------------------------------
fit2df.glmboot = function(boot_results, condense=FALSE){
	df.out = data.frame(OR=exp(boot_results$t0))
	R = dim(boot_results$t)[1]
	for (i in 1:dim(df.out)[1]){
		df.out$L95[i] = exp(sort(boot_results$t[,i]))[floor(R*0.025)]
		df.out$U95[i] = exp(sort(boot_results$t[,i]))[floor((R*0.975)+1)]
		df.out$p[i] = ifelse(boot_results$t0[i] >= 0, mean(boot_results$t[,i]<0)*2, mean(boot_results$t[,i]>0)*2)
	}
	if (condense==TRUE){
		df.out = data.frame(
			"explanatory" = row.names(df.out),
			"OR" = paste0(sprintf("%.2f", df.out$OR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95), ", p=", sprintf("%.3f", df.out$p), ")"))
	}
	return(df.out)
}
