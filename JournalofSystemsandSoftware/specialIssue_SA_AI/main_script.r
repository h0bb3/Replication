rm(list=ls(all=TRUE))
library(coin)
library(caTools)
#install.packages("export")
library(export)

# please adjust this to point to the same directory as the main_script.r
setwd("")

loadData <- function(a_csvFile, algorithm) {
	data <- read.csv(a_csvFile, head=TRUE, sep="\t")

	data <- data[data$initialClustered < data$totalMapped & data$algorithm == algorithm, ]

	data$precision = ifelse(data$totalAutoClustered == 0, 0, (data$totalAutoClustered - data$totalAutoWrong) / (data$totalAutoClustered))
	data$recall = ifelse(data$totalMapped - data$initialClustered - data$totalManuallyClustered == 0, 0, (data$totalAutoClustered - data$totalAutoWrong) / (data$totalMapped - data$initialClustered - data$totalManuallyClustered))
	
	# harmonic means for the performance metrics
	#data$h_mam = ifelse(data$ap == 0 | data$mp == 0 | data$mr == 0, 0, 3*data$ap*data$mr*data$mp/(data$ap*data$mr + data$ap*data$mp + data$mp*data$mr))
	data$f1 = ifelse(data$precision == 0 | data$recall == 0, 0, 2*data$precision*data$recall/(data$precision+data$recall))
	return(data)
}

getOmegaPhi <- function(a_best_percent, a_data) {
	data_filtered = a_data
	data_best = data_filtered[data_filtered$f1 >= quantile(data_filtered$f1, c(1.0 - a_best_percent), names=FALSE),]
	message(paste('data_best rows=',nrow(data_best)))
	out = list()
	out$omega_min <- min(data_best$omega)
	out$omega_max <- max(data_best$omega)
	out$phi_min <- min(data_best$phi)
	out$phi_max <- max(data_best$phi)
	return(out)
}

getFilteredData <- function(a_best_percent, a_data) {
	omegaphi = getOmegaPhi(a_best_percent, a_data)
	out = list()
	out$omega_min <- omegaphi$omega_min
	out$omega_max <- omegaphi$omega_max
	out$phi_min <- omegaphi$phi_min
	out$phi_max <- omegaphi$phi_max

	out$data <- a_data[a_data$phi >= out$phi_min & a_data$phi <= out$phi_max & a_data$omega >= out$omega_min & a_data$omega <= out$omega_max,]

	return(out)
}

getFilteredDataFixedPhi <- function(a_best_percent, a_data, a_phiMin, a_phiMax) {
	omegaphi = getOmegaPhi(a_best_percent, a_data)
	out = list()
	out$omega_min <- omegaphi$omega_min
	out$omega_max <- omegaphi$omega_max
	out$phi_min <- a_phiMin
	out$phi_max <- a_phiMax

	out$data <- a_data[a_data$phi >= out$phi_min & a_data$phi <= out$phi_max & a_data$omega >= out$omega_min & a_data$omega <= out$omega_max,]

	return(out)
}

getFilteredDataFixedPhiFixedOmega <- function(a_data, a_phiMin, a_phiMax, a_omegaMin, a_omegaMax) {
	out = list()
	out$omega_min <- a_omegaMin
	out$omega_max <- a_omegaMax
	out$phi_min <- a_phiMin
	out$phi_max <- a_phiMax

	out$data <- a_data[a_data$phi >= out$phi_min & a_data$phi <= out$phi_max & a_data$omega >= out$omega_min & a_data$omega <= out$omega_max,]

	return(out)
}



getFilteredDataEx3 <- function(a_fraction, a_data) {
      delta = 0.05
	limit = 0

	out = list()

	# filter every 10% mappingPercent using the best_percent omega and phi values
	limit = delta
	data <- a_data[a_data$mappingPercent < limit,]
	filtered <- data[data$f1 >= quantile(data$f1, c(1.0 - a_fraction), names=FALSE),]
	out$data <- filtered
	out$fraction <- a_fraction

      for (i in 1:19) {


	  data <- a_data[a_data$mappingPercent >= limit & a_data$mappingPercent < limit + delta,]
	  filtered <- data[data$f1 >= quantile(data$f1, c(1.0 - a_fraction), names=FALSE),]

	  message(paste('limit=',limit))
	  message(paste('rows=',nrow(filtered)))
	  #message(paste('omega_min=',filtered$omega_min))
	  #message(paste('omega_max=',filtered$omega_max))
	  #message(paste('phi_min=',filtered$phi_min))
	  #message(paste('phi_max=',filtered$phi_max))

	  out$data <- rbind(out$data, filtered)
        limit = limit + delta
      }

	return(out)
}



plotRunningMedian <- function(a_data, a_color, a_column, a_lineStyle) {
	data <- data.frame(a_data[[a_column]], a_data$mappingPercent)
	names(data)[1] = "thedata"
	names(data)[2] = "mappingPercent"
	data <- data[order(data$mappingPercent),]

	# this produces median
	y_lag <- runquantile(data$thedata, 1001, probs=c(0.5), align="center")

	lines(data$mappingPercent, y_lag, col=a_color, lty=a_lineStyle)
}

plotRunningQuantiles <- function(a_data, a_color, a_column) {
	
	data <- data.frame(a_data[[a_column]], a_data$mappingPercent)
	names(data)[1] = "thedata"
	names(data)[2] = "mappingPercent"
	data <- data[order(data$mappingPercent),]

	y = runquantile(data$thedata, 1001, probs=c(0.25, 0.75), align="center")

	#lines(data$mappingPercent, y[,1], col=a_color)
	#lines(data$mappingPercent, y[,2], col=a_color)

	polygon(c(data$mappingPercent, rev(data$mappingPercent)),c(y[,1], rev(y[,2])), col=a_color, border=NA)
}

plotRunningData <- function(a_data1, a_data2, a_data3, a_data4, a_title, a_column, a_legendPos) {
	# Draw an empty plot
	plot(5, 5, type="n", yaxt="n", xaxt="n", ann=FALSE, xlim=c(0, 1.0), ylim = c(0,1))

	# add the x axis
	axis(side = 1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

	# Add a title
	title(a_title)

	# colors from: http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/
	c1 <- rgb(57, 106, 177, max = 255, alpha = 255)
	c1_a <- rgb(114, 147, 203, max = 255, alpha = 80)

	c2 <- rgb(218, 124, 48, max = 255, alpha = 255)
	c2_a <- rgb(225, 151, 76, max = 255, alpha = 80)

	c3 <- rgb(62, 150, 81, max = 255, alpha = 255)
	c3_a <- rgb(132, 186, 91, max = 255, alpha = 80)

	c4 <- rgb(204, 37, 41, max = 255, alpha = 255)
	c4_a <- rgb(211, 94, 96, max = 255, alpha = 80)

	plotRunningQuantiles(a_data1, c1_a, a_column)
	plotRunningQuantiles(a_data2, c2_a, a_column)
	plotRunningQuantiles(a_data3, c3_a, a_column)
	plotRunningQuantiles(a_data4, c4_a, a_column)


	plotRunningMedian(a_data1, c1, a_column, "solid")
	plotRunningMedian(a_data2, c2, a_column, "solid")
	plotRunningMedian(a_data3, c3, a_column, "solid")
	plotRunningMedian(a_data4, c4, a_column, "solid")


	if (a_legendPos != "none") {
		legend(a_legendPos, legend=c("NB", "CA", "IR", "LSI"),col=c(c1, c2, c3, c4), lty=1)
	}
}

plotRunningFilterData <- function(a_new, a_old, a_title, a_column, a_legendPos) {
	# Draw an empty plot
	plot(5, 5, type="n", yaxt="n", xaxt="n", ann=FALSE, xlim=c(0, 1.0), ylim = c(0,1))

	# add the x axis
	axis(side = 1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

	# Add a title
	title(a_title)

	# colors from: http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/
	c1 <- rgb(57, 106, 177, max = 255, alpha = 255)
	c1_a <- rgb(114, 147, 203, max = 255, alpha = 80)

	c2 <- rgb(218, 124, 48, max = 255, alpha = 255)
	c2_a <- rgb(225, 151, 76, max = 255, alpha = 80)

	plotRunningQuantiles(a_new, c1_a, a_column)
	plotRunningQuantiles(a_old, c2_a, a_column)


	plotRunningMedian(a_new, c1, a_column, "solid")
	plotRunningMedian(a_old, c2, a_column, "solid")

	legend(a_legendPos, legend=c("New Filter", "Old Filter"),col=c(c1, c2), lty=1)
}


plotCDAComparison <- function(a_nb, a_ir, a_lsi, a_title, a_column, a_legendPos) {
	# Draw an empty plot
	plot(5, 5, type="n", yaxt="n", xaxt="n", ann=FALSE, xlim=c(0, 1.0), ylim = c(0,1))

	# add the x axis
	axis(side = 1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

	# Add a title
	title(a_title)

	black <- rgb(0, 0, 0, max = 255, alpha = 255)


	# colors from: http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/
	c1 <- rgb(57, 106, 177, max = 255, alpha = 255)
	c1_a <- rgb(114, 147, 203, max = 255, alpha = 80)

	c2 <- rgb(218, 124, 48, max = 255, alpha = 255)
	c2_a <- rgb(225, 151, 76, max = 255, alpha = 80)

	c3 <- rgb(62, 150, 81, max = 255, alpha = 255)
	c3_a <- rgb(132, 186, 91, max = 255, alpha = 80)

	c4 <- rgb(204, 37, 41, max = 255, alpha = 255)
	c4_a <- rgb(211, 94, 96, max = 255, alpha = 80)

	plotRunningQuantiles(a_nb[a_nb$cda == "Y",], c1_a, a_column)
	plotRunningQuantiles(a_nb[a_nb$cda == "N",], c1_a, a_column)

	plotRunningQuantiles(a_ir[a_ir$cda == "Y",], c3_a, a_column)
	plotRunningQuantiles(a_ir[a_ir$cda == "N",], c3_a, a_column)

	plotRunningQuantiles(a_lsi[a_lsi$cda == "Y",], c4_a, a_column)
	plotRunningQuantiles(a_lsi[a_lsi$cda == "N",], c4_a, a_column)


	plotRunningMedian(a_nb[a_nb$cda == "Y",], c1, a_column, "solid")
	plotRunningMedian(a_nb[a_nb$cda == "N",], c1, a_column, "dotted")

	plotRunningMedian(a_ir[a_ir$cda == "Y",], c3, a_column, "solid")
	plotRunningMedian(a_ir[a_ir$cda == "N",], c3, a_column, "dotted")

	plotRunningMedian(a_lsi[a_lsi$cda == "Y",], c4, a_column, "solid")
	plotRunningMedian(a_lsi[a_lsi$cda == "N",], c4, a_column, "dotted")

	legend(a_legendPos, legend=c("NB", "IR", "LSI", "CDA", "NO CDA"),col=c(c1, c3, c4, black, black), lty=c(1, 1, 1, 1, 2))
}


plotBoxPlotsAllData <- function(a_column, a_title) {
	boxplot(
		nb_ant_best[[a_column]], hugme_ant$data[[a_column]], ir_ant_best[[a_column]], lsi_ant_best[[a_column]],
		nb_argouml_best[[a_column]], hugme_argouml$data[[a_column]], ir_argouml_best[[a_column]], lsi_argouml_best[[a_column]],

		nb_apache_best[[a_column]], hugme_apache$data[[a_column]], ir_apache_best[[a_column]], lsi_apache_best[[a_column]],
		nb_jabref_best[[a_column]], hugme_jabref$data[[a_column]], ir_jabref_best[[a_column]], lsi_jabref_best[[a_column]],
		nb_lucene_best[[a_column]], hugme_lucene$data[[a_column]], ir_lucene_best[[a_column]], lsi_lucene_best[[a_column]],
		nb_prom_best[[a_column]], hugme_prom$data[[a_column]], ir_prom_best[[a_column]], lsi_prom_best[[a_column]],
		nb_sweethome3d_best[[a_column]], hugme_sweethome3d$data[[a_column]], ir_sweethome3d_best[[a_column]], lsi_sweethome3d_best[[a_column]],
		nb_teammates_best[[a_column]], hugme_teammates$data[[a_column]], ir_teammates_best[[a_column]], lsi_teammates_best[[a_column]],


		main = a_title,
		at = rev( c(1, 2, 3, 4,  
				6, 7, 8, 9,
				11, 12, 13, 14,
				16, 17, 18, 19, 
				21, 22, 23, 24,
				26, 27, 28, 29,
				31, 32, 33, 34,
                        36, 37, 38, 39)),

		#names = c("Ant", "", "", "", "A.UML", "", "", "", "JabRef", "", "", "", "Lucene", "", "", "", "ProM", "", "", "", "S.H.3D", "", "", "", "T.Mates", "", "", ""),
		las = 2,
		col = c(rgb(114, 147, 203, max = 255),rgb(225, 151, 76, max = 255), rgb(132, 186, 91, max = 255), rgb(204, 37, 41, max = 255, alpha = 255)),
		border = "black",
		horizontal = TRUE,
		notch = FALSE,
		outpch = 46		# outlier ASCII for .
	)
	axis(2, seq(4, 43, 5), c("T.Mates", "S H 3D", "ProM", "Lucene", "JabRef", "C Img", "A.UML", "Ant"), las=2)

}


plotBoxPlotsCDA <- function(a_column, a_title) {

	nb_jabref_y <- nb_jabref[nb_jabref$cda == "Y",]
	nb_jabref_n <- nb_jabref[nb_jabref$cda == "N",]
	ir_jabref_y <- ir_jabref[ir_jabref$cda == "Y",]
	ir_jabref_n <- ir_jabref[ir_jabref$cda == "N",]
	lsi_jabref_y <- lsi_jabref[lsi_jabref$cda == "Y",]
	lsi_jabref_n <- lsi_jabref[lsi_jabref$cda == "N",]

	nb_ant_y <- nb_ant[nb_ant$cda == "Y",]
	nb_ant_n <- nb_ant[nb_ant$cda == "N",]
	ir_ant_y <- ir_ant[ir_ant$cda == "Y",]
	ir_ant_n <- ir_ant[ir_ant$cda == "N",]
	lsi_ant_y <- lsi_ant[lsi_ant$cda == "Y",]
	lsi_ant_n <- lsi_ant[lsi_ant$cda == "N",]

	nb_apache_y <- nb_apache[nb_apache$cda == "Y",]
	nb_apache_n <- nb_apache[nb_apache$cda == "N",]
	ir_apache_y <- ir_apache[ir_apache$cda == "Y",]
	ir_apache_n <- ir_apache[ir_apache$cda == "N",]
	lsi_apache_y <- lsi_apache[lsi_apache$cda == "Y",]
	lsi_apache_n <- lsi_apache[lsi_apache$cda == "N",]

	nb_argouml_y <- nb_argouml[nb_argouml$cda == "Y",]
	nb_argouml_n <- nb_argouml[nb_argouml$cda == "N",]
	ir_argouml_y <- ir_argouml[ir_argouml$cda == "Y",]
	ir_argouml_n <- ir_argouml[ir_argouml$cda == "N",]
	lsi_argouml_y <- lsi_argouml[lsi_argouml$cda == "Y",]
	lsi_argouml_n <- lsi_argouml[lsi_argouml$cda == "N",]

	nb_lucene_y <- nb_lucene[nb_lucene$cda == "Y",]
	nb_lucene_n <- nb_lucene[nb_lucene$cda == "N",]
	ir_lucene_y <- ir_lucene[ir_lucene$cda == "Y",]
	ir_lucene_n <- ir_lucene[ir_lucene$cda == "N",]
	lsi_lucene_y <- lsi_lucene[lsi_lucene$cda == "Y",]
	lsi_lucene_n <- lsi_lucene[lsi_lucene$cda == "N",]

	nb_teammates_y <- nb_teammates[nb_teammates$cda == "Y",]
	nb_teammates_n <- nb_teammates[nb_teammates$cda == "N",]
	ir_teammates_y <- ir_teammates[ir_teammates$cda == "Y",]
	ir_teammates_n <- ir_teammates[ir_teammates$cda == "N",]
	lsi_teammates_y <- lsi_teammates[lsi_teammates$cda == "Y",]
	lsi_teammates_n <- lsi_teammates[lsi_teammates$cda == "N",]

	nb_sweethome3d_y <- nb_sweethome3d[nb_sweethome3d$cda == "Y",]
	nb_sweethome3d_n <- nb_sweethome3d[nb_sweethome3d$cda == "N",]
	ir_sweethome3d_y <- ir_sweethome3d[ir_sweethome3d$cda == "Y",]
	ir_sweethome3d_n <- ir_sweethome3d[ir_sweethome3d$cda == "N",]
	lsi_sweethome3d_y <- lsi_sweethome3d[lsi_sweethome3d$cda == "Y",]
	lsi_sweethome3d_n <- lsi_sweethome3d[lsi_sweethome3d$cda == "N",]

	nb_prom_y <- nb_prom[nb_prom$cda == "Y",]
	nb_prom_n <- nb_prom[nb_prom$cda == "N",]
	ir_prom_y <- ir_prom[ir_prom$cda == "Y",]
	ir_prom_n <- ir_prom[ir_prom$cda == "N",]
	lsi_prom_y <- lsi_prom[lsi_prom$cda == "Y",]
	lsi_prom_n <- lsi_prom[lsi_prom$cda == "N",]

	boxplot(
		nb_ant_y[[a_column]], nb_ant_n[[a_column]], ir_ant_y[[a_column]], ir_ant_n[[a_column]], lsi_ant_y[[a_column]], lsi_ant_n[[a_column]],
		nb_argouml_y[[a_column]], nb_argouml_n[[a_column]], ir_argouml_y[[a_column]], ir_argouml_n[[a_column]], lsi_argouml_y[[a_column]], lsi_argouml_n[[a_column]],
		nb_apache_y[[a_column]], nb_apache_n[[a_column]], ir_apache_y[[a_column]], ir_apache_n[[a_column]], lsi_apache_y[[a_column]], lsi_apache_n[[a_column]],
		nb_jabref_y[[a_column]], nb_jabref_n[[a_column]], ir_jabref_y[[a_column]], ir_jabref_n[[a_column]], lsi_jabref_y[[a_column]], lsi_jabref_n[[a_column]],
		nb_lucene_y[[a_column]], nb_lucene_n[[a_column]], ir_lucene_y[[a_column]], ir_lucene_n[[a_column]], lsi_lucene_y[[a_column]], lsi_lucene_n[[a_column]],
		nb_prom_y[[a_column]], nb_prom_n[[a_column]], ir_prom_y[[a_column]], ir_prom_n[[a_column]], lsi_prom_y[[a_column]], lsi_prom_n[[a_column]],		
		nb_sweethome3d_y[[a_column]], nb_sweethome3d_n[[a_column]], ir_sweethome3d_y[[a_column]], ir_sweethome3d_n[[a_column]], lsi_sweethome3d_y[[a_column]], lsi_sweethome3d_n[[a_column]],
		nb_teammates_y[[a_column]], nb_teammates_n[[a_column]], ir_teammates_y[[a_column]], ir_teammates_n[[a_column]], lsi_teammates_y[[a_column]], lsi_teammates_n[[a_column]],
		

		main = a_title,
		at = rev( c(1, 2, 3, 4, 5, 6, 
				8, 9, 10, 11, 12, 13,
				15, 16, 17, 18, 19, 20,
				22, 23, 24, 25, 26, 27,
				29, 30, 31, 32, 33, 34,
				36, 37, 38, 39, 40, 41,
				43, 44, 45, 46, 47, 48,
                        50, 51, 52, 53, 54, 55)),
		#names = c("Ant", "a", "a", "a", "a", "a", "C Img.", "c", "c", "c", "c", "c", "A.UML", "", "", "", "", "", "JabRef", "", "", "", "", "", "Lucene", "", "", "", "", "", "ProM", "", "", "", "", "", "S H 3D", "", "", "", "", "", "T.Mates", "", "", "", "", ""),
		las = 2,
		col = c(rgb(114, 147, 203, max = 255), rgb(114, 147, 203, max = 255), rgb(132, 186, 91, max = 255), rgb(132, 186, 91, max = 255), rgb(204, 37, 41, max = 255, alpha = 255), rgb(204, 37, 41, max = 255, alpha = 255)),
		border = "black",
		horizontal = TRUE,
		notch = FALSE,
		outpch = 46,	# outlier ASCII for .
		xlab = "F1 Score"
	)
	axis(2, seq(6,56, 7), c("T.Mates", "S H 3D", "ProM", "Lucene", "JabRef", "C Img", "A.UML", "Ant"), las=2)
}




mergeColumn <- function(a_target, a_data, a_dataCol, a_group, a_groupCol) {
	if (length(a_target[[a_dataCol]]) == 0) {
		d = c(a_data[[a_dataCol]]);
		group = rep(a_group, length(a_data[[a_dataCol]]))
	} else {
		d = append(a_target[[a_dataCol]], a_data[[a_dataCol]])
		group = append(a_target[[a_groupCol]], rep(a_group, length(a_data[[a_dataCol]])))
	}
	ret = data.frame(d, group)
	names(ret)[1] = a_dataCol
	names(ret)[2] = a_groupCol
	
	ret[[a_groupCol]] = as.factor(ret[[a_groupCol]])
	ret
}

wilcoxTest <- function(a_dataSet1, a_dataSet2, a_column) {
	td = data.frame()
	td = mergeColumn(td, a_dataSet1, a_column, "d1", "group")
	td = mergeColumn(td, a_dataSet2, a_column, "d2", "group")

	ret = list()
	ret$p = pvalue(wilcox_test(td[[a_column]]~td$group))
	ret$Z = statistic(wilcox_test(td[[a_column]]~td$group))
	ret$r = abs(ret$Z) / sqrt(nrow(td))
	ret$median1 = median(a_dataSet1[[a_column]])
	ret$median2 = median(a_dataSet2[[a_column]])
	ret$medianDiff = ret$median1 - ret$median2
	ret$nrow1 = nrow(a_dataSet1)
	ret$nrow2 = nrow(a_dataSet2)

	# output for copy convenient copy paste
	message(cat(ret$nrow1, "/", ret$nrow2))
	message(ret$p)
	message(ret$r)
	message(ret$Z)
	message(ret$medianDiff)

	ret
}


cdaTest <- function(a_systemName, a_nbData, a_irData, a_lsiData) {
	nb <- wilcoxTest(a_nbData[a_nbData$cda == "Y",], a_nbData[a_nbData$cda == "N", ], "f1")
	nb$func = "nb"
	nb$system = a_systemName

	ir <- wilcoxTest(a_irData[a_irData$cda == "Y",], a_irData[a_irData$cda == "N", ], "f1")
	ir$func = "ir"
	ir$system = a_systemName

	lsi <- wilcoxTest(a_lsiData[a_lsiData$cda == "Y",], a_lsiData[a_lsiData$cda == "N", ], "f1")
	lsi$func = "lsi"
	lsi$system = a_systemName

	rbind(nb, rbind(ir, lsi))
}

getCdaStat <- function(a_cdaTestData, a_statCol) {

	ret = list()
	ret$system = a_cdaTestData['nb',]$system
	ret$stat = a_statCol
	ret$nb = a_cdaTestData['nb', a_statCol][[1]]
	ret$ir = a_cdaTestData['ir', a_statCol][[1]]
	ret$lsi = a_cdaTestData['lsi', a_statCol][[1]]

	ret
}

getAllCdaStatsAsRows <- function(a_cdaTestData) {

	rows = list()
	rows <- getCdaStat(a_cdaTestData, "nrow1")
	rows <- rbind(rows, getCdaStat(a_cdaTestData, "nrow2"))
	rows <- rbind(rows, getCdaStat(a_cdaTestData, "r"))
	rows <- rbind(rows, getCdaStat(a_cdaTestData, "Z"))
	rows <- rbind(rows, getCdaStat(a_cdaTestData, "medianDiff"))
	rows <- rbind(rows, getCdaStat(a_cdaTestData, "p"))

	rows
}


comparisonTest <- function(a_systemName, a_nbBest, a_irBest, a_lsiBest, a_hugme) {
	nb_hugme <- wilcoxTest(a_nbBest, a_hugme, "f1")
	nb_hugme$system = a_systemName
	nb_hugme$comparison = "NB v.s. CA"

	nb_ir <- wilcoxTest(a_nbBest, a_irBest, "f1")
	nb_ir$system = a_systemName
	nb_ir$comparison = "NB v.s. IR"

	nb_lsi <- wilcoxTest(a_nbBest, a_lsiBest, "f1")
	nb_lsi$system = a_systemName
	nb_lsi$comparison = "NB v.s. LSI"

	hugme_ir <- wilcoxTest(a_hugme, a_irBest, "f1")
	hugme_ir$system = a_systemName
	hugme_ir$comparison = "CA v.s. IR"

	hugme_lsi <- wilcoxTest(a_hugme, a_lsiBest, "f1")
	hugme_lsi$system = a_systemName
	hugme_lsi$comparison = "CA v.s. LSI"

	ir_lsi <- wilcoxTest(a_irBest, a_lsiBest, "f1")
	ir_lsi$system = a_systemName
	ir_lsi$comparison = "IR v.s. LSI"

	rbind(nb_hugme, rbind(nb_ir, rbind(nb_lsi, rbind(hugme_ir, rbind(hugme_lsi, ir_lsi)))))
}

getComparisonStat <- function(a_comparisonTestData, a_statCol) {

	ret = list()
	ret$system = a_comparisonTestData['nb_hugme',]$system
	ret$stat = a_statCol
	ret$nb_hugme = a_comparisonTestData['nb_hugme', a_statCol][[1]]
	ret$nb_ir = a_comparisonTestData['nb_ir', a_statCol][[1]]
	ret$nb_lsi = a_comparisonTestData['nb_lsi', a_statCol][[1]]

	ret$hugme_ir  = a_comparisonTestData['hugme_ir', a_statCol][[1]]
	ret$hugme_lsi = a_comparisonTestData['hugme_lsi', a_statCol][[1]]

	ret$ir_lsi = a_comparisonTestData['ir_lsi', a_statCol][[1]]

	ret
}

getAllComparisonStatsAsRows <- function(a_comparisonTestData) {

	rows = list()
	rows <- getComparisonStat(a_comparisonTestData, "nrow1")
	rows <- rbind(rows, getComparisonStat(a_comparisonTestData, "nrow2"))
	rows <- rbind(rows, getComparisonStat(a_comparisonTestData, "r"))
	rows <- rbind(rows, getComparisonStat(a_comparisonTestData, "Z"))
	rows <- rbind(rows, getComparisonStat(a_comparisonTestData, "medianDiff"))
	rows <- rbind(rows, getComparisonStat(a_comparisonTestData, "p"))

	rows
}


CAttractStr = "HuGMe:CAttract"
NBAttractStr = "NaiveBayes:NBAttract"
IRAttractStr = "LSI_IR:IRAttract"
LSIAttractStr = "LSI_IR:LSIAttract"
hugme_filter_fraction = 0.5



#load ant - done
hugme_ant_loaded <- loadData("data/ant.csv", CAttractStr)
nrow(hugme_ant_loaded)
hugme_ant  <- getFilteredDataEx3(hugme_filter_fraction, hugme_ant_loaded)
nb_ant_loaded <- loadData("data/ant.csv", NBAttractStr)
nb_ant <- nb_ant_loaded
ir_ant_loaded <- loadData("data/ant.csv", IRAttractStr)

#ir_ant_loaded <- loadData("../data/ant.csv", "LSI_IR:IR")
ir_ant <- ir_ant_loaded
lsi_ant_loaded <- loadData("data/ant.csv", LSIAttractStr)
lsi_ant <- lsi_ant_loaded

antCdaTest <- getAllCdaStatsAsRows(cdaTest("Ant", nb_ant, ir_ant, lsi_ant))
antCdaTest

plotCDAComparison(nb_ant, ir_ant, lsi_ant, "Ant CDA F1 Comparison", "f1", "bottomright")

nb_ant_best <- nb_ant[nb_ant$cda == "Y",]
ir_ant_best <- ir_ant[ir_ant$cda == "Y",]
lsi_ant_best <- lsi_ant[lsi_ant$cda == "Y",]

antComparisonTest= getAllComparisonStatsAsRows(comparisonTest("Ant", nb_ant_best, ir_ant_best, lsi_ant_best, hugme_ant$data))
antComparisonTest

plotRunningData(nb_ant_best, hugme_ant$data, ir_ant_best, lsi_ant_best, "Ant F1 Comparison", "f1", "bottomright")


#load argouml - done
hugme_argouml_loaded <- loadData("data/argouml.csv", CAttractStr)
nrow(hugme_argouml_loaded)
hugme_argouml  <- getFilteredDataEx3(hugme_filter_fraction, hugme_argouml_loaded)
nb_argouml_loaded <- loadData("data/argouml.csv", NBAttractStr)
nb_argouml <- nb_argouml_loaded
ir_argouml_loaded <- loadData("data/argouml.csv", IRAttractStr)
ir_argouml <- ir_argouml_loaded
lsi_argouml_loaded <- loadData("data/argouml.csv", LSIAttractStr)
lsi_argouml <- lsi_argouml_loaded

argoumlCdaTest <- getAllCdaStatsAsRows(cdaTest("ArgoUML", nb_argouml, ir_argouml, lsi_argouml))
argoumlCdaTest

plotCDAComparison(nb_argouml, ir_argouml, lsi_argouml, "ArgoUML CDA F1 Comparison", "f1", "bottomright")

nb_argouml_best <- nb_argouml[nb_argouml$cda == "Y",]
ir_argouml_best <- ir_argouml[ir_argouml$cda == "N",]
lsi_argouml_best <- lsi_argouml[lsi_argouml$cda == "Y",]

argoumlComparisonTest = getAllComparisonStatsAsRows(comparisonTest("ArgoUML", nb_argouml_best, ir_argouml_best, lsi_argouml_best, hugme_argouml$data))
argoumlComparisonTest
plotRunningData(nb_argouml_best, hugme_argouml$data, ir_argouml_best, lsi_argouml_best, "ArgoUML F1 Comparison", "f1", "bottomright")


#load apache
hugme_apache_loaded <- loadData("data/apache.csv", CAttractStr)
nrow(hugme_apache_loaded)
hugme_apache<- getFilteredDataEx3(hugme_filter_fraction, hugme_apache_loaded)

nrow(hugme_apache$data)
summary(hugme_apache)
nb_apache_loaded <- loadData("data/apache.csv", NBAttractStr)
nb_apache<- nb_apache_loaded
ir_apache_loaded <- loadData("data/apache.csv", IRAttractStr)
ir_apache<- ir_apache_loaded
lsi_apache_loaded <- loadData("data/apache.csv", LSIAttractStr)
lsi_apache<- lsi_apache_loaded

apacheCdaTest <- getAllCdaStatsAsRows(cdaTest("Cmn.Img.", nb_apache, ir_apache, lsi_apache))
apacheCdaTest

plotCDAComparison(nb_apache, ir_apache, lsi_apache, "Cmn. Img. CDA F1 Comparison", "f1", "bottomright")

nb_apache_best <- nb_apache[nb_apache$cda == "Y",]
ir_apache_best <- ir_apache[ir_apache$cda == "Y",]
lsi_apache_best <- lsi_apache[lsi_apache$cda == "Y",]

apacheComparisonTest = getAllComparisonStatsAsRows(comparisonTest("Cmn. Img.", nb_apache_best, ir_apache_best, lsi_apache_best, hugme_apache$data))
apacheComparisonTest
plotRunningData(nb_apache_best, hugme_apache$data, ir_apache_best, lsi_apache_best, "Cmn Img F1 Comparison", "f1", "bottomright")


# load jabref - done
hugme_jabref_loaded <- loadData("data/jabref.csv", CAttractStr)
nrow(hugme_jabref_loaded)
nb_jabref_loaded <- loadData("data/jabref.csv", NBAttractStr)
nrow(nb_jabref_loaded)
ir_jabref_loaded <- loadData("data/jabref.csv", IRAttractStr)
nrow(ir_jabref_loaded)
lsi_jabref_loaded <- loadData("data/jabref.csv", LSIAttractStr)
nrow(lsi_jabref_loaded)

#filtering for hugme
summary(hugme_jabref_loaded)
hugme_jabref <- getFilteredDataEx3(hugme_filter_fraction, hugme_jabref_loaded)
hugme_jabref_oldfilter <- getFilteredData(0.1, hugme_jabref_loaded)

# compare new and old filter
plotRunningFilterData(hugme_jabref$data, hugme_jabref_oldfilter$data, "Filter Comparison", "f1", "bottomright")

nb_jabref <- nb_jabref_loaded
ir_jabref <- ir_jabref_loaded
lsi_jabref <- lsi_jabref_loaded


# compare with and without cda
jabrefCdaTest <- getAllCdaStatsAsRows(cdaTest("JabRef", nb_jabref, ir_jabref, lsi_jabref))
jabrefCdaTest

plotCDAComparison(nb_jabref, ir_jabref, lsi_jabref, "JabRef CDA F1 Comparison", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

nb_jabref_best <- nb_jabref[nb_jabref$cda == "Y",]
ir_jabref_best <- ir_jabref[ir_jabref$cda == "Y",]
lsi_jabref_best <- lsi_jabref[lsi_jabref$cda == "Y",]

# compare the mapping functions
jabrefComparisonTest= getAllComparisonStatsAsRows(comparisonTest("JabRef", nb_jabref_best, ir_jabref_best, lsi_jabref_best, hugme_jabref$data))
jabrefComparisonTest

plotRunningData(nb_jabref_best, hugme_jabref$data, ir_jabref_best, lsi_jabref_best, "JabRef F1 Comparison", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)


#load lucene - done
hugme_lucene_loaded <- loadData("data/lucene.csv", CAttractStr)
nrow(hugme_lucene_loaded)
hugme_lucene<- getFilteredDataEx3(hugme_filter_fraction, hugme_lucene_loaded)
nb_lucene_loaded <- loadData("data/lucene.csv", NBAttractStr)
nb_lucene<- nb_lucene_loaded
ir_lucene_loaded <- loadData("data/lucene.csv", IRAttractStr)
ir_lucene<- ir_lucene_loaded
lsi_lucene_loaded <- loadData("data/lucene.csv", LSIAttractStr)
lsi_lucene<- lsi_lucene_loaded

luceneCdaTest <- getAllCdaStatsAsRows(cdaTest("Lucene", nb_lucene, ir_lucene, lsi_lucene))
luceneCdaTest

plotCDAComparison(nb_lucene, ir_lucene, lsi_lucene, "Lucene CDA F1 Comparison", "f1", "bottomright")

nb_lucene_best <- nb_lucene[nb_lucene$cda == "Y",]
ir_lucene_best <- ir_lucene[ir_lucene$cda == "N",]
lsi_lucene_best <- lsi_lucene[lsi_lucene$cda == "N",]

luceneComparisonTest = getAllComparisonStatsAsRows(comparisonTest("Lucene", nb_lucene_best, ir_lucene_best, lsi_lucene_best, hugme_lucene$data))
luceneComparisonTest
plotRunningData(nb_lucene_best, hugme_lucene$data, ir_lucene_best, lsi_lucene_best, "Lucene F1 Comparison", "f1", "bottomright")


#load teammates
hugme_teammates_loaded <- loadData("data/teammates.csv", CAttractStr)
nrow(hugme_teammates_loaded)
hugme_teammates<- getFilteredDataEx3(hugme_filter_fraction, hugme_teammates_loaded)
nb_teammates_loaded <- loadData("data/teammates.csv", NBAttractStr)
nrow(nb_teammates_loaded)
nb_teammates<- nb_teammates_loaded
ir_teammates_loaded <- loadData("data/teammates.csv", IRAttractStr)
nrow(ir_teammates_loaded)
ir_teammates<- ir_teammates_loaded
lsi_teammates_loaded <- loadData("data/teammates.csv", LSIAttractStr)
nrow(lsi_teammates_loaded)
lsi_teammates<- lsi_teammates_loaded

teammatesCdaTest <- getAllCdaStatsAsRows(cdaTest("TeamMates", nb_teammates, ir_teammates, lsi_teammates))
teammatesCdaTest

plotCDAComparison(nb_teammates, ir_teammates, lsi_teammates, "Teammates CDA F1 Comparison", "f1", "bottomright")

nb_teammates_best <- nb_teammates[nb_teammates$cda == "Y",]
ir_teammates_best <- ir_teammates[ir_teammates$cda == "Y",]
lsi_teammates_best <- lsi_teammates[lsi_teammates$cda == "Y",]

teammatesComparisonTest = getAllComparisonStatsAsRows(comparisonTest("TeamMates", nb_teammates_best, ir_teammates_best, lsi_teammates_best, hugme_teammates$data))
teammatesComparisonTest
plotRunningData(nb_teammates_best, hugme_teammates$data, ir_teammates_best, lsi_teammates_best, "Teammates F1 Comparison", "f1", "bottomright")

#sweethome3d - done
hugme_sweethome3d_loaded <- loadData("data/sweethome3d.csv", CAttractStr)
nrow(hugme_sweethome3d_loaded)
hugme_sweethome3d<- getFilteredDataEx3(hugme_filter_fraction, hugme_sweethome3d_loaded)
nb_sweethome3d_loaded <- loadData("data/sweethome3d.csv", NBAttractStr)
nb_sweethome3d<- nb_sweethome3d_loaded
ir_sweethome3d_loaded <- loadData("data/sweethome3d.csv", IRAttractStr)
ir_sweethome3d<- ir_sweethome3d_loaded
lsi_sweethome3d_loaded <- loadData("data/sweethome3d.csv", LSIAttractStr)
lsi_sweethome3d<- lsi_sweethome3d_loaded

sweethome3dCdaTest <- getAllCdaStatsAsRows(cdaTest("Sweet Home 3D", nb_sweethome3d, ir_sweethome3d, lsi_sweethome3d))
sweethome3dCdaTest

plotCDAComparison(nb_sweethome3d, ir_sweethome3d, lsi_sweethome3d, "SweetHome 3D CDA F1 Comparison", "f1", "bottomright")

nb_sweethome3d_best <- nb_sweethome3d[nb_sweethome3d$cda == "Y",]
ir_sweethome3d_best <- ir_sweethome3d[ir_sweethome3d$cda == "Y",]
lsi_sweethome3d_best <- lsi_sweethome3d[lsi_sweethome3d$cda == "Y",]

sweethome3dComparisonTest = getAllComparisonStatsAsRows(comparisonTest("SweetHome 3D", nb_sweethome3d_best, ir_sweethome3d_best, lsi_sweethome3d_best, hugme_sweethome3d$data))
sweethome3dComparisonTest
plotRunningData(nb_sweethome3d_best, hugme_sweethome3d$data, ir_sweethome3d_best, lsi_sweethome3d_best, "SweetHome 3D F1 Comparison", "f1", "bottomright")


#prom - done
hugme_prom_loaded <- loadData("data/prom.csv", CAttractStr)
nrow(hugme_prom_loaded)
hugme_prom<- getFilteredDataEx3(hugme_filter_fraction, hugme_prom_loaded)
nb_prom_loaded <- loadData("data/prom.csv", NBAttractStr)
nb_prom<- nb_prom_loaded
ir_prom_loaded <- loadData("data/prom.csv", IRAttractStr)
ir_prom<- ir_prom_loaded
lsi_prom_loaded <- loadData("data/prom.csv", LSIAttractStr)
lsi_prom<- lsi_prom_loaded

promCdaTest <- getAllCdaStatsAsRows(cdaTest("ProM", nb_prom, ir_prom, lsi_prom))
promCdaTest

plotCDAComparison(nb_prom, ir_prom, lsi_prom, "ProM CDA F1 Comparison", "f1", "bottomright")

nb_prom_best <- nb_prom[nb_prom$cda == "N",]
ir_prom_best <- ir_prom[ir_prom$cda == "N",]
lsi_prom_best <- lsi_prom[lsi_prom$cda == "Y",]

promComparisonTest = getAllComparisonStatsAsRows(comparisonTest("ProM", nb_prom_best, ir_prom_best, lsi_prom_best, hugme_prom$data))
promComparisonTest
plotRunningData(nb_prom_best, hugme_prom$data, ir_prom_best, lsi_prom_best, "ProM F1 Comparison", "f1", "bottomright")


# compile the cda data for saving
cdaTable <- antCdaTest
cdaTable <- rbind(cdaTable, argoumlCdaTest)
cdaTable <- rbind(cdaTable, apacheCdaTest)
cdaTable <- rbind(cdaTable, jabrefCdaTest)
cdaTable <- rbind(cdaTable, luceneCdaTest)
cdaTable <- rbind(cdaTable, promCdaTest)
cdaTable <- rbind(cdaTable, sweethome3dCdaTest)
cdaTable <- rbind(cdaTable, teammatesCdaTest)

cdaTable
write.table(cdaTable, "cda_comparison.csv", sep = "\t", row.names=FALSE)


# compile the comparison data for saving
comparisonTable <- antComparisonTest
comparisonTable <- rbind(comparisonTable, argoumlComparisonTest)
comparisonTable <- rbind(comparisonTable, apacheComparisonTest)
comparisonTable <- rbind(comparisonTable, jabrefComparisonTest)
comparisonTable <- rbind(comparisonTable, luceneComparisonTest)
comparisonTable <- rbind(comparisonTable, promComparisonTest)
comparisonTable <- rbind(comparisonTable, sweethome3dComparisonTest)
comparisonTable <- rbind(comparisonTable, teammatesComparisonTest)

comparisonTable
write.table(comparisonTable, "comparison.csv", sep = "\t", row.names=FALSE)

plotBoxPlotsAllData("f1", "All the datas")

# raw numbers
nrow(nb_jabref_loaded) +
nrow(hugme_jabref_loaded) +
nrow(ir_jabref_loaded) +
nrow(lsi_jabref_loaded) +

nrow(nb_ant_loaded) +
nrow(hugme_ant_loaded) +
nrow(ir_ant_loaded) +
nrow(lsi_ant_loaded) +  

nrow(nb_argouml_loaded) +
nrow(hugme_argouml_loaded) +
nrow(ir_argouml_loaded) +
nrow(lsi_argouml_loaded) +

nrow(nb_lucene_loaded) +
nrow(hugme_lucene_loaded) +
nrow(ir_lucene_loaded) +
nrow(lsi_lucene_loaded) +

nrow(nb_teammates_loaded) +
nrow(hugme_teammates_loaded) +
nrow(ir_teammates_loaded) +
nrow(lsi_teammates_loaded) + 

nrow(nb_prom_loaded) +
nrow(hugme_prom_loaded) +
nrow(ir_prom_loaded) +
nrow(lsi_prom_loaded) + 

nrow(nb_sweethome3d_loaded) +
nrow(hugme_sweethome3d_loaded) +
nrow(ir_sweethome3d_loaded) +
nrow(lsi_sweethome3d_loaded)

# number of filtered data rows
nrow(hugme_jabref_loaded) - nrow(hugme_jabref$data) + 
nrow(hugme_ant_loaded) - nrow(hugme_ant$data) + 
nrow(hugme_lucene_loaded) - nrow(hugme_lucene$data) +
nrow(hugme_teammates_loaded) - nrow(hugme_teammates$data) +
nrow(hugme_sweethome3d_loaded) - nrow(hugme_sweethome3d$data) +
nrow(hugme_argouml_loaded) - nrow(hugme_argouml$data) +
nrow(hugme_prom_loaded) - nrow(hugme_prom$data)


# image generation and saving to file for article
windows()
par(mar=c(3,5,2,1))
plotBoxPlotsCDA("f1", "CDA F1 Score Comparison")
graph2eps(file="f1_cda.eps", bg = "transparent", fallback_resolution = 600)

windows(height = 11.0, width = 8)
oldPar = par(oma=c(2,2,0,0),mar=c(2,2,2,1), mfrow=c(4,2))
#oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotCDAComparison(nb_ant, ir_ant, lsi_ant, "Ant", "f1", "none")

black <- rgb(0, 0, 0, max = 255, alpha = 255)
lc1 <- rgb(57, 106, 177, max = 255, alpha = 255)
lc2 <- rgb(218, 124, 48, max = 255, alpha = 255)
lc3 <- rgb(62, 150, 81, max = 255, alpha = 255)
lc4 <- rgb(204, 37, 41, max = 255, alpha = 255)

legend("topleft", legend=c("NBAttract", "IRAttract", "LSIAttract", "With CDA", "Without CDA"), col=c(lc1, lc3, lc4, black, black), ncol=2, lty=c(1, 1, 1, 1, 2))

axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotCDAComparison(nb_argouml, ir_argouml, lsi_argouml, "ArgoUML", "f1", "none")

plotCDAComparison(nb_apache, ir_apache, lsi_apache, "Commons Imaging", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotCDAComparison(nb_jabref, ir_jabref, lsi_jabref, "JabRef", "f1", "none")

plotCDAComparison(nb_lucene, ir_lucene, lsi_lucene, "Lucene", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotCDAComparison(nb_prom, ir_prom, lsi_prom, "ProM", "f1", "none")

plotCDAComparison(nb_sweethome3d, ir_sweethome3d, lsi_sweethome3d, "Sweet Home 3D", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotCDAComparison(nb_teammates, ir_teammates, lsi_teammates, "TeamMates", "f1", "none")

mtext(text="Initial Mapping Size",side=1,line=0,outer=TRUE)
mtext(text="F1 Score",side=2,line=0,outer=TRUE)

graph2eps(file="f1_cda_plots.eps", bg = "transparent", fallback_resolution = 600)

windows()
par(mar=c(3,5,2,1))
plotBoxPlotsAllData("f1", "Attraction Function F1 Score Comparisons")
graph2eps(file="f1_comparison_boxplots.eps", bg = "transparent", fallback_resolution = 600)

windows(height = 11.0, width = 8)
oldPar = par(oma=c(2,2,0,0),mar=c(2,2,2,1), mfrow=c(4,2))
#oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotRunningData(nb_ant_best, hugme_ant$data, ir_ant_best, lsi_ant_best, "Ant", "f1", "none")
lc1 <- rgb(57, 106, 177, max = 255, alpha = 255)
lc2 <- rgb(218, 124, 48, max = 255, alpha = 255)
lc3 <- rgb(62, 150, 81, max = 255, alpha = 255)
lc4 <- rgb(204, 37, 41, max = 255, alpha = 255)

legend("topleft", legend=c("NBAttract", "CountAttract", "IRAttract", "LSIAttract"), lty=1, col=c(lc1, lc2, lc3, lc4))

axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(nb_argouml_best, hugme_argouml$data, ir_argouml_best, lsi_argouml_best, "ArgoUML", "f1", "none")

plotRunningData(nb_apache_best, hugme_apache$data, ir_apache_best, lsi_apache_best, "Commons Imaging", "f1", "none")

axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(nb_jabref_best, hugme_jabref$data, ir_jabref_best, lsi_jabref_best, "JabRef", "f1", "none")

plotRunningData(nb_lucene_best, hugme_lucene$data, ir_lucene_best, lsi_lucene_best, "Lucene", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(nb_prom_best, hugme_prom$data, ir_prom_best, lsi_prom_best, "ProM", "f1", "none")

plotRunningData(nb_sweethome3d_best, hugme_sweethome3d$data, ir_sweethome3d_best, lsi_sweethome3d_best, "Sweet Home 3D", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(nb_teammates_best, hugme_teammates$data, ir_teammates_best, lsi_teammates_best, "TeamMates", "f1", "none")

mtext(text="Initial Mapping Size",side=1,line=0,outer=TRUE)
mtext(text="F1 Score",side=2,line=0,outer=TRUE)

graph2eps(file="f1_comparison_plots.eps", bg = "transparent", fallback_resolution = 600)


