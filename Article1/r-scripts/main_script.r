rm(list=ls(all=TRUE))
library(coin)
library(caTools)
#install.packages("export")
library(export)

setwd("C:/hObbE/projects/coding/research/s4rdm3x/r-scripts/Article1/")

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


getFilteredDataEx <- function(a_best_percent, a_data) {
      delta = 0.1
	limit = 0

	# calculate the phi limits based on the average phi for 10% increments of mappingPercent
	phi_min = 0
	phi_max = 0
	for (i in 0:9) {
		data <- a_data[a_data$mappingPercent >= limit & a_data$mappingPercent < limit + delta,]
		out <- getOmegaPhi(a_best_percent, data)
		phi_min = phi_min + out$phi_min
		phi_max = phi_max + out$phi_max
		limit = limit + delta
	}
	phi_min = phi_min / 10
	phi_max = phi_max / 10

	# filter every 10% mappingPercent using the best_percent omega and phi values
	limit = delta
	data <- a_data[a_data$mappingPercent < limit,]
	filtered <- getFilteredDataFixedPhi(a_best_percent, data, phi_min, phi_max)
	out$data <- filtered$data

      for (i in 1:9) {


	  data <- a_data[a_data$mappingPercent >= limit & a_data$mappingPercent < limit + delta,]
	  filtered <- getFilteredDataFixedPhi(a_best_percent, data, phi_min, phi_max)

	  message(paste('limit=',limit))
	  message(paste('rows=',nrow(data)))
	  message(paste('omega_min=',filtered$omega_min))
	  message(paste('omega_max=',filtered$omega_max))
	  message(paste('phi_min=',filtered$phi_min))
	  message(paste('phi_max=',filtered$phi_max))





	  out$data <- rbind(out$data, filtered$data)
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
		nb_jabref_best[[a_column]], hugme_jabref$data[[a_column]], ir_jabref_best[[a_column]], lsi_jabref_best[[a_column]],
		nb_lucene_best[[a_column]], hugme_lucene$data[[a_column]], ir_lucene_best[[a_column]], lsi_lucene_best[[a_column]],
		nb_prom_best[[a_column]], hugme_prom$data[[a_column]], ir_prom_best[[a_column]], lsi_prom_best[[a_column]],
		nb_sweethome3d_best[[a_column]], hugme_sweethome3d$data[[a_column]], ir_sweethome3d_best[[a_column]], lsi_sweethome3d_best[[a_column]],
		nb_teammates_best[[a_column]], hugme_teammates$data[[a_column]], ir_teammates_best[[a_column]], lsi_teammates_best[[a_column]],


		main = a_title,
		at = rev(c(1, 2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 31, 32, 33, 34)),
		names = c("Ant", "", "", "", "A.UML", "", "", "", "JabRef", "", "", "", "Lucene", "", "", "", "ProM", "", "", "", "S.H.3D", "", "", "", "T.Mates", "", "", ""),
		las = 2,
		col = c(rgb(114, 147, 203, max = 255),rgb(225, 151, 76, max = 255), rgb(132, 186, 91, max = 255), rgb(204, 37, 41, max = 255, alpha = 255)),
		border = "black",
		horizontal = TRUE,
		notch = FALSE,
		outpch = 46		# outlier ASCII for .
	)
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
				43, 44, 45, 46, 47, 48)),
		names = c("Ant", "", "", "", "", "", "A.UML", "", "", "", "", "", "JabRef", "", "", "", "", "", "Lucene", "", "", "", "", "", "ProM", "", "", "", "", "", "S.H.3D", "", "", "", "", "", "T.Mates", "", "", "", "", ""),
		las = 2,
		col = c(rgb(114, 147, 203, max = 255), rgb(114, 147, 203, max = 255), rgb(132, 186, 91, max = 255), rgb(132, 186, 91, max = 255), rgb(204, 37, 41, max = 255, alpha = 255), rgb(204, 37, 41, max = 255, alpha = 255)),
		border = "black",
		horizontal = TRUE,
		notch = FALSE,
		outpch = 46,	# outlier ASCII for .
		xlab = "F1 Score"
	)
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




# load jabref
hugme_jabref_loaded <- loadData("data/jabref.csv", "HuGMe:HuGMe")
nrow(hugme_jabref_loaded)
nb_jabref_loaded <- loadData("data/jabref.csv", "NaiveBayes:NB")
nrow(nb_jabref_loaded)
ir_jabref_loaded <- loadData("data/jabref.csv", "LSI_IR:IR")
nrow(ir_jabref_loaded)
lsi_jabref_loaded <- loadData("data/jabref.csv", "LSI_IR:LSI")
nrow(lsi_jabref_loaded)

#filtering for hugme
summary(hugme_jabref_loaded)
hugme_jabref <- getFilteredDataEx(0.1, hugme_jabref_loaded)
hugme_jabref_oldfilter <- getFilteredData(0.1, hugme_jabref_loaded)

# compare new and old filter
plotRunningFilterData(hugme_jabref$data, hugme_jabref_oldfilter$data, "Filter Comparison", "f1", "bottomright")

nb_jabref <- nb_jabref_loaded
ir_jabref <- ir_jabref_loaded
lsi_jabref <- lsi_jabref_loaded

summary(hugme_jabref)
hugme_jabref$omega_min
hugme_jabref$omega_max
hugme_jabref$phi_min
hugme_jabref$phi_max

nrow(hugme_jabref$data)

wilcoxTest(nb_jabref[nb_jabref$cda == "Y",], nb_jabref[nb_jabref$cda == "N", ], "f1")
wilcoxTest(ir_jabref[ir_jabref$cda == "Y",], ir_jabref[ir_jabref$cda == "N", ], "f1")
wilcoxTest(lsi_jabref[lsi_jabref$cda == "Y",], lsi_jabref[lsi_jabref$cda == "N", ], "f1")

plotCDAComparison(nb_jabref, ir_jabref, lsi_jabref, "JabRef CDA F1 Comparison", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)




nb_jabref_best <- nb_jabref[nb_jabref$cda == "Y",]
ir_jabref_best <- ir_jabref[ir_jabref$cda == "Y",]
lsi_jabref_best <- lsi_jabref[lsi_jabref$cda == "Y",]


wilcoxTest(nb_jabref_best, ir_jabref_best, "f1")
wilcoxTest(nb_jabref_best, lsi_jabref_best, "f1")
wilcoxTest(nb_jabref_best, hugme_jabref$data, "f1")
wilcoxTest(ir_jabref_best, lsi_jabref_best, "f1")
wilcoxTest(ir_jabref_best, hugme_jabref$data, "f1")
wilcoxTest(lsi_jabref_best, hugme_jabref$data, "f1")

plotRunningData(nb_jabref_best, hugme_jabref$data, ir_jabref_best, lsi_jabref_best, "JabRef F1 Comparison", "f1", "bottomright")





#load ant
hugme_ant_loaded <- loadData("data/ant.csv", "HuGMe:HuGMe")
hugme_ant  <- getFilteredDataEx(0.1, hugme_ant_loaded)
nb_ant_loaded <- loadData("data/ant.csv", "NaiveBayes:NB")
nb_ant <- nb_ant_loaded
ir_ant_loaded <- loadData("data/ant.csv", "LSI_IR:IR")
ir_ant <- ir_ant_loaded
lsi_ant_loaded <- loadData("data/ant.csv", "LSI_IR:LSI")
lsi_ant <- lsi_ant_loaded

wilcoxTest(nb_ant[nb_ant$cda == "Y",], nb_ant[nb_ant$cda == "N", ], "f1")
wilcoxTest(ir_ant[ir_ant$cda == "Y",], ir_ant[ir_ant$cda == "N", ], "f1")
wilcoxTest(lsi_ant[lsi_ant$cda == "Y",], lsi_ant[lsi_ant$cda == "N", ], "f1")

plotCDAComparison(nb_ant, ir_ant, lsi_ant, "Ant CDA F1 Comparison", "f1", "bottomright")

nb_ant_best <- nb_ant[nb_ant$cda == "Y",]
ir_ant_best <- ir_ant[ir_ant$cda == "Y",]
lsi_ant_best <- lsi_ant[lsi_ant$cda == "Y",]

wilcoxTest(nb_ant_best, ir_ant_best, "f1")
wilcoxTest(nb_ant_best, lsi_ant_best, "f1")
wilcoxTest(nb_ant_best, hugme_ant$data, "f1")
wilcoxTest(ir_ant_best, lsi_ant_best, "f1")
wilcoxTest(ir_ant_best, hugme_ant$data, "f1")
wilcoxTest(lsi_ant_best, hugme_ant$data, "f1")

plotRunningData(nb_ant_best, hugme_ant$data, ir_ant_best, lsi_ant_best, "Ant F1 Comparison", "f1", "bottomright")



#load argouml
hugme_argouml_loaded <- loadData("data/argouml.csv", "HuGMe:HuGMe")
hugme_argouml  <- getFilteredDataEx(0.1, hugme_argouml_loaded)
nb_argouml_loaded <- loadData("data/argouml.csv", "NaiveBayes:NB")
nb_argouml <- nb_argouml_loaded
ir_argouml_loaded <- loadData("data/argouml.csv", "LSI_IR:IR")
ir_argouml <- ir_argouml_loaded
lsi_argouml_loaded <- loadData("data/argouml.csv", "LSI_IR:LSI")
lsi_argouml <- lsi_argouml_loaded

wilcoxTest(nb_argouml[nb_argouml$cda == "Y",], nb_argouml[nb_argouml$cda == "N", ], "f1")
wilcoxTest(ir_argouml[ir_argouml$cda == "Y",], ir_argouml[ir_argouml$cda == "N", ], "f1")
wilcoxTest(lsi_argouml[lsi_argouml$cda == "Y",], lsi_argouml[lsi_argouml$cda == "N", ], "f1")

plotCDAComparison(nb_argouml, ir_argouml, lsi_argouml, "ArgoUML CDA F1 Comparison", "f1", "bottomright")

nb_argouml_best <- nb_argouml[nb_argouml$cda == "Y",]
ir_argouml_best <- ir_argouml[ir_argouml$cda == "Y",]
lsi_argouml_best <- lsi_argouml[lsi_argouml$cda == "Y",]

wilcoxTest(nb_argouml_best, ir_argouml_best, "f1")
wilcoxTest(nb_argouml_best, lsi_argouml_best, "f1")
wilcoxTest(nb_argouml_best, hugme_argouml$data, "f1")
wilcoxTest(ir_argouml_best, lsi_argouml_best, "f1")
wilcoxTest(ir_argouml_best, hugme_argouml$data, "f1")
wilcoxTest(lsi_argouml_best, hugme_argouml$data, "f1")

plotRunningData(nb_argouml_best, hugme_argouml$data, ir_argouml_best, lsi_argouml_best, "ArgoUML F1 Comparison", "f1", "bottomright")

#load lucene
hugme_lucene_loaded <- loadData("data/lucene.csv", "HuGMe:HuGMe")
hugme_lucene<- getFilteredDataEx(0.1, hugme_lucene_loaded)
nb_lucene_loaded <- loadData("data/lucene.csv", "NaiveBayes:NB")
nb_lucene<- nb_lucene_loaded
ir_lucene_loaded <- loadData("data/lucene.csv", "LSI_IR:IR")
ir_lucene<- ir_lucene_loaded
lsi_lucene_loaded <- loadData("data/lucene.csv", "LSI_IR:LSI")
lsi_lucene<- lsi_lucene_loaded

wilcoxTest(nb_lucene[nb_lucene$cda == "Y",], nb_lucene[nb_lucene$cda == "N", ], "f1")
wilcoxTest(ir_lucene[ir_lucene$cda == "Y",], ir_lucene[ir_lucene$cda == "N", ], "f1")
wilcoxTest(lsi_lucene[lsi_lucene$cda == "Y",], lsi_lucene[lsi_lucene$cda == "N", ], "f1")

plotCDAComparison(nb_lucene, ir_lucene, lsi_lucene, "Lucene CDA F1 Comparison", "f1", "bottomright")

nb_lucene_best <- nb_lucene[nb_lucene$cda == "Y",]
ir_lucene_best <- ir_lucene[ir_lucene$cda == "N",]
lsi_lucene_best <- lsi_lucene[lsi_lucene$cda == "Y",]

wilcoxTest(nb_lucene_best, ir_lucene_best, "f1")
wilcoxTest(nb_lucene_best, lsi_lucene_best, "f1")
wilcoxTest(nb_lucene_best, hugme_lucene$data, "f1")
wilcoxTest(ir_lucene_best, lsi_lucene_best, "f1")
wilcoxTest(ir_lucene_best, hugme_lucene$data, "f1")
wilcoxTest(lsi_lucene_best, hugme_lucene$data, "f1")

plotRunningData(nb_lucene_best, hugme_lucene$data, ir_lucene_best, lsi_lucene_best, "Lucene F1 Comparison", "f1", "bottomright")


#load teammates
hugme_teammates_loaded <- loadData("data/teammates.csv", "HuGMe:HuGMe")
hugme_teammates<- getFilteredDataEx(0.1, hugme_teammates_loaded)
nb_teammates_loaded <- loadData("data/teammates.csv", "NaiveBayes:NB")
nb_teammates<- nb_teammates_loaded
ir_teammates_loaded <- loadData("data/teammates.csv", "LSI_IR:IR")
ir_teammates<- ir_teammates_loaded
lsi_teammates_loaded <- loadData("data/teammates.csv", "LSI_IR:LSI")
lsi_teammates<- lsi_teammates_loaded

wilcoxTest(nb_teammates[nb_teammates$cda == "Y",], nb_teammates[nb_teammates$cda == "N", ], "f1")
wilcoxTest(ir_teammates[ir_teammates$cda == "Y",], ir_teammates[ir_teammates$cda == "N", ], "f1")
wilcoxTest(lsi_teammates[lsi_teammates$cda == "Y",], lsi_teammates[lsi_teammates$cda == "N", ], "f1")

plotCDAComparison(nb_teammates, ir_teammates, lsi_teammates, "Teammates CDA F1 Comparison", "f1", "bottomright")

nb_teammates_best <- nb_teammates[nb_teammates$cda == "Y",]
ir_teammates_best <- ir_teammates[ir_teammates$cda == "Y",]
lsi_teammates_best <- lsi_teammates[lsi_teammates$cda == "Y",]

wilcoxTest(nb_teammates_best, ir_teammates_best, "f1")
wilcoxTest(nb_teammates_best, lsi_teammates_best, "f1")
wilcoxTest(nb_teammates_best, hugme_teammates$data, "f1")
wilcoxTest(ir_teammates_best, lsi_teammates_best, "f1")
wilcoxTest(ir_teammates_best, hugme_teammates$data, "f1")
wilcoxTest(lsi_teammates_best, hugme_teammates$data, "f1")


plotRunningData(nb_teammates_best, hugme_teammates$data, ir_teammates_best, lsi_teammates_best, "Teammates F1 Comparison", "f1", "bottomright")

#sweethome3d
hugme_sweethome3d_loaded <- loadData("data/sweethome3d.csv", "HuGMe:HuGMe")
hugme_sweethome3d<- getFilteredDataEx(0.1, hugme_sweethome3d_loaded)
nb_sweethome3d_loaded <- loadData("data/sweethome3d.csv", "NaiveBayes:NB")
nb_sweethome3d<- nb_sweethome3d_loaded
ir_sweethome3d_loaded <- loadData("data/sweethome3d.csv", "LSI_IR:IR")
ir_sweethome3d<- ir_sweethome3d_loaded
lsi_sweethome3d_loaded <- loadData("data/sweethome3d.csv", "LSI_IR:LSI")
lsi_sweethome3d<- lsi_sweethome3d_loaded

wilcoxTest(nb_sweethome3d[nb_sweethome3d$cda == "Y",], nb_sweethome3d[nb_sweethome3d$cda == "N", ], "f1")
wilcoxTest(ir_sweethome3d[ir_sweethome3d$cda == "Y",], ir_sweethome3d[ir_sweethome3d$cda == "N", ], "f1")
wilcoxTest(lsi_sweethome3d[lsi_sweethome3d$cda == "Y",], lsi_sweethome3d[lsi_sweethome3d$cda == "N", ], "f1")

plotCDAComparison(nb_sweethome3d, ir_sweethome3d, lsi_sweethome3d, "SweetHome 3D CDA F1 Comparison", "f1", "bottomright")

nb_sweethome3d_best <- nb_sweethome3d[nb_sweethome3d$cda == "Y",]
ir_sweethome3d_best <- ir_sweethome3d[ir_sweethome3d$cda == "Y",]
lsi_sweethome3d_best <- lsi_sweethome3d[lsi_sweethome3d$cda == "Y",]

wilcoxTest(nb_sweethome3d_best, ir_sweethome3d_best, "f1")
wilcoxTest(nb_sweethome3d_best, lsi_sweethome3d_best, "f1")
wilcoxTest(nb_sweethome3d_best, hugme_sweethome3d$data, "f1")
wilcoxTest(ir_sweethome3d_best, lsi_sweethome3d_best, "f1")
wilcoxTest(ir_sweethome3d_best, hugme_sweethome3d$data, "f1")
wilcoxTest(lsi_sweethome3d_best, hugme_sweethome3d$data, "f1")

plotRunningData(nb_sweethome3d_best, hugme_sweethome3d$data, ir_sweethome3d_best, lsi_sweethome3d_best, "SweetHome 3D F1 Comparison", "f1", "bottomright")


#prom
hugme_prom_loaded <- loadData("data/prom.csv", "HuGMe:HuGMe")
hugme_prom<- getFilteredDataEx(0.1, hugme_prom_loaded)
nb_prom_loaded <- loadData("data/prom.csv", "NaiveBayes:NB")
nb_prom<- nb_prom_loaded
ir_prom_loaded <- loadData("data/prom.csv", "LSI_IR:IR")
ir_prom<- ir_prom_loaded
lsi_prom_loaded <- loadData("data/prom.csv", "LSI_IR:LSI")
lsi_prom<- lsi_prom_loaded

wilcoxTest(nb_prom[nb_prom$cda == "Y",], nb_prom[nb_prom$cda == "N", ], "f1")
wilcoxTest(ir_prom[ir_prom$cda == "Y",], ir_prom[ir_prom$cda == "N", ], "f1")
wilcoxTest(lsi_prom[lsi_prom$cda == "Y",], lsi_prom[lsi_prom$cda == "N", ], "f1")

plotCDAComparison(nb_prom, ir_prom, lsi_prom, "ProM CDA F1 Comparison", "f1", "bottomright")

nb_prom_best <- nb_prom[nb_prom$cda == "Y",]
ir_prom_best <- ir_prom[ir_prom$cda == "N",]
lsi_prom_best <- lsi_prom[lsi_prom$cda == "Y",]

plotRunningData(nb_prom_best, hugme_prom$data, ir_prom_best, lsi_prom_best, "ProM F1 Comparison", "f1", "bottomright")

wilcoxTest(nb_prom_best, ir_prom_best, "f1")
wilcoxTest(nb_prom_best, lsi_prom_best, "f1")
wilcoxTest(nb_prom_best, hugme_prom$data, "f1")
wilcoxTest(ir_prom_best, lsi_prom_best, "f1")
wilcoxTest(ir_prom_best, hugme_prom$data, "f1")
wilcoxTest(lsi_prom_best, hugme_prom$data, "f1")



wilcoxTest(ir_prom[ir_prom$cda == "Y",], ir_prom[ir_prom$cda == "N", ], "f1")
wilcoxTest(lsi_prom[lsi_prom$cda == "Y",], lsi_prom[lsi_prom$cda == "N", ], "f1")




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
plotBoxPlotsCDA("f1", "CDA F1 Score Comparison")
graph2eps(file="f1_cda.eps", bg = "transparent", fallback_resolution = 600)

oldPar = par(oma=c(2,2,0,0),mar=c(2,2,2,1), mfrow=c(4,2))
#oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotCDAComparison(nb_ant, ir_ant, lsi_ant, "ANT CDA F1 Comparison", "f1", "topleft")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotCDAComparison(nb_argouml, ir_argouml, lsi_argouml, "ArgoUML CDA F1 Comparison", "f1", "topleft")

plotCDAComparison(nb_jabref, ir_jabref, lsi_jabref, "JabRef CDA F1 Comparison", "f1", "bottomleft")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotCDAComparison(nb_lucene, ir_lucene, lsi_lucene, "Lucene CDA F1 Comparison", "f1", "bottomleft")

plotCDAComparison(nb_prom, ir_prom, lsi_prom, "ProM CDA F1 Comparison", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotCDAComparison(nb_sweethome3d, ir_sweethome3d, lsi_sweethome3d, "SweetHome 3D CDA F1 Comparison", "f1", "topleft")

plotCDAComparison(nb_teammates, ir_teammates, lsi_teammates, "TeamMates CDA F1 Comparison", "f1", "topleft")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

mtext(text="Initial Mapping Size",side=1,line=0,outer=TRUE)
mtext(text="F1 Score",side=2,line=0,outer=TRUE)

graph2eps(file="f1_cda_plots.eps", bg = "transparent", fallback_resolution = 600)


plotBoxPlotsAllData("f1", "Attraction Function F1 Score Comparisons")
graph2eps(file="f1_comparison_boxplots.eps", bg = "transparent", fallback_resolution = 600)


oldPar = par(oma=c(2,2,0,0),mar=c(2,2,2,1), mfrow=c(4,2))
#oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotRunningData(nb_ant_best, hugme_ant$data, ir_ant_best, lsi_ant_best, "Ant", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(nb_argouml_best, hugme_argouml$data, ir_argouml_best, lsi_argouml_best, "ArgoUML", "f1", "none")

plotRunningData(nb_jabref_best, hugme_jabref$data, ir_jabref_best, lsi_jabref_best, "JabRef", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(nb_lucene_best, hugme_lucene$data, ir_lucene_best, lsi_lucene_best, "Lucene", "f1", "none")

plotRunningData(nb_prom_best, hugme_prom$data, ir_prom_best, lsi_prom_best, "ProM", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(nb_sweethome3d_best, hugme_sweethome3d$data, ir_sweethome3d_best, lsi_sweethome3d_best, "SweetHome 3D", "f1", "none")

plotRunningData(nb_teammates_best, hugme_teammates$data, ir_teammates_best, lsi_teammates_best, "TeamMates", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

# plot the legend in the last slot
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

lc1 <- rgb(57, 106, 177, max = 255, alpha = 255)
lc2 <- rgb(218, 124, 48, max = 255, alpha = 255)
lc3 <- rgb(62, 150, 81, max = 255, alpha = 255)
lc4 <- rgb(204, 37, 41, max = 255, alpha = 255)

legend("topleft", legend=c("NBAttract", "CountAttract", "IRAttract", "LSIAttract"), lty=1, col=c(lc1, lc2, lc3, lc4))


mtext(text="Initial Mapping Size",side=1,line=0,outer=TRUE)
mtext(text="F1 Score",side=2,line=0,outer=TRUE)

graph2eps(file="f1_comparison_plots.eps", bg = "transparent", fallback_resolution = 600)




# testing the hypothesis that highest initial mapping gives the worst score
f1_limit = 0.05
test_data <- nb_ant_best[nb_ant_best$f1 < f1_limit,]
test_data <- rbind(test_data, hugme_ant$data[hugme_ant$data$f1 < f1_limit,])
test_data <- rbind(test_data, ir_ant_best[ir_ant_best$f1 < f1_limit,])
test_data <- rbind(test_data, lsi_ant_best[lsi_ant_best$f1 < f1_limit,])

test_data <- rbind(test_data, nb_argouml_best[nb_argouml_best$f1 < f1_limit,])
test_data <- rbind(test_data, hugme_argouml$data[hugme_argouml$data$f1 < f1_limit,])
test_data <- rbind(test_data, ir_argouml_best[ir_argouml_best$f1 < f1_limit,])
test_data <- rbind(test_data, lsi_argouml_best[lsi_argouml_best$f1 < f1_limit,])

test_data <- rbind(test_data, nb_jabref_best[nb_jabref_best$f1 < f1_limit,])
test_data <- rbind(test_data, hugme_jabref$data[hugme_jabref$data$f1 < f1_limit,])
test_data <- rbind(test_data, ir_jabref_best[ir_jabref_best$f1 < f1_limit,])
test_data <- rbind(test_data, lsi_jabref_best[lsi_jabref_best$f1 < f1_limit,])

test_data <- rbind(test_data, nb_lucene_best[nb_lucene_best$f1 < f1_limit,])
test_data <- rbind(test_data, hugme_lucene$data[hugme_lucene$data$f1 < f1_limit,])
test_data <- rbind(test_data, ir_lucene_best[ir_lucene_best$f1 < f1_limit,])
test_data <- rbind(test_data, lsi_lucene_best[lsi_lucene_best$f1 < f1_limit,])

test_data <- rbind(test_data, nb_prom_best[nb_prom_best$f1 < f1_limit,])
test_data <- rbind(test_data, hugme_prom$data[hugme_prom$data$f1 < f1_limit,])
test_data <- rbind(test_data, ir_prom_best[ir_prom_best$f1 < f1_limit,])
test_data <- rbind(test_data, lsi_prom_best[lsi_prom_best$f1 < f1_limit,])

test_data <- rbind(test_data, nb_sweethome3d_best[nb_sweethome3d_best$f1 < f1_limit,])
test_data <- rbind(test_data, hugme_sweethome3d$data[hugme_sweethome3d$data$f1 < f1_limit,])
test_data <- rbind(test_data, ir_sweethome3d_best[ir_sweethome3d_best$f1 < f1_limit,])
test_data <- rbind(test_data, lsi_sweethome3d_best[lsi_sweethome3d_best$f1 < f1_limit,])

test_data <- rbind(test_data, nb_teammates_best[nb_teammates_best$f1 < f1_limit,])
test_data <- rbind(test_data, hugme_teammates$data[hugme_teammates$data$f1 < f1_limit,])
test_data <- rbind(test_data, ir_teammates_best[ir_teammates_best$f1 < f1_limit,])
test_data <- rbind(test_data, lsi_teammates_best[lsi_teammates_best$f1 < f1_limit,])

plot(test_data$mappingPercent, test_data$f1, xlab="Initial Mapping Size", ylab="F1-Score")
title("Initial Mapping Size for F1 Scores < 0.05")
graph2eps(file="f1_worst_plot.eps", bg = "transparent", fallback_resolution = 600)



# Old stuff to be removed


oldPar = par(fig=c(0.39,0.68,0.55,1.0), new = TRUE)
plotRunningData(nbc_ant, hugme_ant$data, hugme_manual_ant$data, "Ant", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.55,1.0), new = TRUE)
plotRunningData(nbc_argouml, hugme_argouml$data, hugme_manual_argouml$data, "ArgoUML", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

# row 2
oldPar = par(fig=c(0.075,0.365,0.075,0.5125), new = TRUE)
plotRunningData(nbc_lucene, hugme_lucene$data, hugme_manual_lucene$data, "Lucene", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

oldPar = par(fig=c(0.39,0.68,0.075,0.5125), new = TRUE)
plotRunningData(nbc_teammates, hugme_teammates$data, hugme_manual_teammates$data, "Teammates", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.075,0.5125), new = TRUE)
plotRunningData(nbc_sweethome3d, hugme_sweethome3d$data, hugme_manual_sweethome3d$data, "Sweet Home 3D", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

legend(a_legendPos, legend=c("NB", "IR", "LSI", "CDA", "NO CDA"),col=c(c1, c3, c4, black, black), lty=c(1, 1, 1, 1, 2))


mtext(text="Initial Mapping Fraction",side=1,line=0,outer=TRUE)
mtext(text="performance",side=2,line=0,outer=TRUE)









# after filtering
nrow(nbc_jabref) +
nrow(hugme_jabref$data) +
nrow(hugme_manual_jabref$data) +

nrow(nbc_ant) +
nrow(hugme_ant$data) +
nrow(hugme_manual_ant$data) +

nrow(nbc_argouml) +
nrow(hugme_argouml$data) +
nrow(hugme_manual_argouml$data) +

nrow(nbc_lucene) +
nrow(hugme_lucene$data) +
nrow(hugme_manual_lucene$data) +

nrow(nbc_teammates) +
nrow(hugme_teammates$data) +
nrow(hugme_manual_teammates$data) +

nrow(nbc_sweethome3d) +
nrow(hugme_sweethome3d$data) +
nrow(hugme_manual_sweethome3d$data)




oldPar = par(oma=c(2,2,0,0),mar=c(0,0,2,0), mfrow=c(2,3))
oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotRunningData(nbc_jabref, hugme_jabref$data, hugme_manual_jabref$data, "JabRef", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

oldPar = par(fig=c(0.39,0.68,0.55,1.0), new = TRUE)
plotRunningData(nbc_ant, hugme_ant$data, hugme_manual_ant$data, "Ant", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.55,1.0), new = TRUE)
plotRunningData(nbc_argouml, hugme_argouml$data, hugme_manual_argouml$data, "ArgoUML", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

# row 2
oldPar = par(fig=c(0.075,0.365,0.075,0.5125), new = TRUE)
plotRunningData(nbc_lucene, hugme_lucene$data, hugme_manual_lucene$data, "Lucene", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

oldPar = par(fig=c(0.39,0.68,0.075,0.5125), new = TRUE)
plotRunningData(nbc_teammates, hugme_teammates$data, hugme_manual_teammates$data, "Teammates", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.075,0.5125), new = TRUE)
plotRunningData(nbc_sweethome3d, hugme_sweethome3d$data, hugme_manual_sweethome3d$data, "Sweet Home 3D", "performance", "topright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

mtext(text="Initial Mapping Fraction",side=1,line=0,outer=TRUE)
mtext(text="performance",side=2,line=0,outer=TRUE)

graph2eps(file="p_all.eps", bg = "transparent", fallback_resolution = 600)

# F1 Score
oldPar = par(oma=c(2,2,0,0),mar=c(0,0,2,0), mfrow=c(2,3))
oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotRunningData(nbc_jabref, hugme_jabref$data, hugme_manual_jabref$data, "JabRef", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

oldPar = par(fig=c(0.39,0.68,0.55,1.0), new = TRUE)
plotRunningData(nbc_ant, hugme_ant$data, hugme_manual_ant$data, "Ant", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.55,1.0), new = TRUE)
plotRunningData(nbc_argouml, hugme_argouml$data, hugme_manual_argouml$data, "ArgoUML", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

# row 2
oldPar = par(fig=c(0.075,0.365,0.075,0.5125), new = TRUE)
plotRunningData(nbc_lucene, hugme_lucene$data, hugme_manual_lucene$data, "Lucene", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

oldPar = par(fig=c(0.39,0.68,0.075,0.5125), new = TRUE)
plotRunningData(nbc_teammates, hugme_teammates$data, hugme_manual_teammates$data, "Teammates", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.075,0.5125), new = TRUE)
plotRunningData(nbc_sweethome3d, hugme_sweethome3d$data, hugme_manual_sweethome3d$data, "Sweet Home 3D", "f1", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

mtext(text="Initial Mapping Fraction",side=1,line=0,outer=TRUE)
mtext(text="Precision and Recall F1 score",side=2,line=0,outer=TRUE)

graph2eps(file="f1_all.eps", bg = "transparent", fallback_resolution = 600)

par(oldPar)


# Precsion Score
oldPar = par(oma=c(2,2,0,0),mar=c(0,0,2,0), mfrow=c(2,3))
oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotRunningData(nbc_jabref, hugme_jabref$data, hugme_manual_jabref$data, "JabRef", "precision", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

oldPar = par(fig=c(0.39,0.68,0.55,1.0), new = TRUE)
plotRunningData(nbc_ant, hugme_ant$data, hugme_manual_ant$data, "Ant", "precision", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.55,1.0), new = TRUE)
plotRunningData(nbc_argouml, hugme_argouml$data, hugme_manual_argouml$data, "ArgoUML", "precision", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

# row 2
oldPar = par(fig=c(0.075,0.365,0.075,0.5125), new = TRUE)
plotRunningData(nbc_lucene, hugme_lucene$data, hugme_manual_lucene$data, "Lucene", "precision", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)

oldPar = par(fig=c(0.39,0.68,0.075,0.5125), new = TRUE)
plotRunningData(nbc_teammates, hugme_teammates$data, hugme_manual_teammates$data, "Teammates", "precision", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

oldPar = par(fig=c(0.705,0.99,0.075,0.5125), new = TRUE)
plotRunningData(nbc_sweethome3d, hugme_sweethome3d$data, hugme_manual_sweethome3d$data, "Sweet Home 3D", "precision", "bottomright")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01, labels=FALSE)

mtext(text="Initial Mapping Fraction",side=1,line=0,outer=TRUE)
mtext(text="Precision",side=2,line=0,outer=TRUE)




plotBoxPlotsAllData("Performance", "Performance Comparison")
plotBoxPlotsAllData("f1", "F1 Comparison")



wilcoxTest(nb_jabref_best, hugme_jabref$data, "f1")
wilcoxTest(nb_jabref_best, ir_jabref_best, "f1")
wilcoxTest(nb_jabref_best, lsi_jabref_best, "f1")
wilcoxTest(ir_jabref_best, lsi_jabref_best, "f1")
wilcoxTest(ir_jabref_best, hugme_jabref$data, "f1")
wilcoxTest(lsi_jabref_best, hugme_jabref$data, "f1")


wilcoxTest(nb_ant_best, hugme_ant$data, "f1")
wilcoxTest(nb_ant_best, ir_ant_best, "f1")
wilcoxTest(nb_ant_best, lsi_ant_best, "f1")
wilcoxTest(ir_ant_best, lsi_ant_best, "f1")
wilcoxTest(ir_ant_best, hugme_ant$data, "f1")
wilcoxTest(lsi_ant_best, hugme_ant$data, "f1")




wilcoxTest(nbc_jabref, hugme_jabref$data, "performance")
wilcoxTest(nbc_jabref, hugme_manual_jabref$data, "performance")

wilcoxTest(nbc_ant, hugme_ant$data, "performance")
wilcoxTest(nbc_ant, hugme_manual_ant$data, "performance")

wilcoxTest(nbc_argouml, hugme_argouml$data, "performance")
wilcoxTest(nbc_argouml, hugme_manual_argouml$data, "performance")

wilcoxTest(nbc_lucene, hugme_lucene$data, "performance")
wilcoxTest(nbc_lucene, hugme_manual_lucene$data, "performance")

wilcoxTest(nbc_teammates, hugme_teammates$data, "performance")
wilcoxTest(nbc_teammates, hugme_manual_teammates$data, "performance")

wilcoxTest(nbc_sweethome3d, hugme_sweethome3d$data, "performance")
wilcoxTest(nbc_sweethome3d, hugme_manual_sweethome3d$data, "performance")


wilcoxTest(nbc_jabref, hugme_jabref$data, "f1")
wilcoxTest(nbc_jabref, hugme_manual_jabref$data, "f1")

wilcoxTest(nbc_ant, hugme_ant$data, "f1")
wilcoxTest(nbc_ant, hugme_manual_ant$data, "f1")

wilcoxTest(nbc_argouml, hugme_argouml$data, "f1")
wilcoxTest(nbc_argouml, hugme_manual_argouml$data, "f1")

wilcoxTest(nbc_lucene, hugme_lucene$data, "f1")
wilcoxTest(nbc_lucene, hugme_manual_lucene$data, "f1")

wilcoxTest(nbc_teammates, hugme_teammates$data, "f1")
wilcoxTest(nbc_teammates, hugme_manual_teammates$data, "f1")

wilcoxTest(nbc_sweethome3d, hugme_sweethome3d$data, "f1")
wilcoxTest(nbc_sweethome3d, hugme_manual_sweethome3d$data, "f1")




# fun and tests
hist(nbc_jabref$recall)

summary(nbc_jabref)

# this is to compare with the large evo changes of Improving Automated Mapping in Reflexion Models Using Information Retrieval Techniques
getMeanF1LargeEvoChange <- function(a_data) {
	mean(a_data[a_data$initialClustered > (a_data$totalMapped - 100) & a_data$initialClustered < (a_data$totalMapped - 10),]$f1)
}

getMeanF1LargeEvoChange(nbc_jabref)
getMeanF1LargeEvoChange(nbc_ant)
getMeanF1LargeEvoChange(nbc_argouml)
getMeanF1LargeEvoChange(nbc_lucene)
getMeanF1LargeEvoChange(nbc_teammates)
getMeanF1LargeEvoChange(nbc_sweethome3d)

