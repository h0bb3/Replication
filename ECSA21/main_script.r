rm(list=ls(all=TRUE))
library(coin)
library(caTools)
#install.packages("export")
library(export)

# please set the working directory accordingly
setwd("C:/hObbE/projects/coding/research/s4rdm3x/r-scripts/ECSA2021/")

loadData <- function(a_csvFile, algorithm) {
	data <- read.csv(a_csvFile, head=TRUE, sep="\t")

	data <- data[data$initialClustered < data$totalMapped & data$algorithm == algorithm, ]

	truepositives = data$totalAutoClustered - data$totalAutoWrong
	falsenegatives = data$totalMapped - data$initialClustered - data$totalManuallyClustered - data$totalAutoClustered

	data$precision = ifelse(data$totalAutoClustered == 0, 0, (truepositives) / (data$totalAutoClustered))
	data$recall = ifelse(data$totalMapped - data$initialClustered - data$totalManuallyClustered == 0, 0, (truepositives) / (truepositives + falsenegatives))
	
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
	#y_lag <- runquantile(data$thedata, 301, probs=c(0.5), align="center")

	lines(data$mappingPercent, y_lag, col=a_color, lty=a_lineStyle)
}

plotRunningQuantiles <- function(a_data, a_color, a_column) {
	
	data <- data.frame(a_data[[a_column]], a_data$mappingPercent)
	names(data)[1] = "thedata"
	names(data)[2] = "mappingPercent"
	data <- data[order(data$mappingPercent),]

	y = runquantile(data$thedata, 1001, probs=c(0.25, 0.75), align="center")
	#y = runquantile(data$thedata, 301, probs=c(0.25, 0.75), align="center")

	#lines(data$mappingPercent, y[,1], col=a_color)
	#lines(data$mappingPercent, y[,2], col=a_color)

	polygon(c(data$mappingPercent, rev(data$mappingPercent)),c(y[,1], rev(y[,2])), col=a_color, border=NA)
}

plotRunningData <- function(a_data1, a_data2, a_data3, a_title, a_column, a_legendPos) {
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


	plotRunningQuantiles(a_data1, c1_a, a_column)
	plotRunningQuantiles(a_data2, c2_a, a_column)
	plotRunningQuantiles(a_data3, c3_a, a_column)


	plotRunningMedian(a_data1, c1, a_column, "solid")
	plotRunningMedian(a_data2, c2, a_column, "solid")
	plotRunningMedian(a_data3, c3, a_column, "solid")


	if (a_legendPos != "none") {
		legend(a_legendPos, legend=c("Uniform", "Intuitive", "Otimized"),col=c(c1, c2, c3), lty=1)
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
		ant_uniform$data[[a_column]], ant_intuitive$data[[a_column]], ant_optimized$data[[a_column]],
		argouml_uniform$data[[a_column]], argouml_intuitive$data[[a_column]], argouml_optimized$data[[a_column]],
		cmnimg_uniform$data[[a_column]], cmnimg_intuitive$data[[a_column]], cmnimg_optimized$data[[a_column]],
		jabref_uniform$data[[a_column]], jabref_intuitive$data[[a_column]], jabref_optimized$data[[a_column]],
		lucene_uniform$data[[a_column]], lucene_intuitive$data[[a_column]], lucene_optimized$data[[a_column]],
		prom_uniform$data[[a_column]], prom_intuitive$data[[a_column]], prom_optimized$data[[a_column]],
		sh3d_uniform$data[[a_column]], sh3d_intuitive$data[[a_column]], sh3d_optimized$data[[a_column]],
		teammates_uniform$data[[a_column]], teammates_intuitive$data[[a_column]], teammates_optimized$data[[a_column]],


		main = a_title,
		at = rev(c(1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 27, 29, 30, 31)),
		names = c("Ant", "", "", "A.UML", "", "", "C.Img.", "", "", "JabRef", "", "", "Lucene", "", "", "ProM", "", "", "S.H.3D", "", "", "T.Mates", "", ""),
		las = 2,
		col = c(rgb(114, 147, 203, max = 255),rgb(225, 151, 76, max = 255), rgb(132, 186, 91, max = 255)),
		border = "black",
		horizontal = TRUE,
		notch = FALSE,
		outpch = 46		# outlier ASCII for .
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


comparisonTest <- function(a_systemName, a_uniform, a_intuitive, a_optimized) {
	opt_int <- wilcoxTest(a_optimized, a_intuitive, "f1")
	opt_int$system = a_systemName
	opt_int$comparison = "Opt. v.s. Int."

	opt_uni <- wilcoxTest(a_optimized, a_uniform, "f1")
	opt_uni$system = a_systemName
	opt_uni$comparison = "Opt. v.s. Uni."

	int_uni <- wilcoxTest(a_intuitive, a_uniform, "f1")
	int_uni$system = a_systemName
	int_uni$comparison = "Int. v.s. Uni"


	rbind(opt_int, rbind(opt_uni, int_uni))
}

getComparisonStat <- function(a_comparisonTestData, a_statCol) {

	ret = list()
	ret$system = a_comparisonTestData['opt_int',]$system
	ret$stat = a_statCol
	ret$opt_int = a_comparisonTestData['opt_int', a_statCol][[1]]
	ret$opt_uni = a_comparisonTestData['opt_uni', a_statCol][[1]]
	ret$int_uni = a_comparisonTestData['int_uni', a_statCol][[1]]

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


# ant
ant_uniform_loaded <- loadData("data/ant.csv", "HuGMe:uniform")
ant_uniform <- getFilteredDataEx(0.1, ant_uniform_loaded)
ant_intuitive_loaded <- loadData("data/ant.csv", "HuGMe:intuitive")
ant_intuitive<- getFilteredDataEx(0.1, ant_intuitive_loaded)
ant_optimized_loaded <- loadData("data/ant.csv", "HuGMe:optimized")
ant_optimized<- getFilteredDataEx(0.1, ant_optimized_loaded )

nrow(ant_uniform$data)
nrow(ant_intuitive$data)
nrow(ant_optimized$data)

boxplot(ant_uniform$data$f1, ant_intuitive$data$f1, ant_optimized$data$f1)
plotRunningData(ant_uniform$data, ant_intuitive$data, ant_optimized$data, "Ant F1 Comparison", "f1", "bottomright")


ant_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("Ant", ant_uniform$data, ant_intuitive$data, ant_optimized$data))
ant_comparisonTest

# argouml
argouml_uniform_loaded <- loadData("data/argouml.csv", "HuGMe:uniform")
argouml_uniform <- getFilteredDataEx(0.1, argouml_uniform_loaded)
argouml_intuitive_loaded <- loadData("data/argouml.csv", "HuGMe:intuitive")
argouml_intuitive<- getFilteredDataEx(0.1, argouml_intuitive_loaded)
argouml_optimized_loaded <- loadData("data/argouml.csv", "HuGMe:optimized")
argouml_optimized<- getFilteredDataEx(0.1, argouml_optimized_loaded )

nrow(argouml_uniform$data)
nrow(argouml_intuitive$data)
nrow(argouml_optimized$data)

boxplot(argouml_uniform$data$f1, argouml_intuitive$data$f1, argouml_optimized$data$f1)
plotRunningData(argouml_uniform$data, argouml_intuitive$data, argouml_optimized$data, "ArgoUML F1 Comparison", "f1", "bottomright")

argouml_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("ArgoUML", argouml_uniform$data, argouml_intuitive$data, argouml_optimized$data))
argouml_comparisonTest

# commons imaging
cmnimg_uniform_loaded <- loadData("data/cmnimg.csv", "HuGMe:uniform")
cmnimg_uniform <- getFilteredDataEx(0.1, cmnimg_uniform_loaded)
cmnimg_intuitive_loaded <- loadData("data/cmnimg.csv", "HuGMe:intuitive")
cmnimg_intuitive<- getFilteredDataEx(0.1, cmnimg_intuitive_loaded)
cmnimg_optimized_loaded <- loadData("data/cmnimg.csv", "HuGMe:optimized")
cmnimg_optimized<- getFilteredDataEx(0.1, cmnimg_optimized_loaded )

nrow(cmnimg_uniform$data)
nrow(cmnimg_intuitive$data)
nrow(cmnimg_optimized$data)

boxplot(cmnimg_uniform$data$f1, cmnimg_intuitive$data$f1, cmnimg_optimized$data$f1)
plotRunningData(cmnimg_uniform$data, cmnimg_intuitive$data, cmnimg_optimized$data, "Commons Imaging F1 Comparison", "f1", "bottomright")

cmnimg_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("Commons Imaging", cmnimg_uniform$data, cmnimg_intuitive$data, cmnimg_optimized$data))
cmnimg_comparisonTest

# jabref
jabref_uniform_loaded <- loadData("data/jabref.csv", "HuGMe:uniform")
jabref_uniform <- getFilteredDataEx(0.1, jabref_uniform_loaded)
jabref_intuitive_loaded <- loadData("data/jabref.csv", "HuGMe:intuitive")
jabref_intuitive<- getFilteredDataEx(0.1, jabref_intuitive_loaded)
jabref_optimized_loaded <- loadData("data/jabref.csv", "HuGMe:optimized")
jabref_optimized<- getFilteredDataEx(0.1, jabref_optimized_loaded )

nrow(jabref_uniform$data)
nrow(jabref_intuitive$data)
nrow(jabref_optimized$data)

boxplot(jabref_uniform$data$f1, jabref_intuitive$data$f1, jabref_optimized$data$f1)
plotRunningData(jabref_uniform$data, jabref_intuitive$data, jabref_optimized$data, "JabRef F1 Comparison", "f1", "bottomright")

jabref_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("JabRef", jabref_uniform$data, jabref_intuitive$data, jabref_optimized$data))
jabref_comparisonTest

# lucene
lucene_uniform_loaded <- loadData("data/lucene.csv", "HuGMe:uniform")
lucene_uniform <- getFilteredDataEx(0.1, lucene_uniform_loaded)
lucene_intuitive_loaded <- loadData("data/lucene.csv", "HuGMe:intuitive")
lucene_intuitive<- getFilteredDataEx(0.1, lucene_intuitive_loaded)
lucene_optimized_loaded <- loadData("data/lucene.csv", "HuGMe:optimized")
lucene_optimized<- getFilteredDataEx(0.1, lucene_optimized_loaded )

nrow(lucene_uniform$data)
nrow(lucene_intuitive$data)
nrow(lucene_optimized$data)

boxplot(lucene_uniform$data$f1, lucene_intuitive$data$f1, lucene_optimized$data$f1)
plotRunningData(lucene_uniform$data, lucene_intuitive$data, lucene_optimized$data, "Lucene F1 Comparison", "f1", "bottomright")

lucene_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("Lucene", lucene_uniform$data, lucene_intuitive$data, lucene_optimized$data))
lucene_comparisonTest


# prom
prom_uniform_loaded <- loadData("data/prom.csv", "HuGMe:uniform")
prom_uniform <- getFilteredDataEx(0.1, prom_uniform_loaded)
prom_intuitive_loaded <- loadData("data/prom.csv", "HuGMe:intuitive")
prom_intuitive<- getFilteredDataEx(0.1, prom_intuitive_loaded)
prom_optimized_loaded <- loadData("data/prom.csv", "HuGMe:optimized")
prom_optimized<- getFilteredDataEx(0.1, prom_optimized_loaded )

nrow(prom_uniform$data)
nrow(prom_intuitive$data)
nrow(prom_optimized$data)

boxplot(prom_uniform$data$f1, prom_intuitive$data$f1, prom_optimized$data$f1)
plotRunningData(prom_uniform$data, prom_intuitive$data, prom_optimized$data, "ProM F1 Comparison", "f1", "bottomright")

prom_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("ProM", prom_uniform$data, prom_intuitive$data, prom_optimized$data))
prom_comparisonTest

# sweet home 3d
sh3d_uniform_loaded <- loadData("data/sweethome3d.csv", "HuGMe:uniform")
sh3d_uniform <- getFilteredDataEx(0.1, sh3d_uniform_loaded)
sh3d_intuitive_loaded <- loadData("data/sweethome3d.csv", "HuGMe:intuitive")
sh3d_intuitive<- getFilteredDataEx(0.1, sh3d_intuitive_loaded)
sh3d_optimized_loaded <- loadData("data/sweethome3d.csv", "HuGMe:optimized")
sh3d_optimized<- getFilteredDataEx(0.1, sh3d_optimized_loaded )

nrow(sh3d_uniform$data)
nrow(sh3d_intuitive$data)
nrow(sh3d_optimized$data)

boxplot(sh3d_uniform$data$f1, sh3d_intuitive$data$f1, sh3d_optimized$data$f1)
plotRunningData(sh3d_uniform$data, sh3d_intuitive$data, sh3d_optimized$data, "Sweet Home 3D F1 Comparison", "f1", "bottomright")

sh3d_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("Sweet Home 3D", sh3d_uniform$data, sh3d_intuitive$data, sh3d_optimized$data))
sh3d_comparisonTest

# teammates
teammates_uniform_loaded <- loadData("data/teammates.csv", "HuGMe:uniform")
teammates_uniform <- getFilteredDataEx(0.1, teammates_uniform_loaded)
teammates_intuitive_loaded <- loadData("data/teammates.csv", "HuGMe:intuitive")
teammates_intuitive<- getFilteredDataEx(0.1, teammates_intuitive_loaded)
teammates_optimized_loaded <- loadData("data/teammates.csv", "HuGMe:optimized")
teammates_optimized<- getFilteredDataEx(0.1, teammates_optimized_loaded )

nrow(teammates_uniform$data)
nrow(teammates_intuitive$data)
nrow(teammates_optimized$data)

boxplot(teammates_uniform$data$f1, teammates_intuitive$data$f1, teammates_optimized$data$f1)
plotRunningData(teammates_uniform$data, teammates_intuitive$data, teammates_optimized$data, "Teammates F1 Comparison", "f1", "bottomright")

teammates_comparisonTest = getAllComparisonStatsAsRows(comparisonTest("Teammates", teammates_uniform$data, teammates_intuitive$data, teammates_optimized$data))
teammates_comparisonTest


# compile the comparison data for saving
comparisonTable <- ant_comparisonTest
comparisonTable <- rbind(comparisonTable, argouml_comparisonTest)
comparisonTable <- rbind(comparisonTable, cmnimg_comparisonTest)
comparisonTable <- rbind(comparisonTable, jabref_comparisonTest)
comparisonTable <- rbind(comparisonTable, lucene_comparisonTest)
comparisonTable <- rbind(comparisonTable, prom_comparisonTest)
comparisonTable <- rbind(comparisonTable, sh3d_comparisonTest)
comparisonTable <- rbind(comparisonTable, teammates_comparisonTest)

comparisonTable
write.table(comparisonTable, "comparison.csv", sep = "\t", row.names=FALSE)

# images for article
plotBoxPlotsAllData("f1", "Dependency Weights F1 Score Comparisons")
mtext(text="F1 Score",side=1,line=2,outer=FALSE)
graph2eps(file="f1_comparison_boxplots.eps", bg = "transparent", fallback_resolution = 600)


windows(height = 11.0, width = 8)
oldPar = par(oma=c(2,2,0,0),mar=c(2,2,2,1), mfrow=c(4,2))
#oldPar = par(fig=c(0.075,0.365,0.55,1.0))
plotRunningData(ant_uniform$data, ant_intuitive$data, ant_optimized$data, "Ant", "f1", "none")
lc1 <- rgb(57, 106, 177, max = 255, alpha = 255)
lc2 <- rgb(218, 124, 48, max = 255, alpha = 255)
lc3 <- rgb(62, 150, 81, max = 255, alpha = 255)
lc4 <- rgb(204, 37, 41, max = 255, alpha = 255)

legend("topleft", legend=c("Uniform", "Intuitive", "Optimized"), lty=1, col=c(lc1, lc2, lc3))

axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(argouml_uniform$data, argouml_intuitive$data, argouml_optimized$data, "ArgoUML", "f1", "none")

plotRunningData(cmnimg_uniform$data, cmnimg_intuitive$data, cmnimg_optimized$data, "Commons Imaging", "f1", "none")

axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(jabref_uniform$data, jabref_intuitive$data, jabref_optimized$data, "JabRef", "f1", "none")

plotRunningData(lucene_uniform$data, lucene_intuitive$data, lucene_optimized$data, "Lucene", "f1", "none")
axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(prom_uniform$data, prom_intuitive$data, prom_optimized$data, "ProM", "f1", "none")


plotRunningData(sh3d_uniform$data, sh3d_intuitive$data, sh3d_optimized$data, "Sweet Home 3D", "f1", "none")

axis(side = 2, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), tck=-.01)
plotRunningData(teammates_uniform$data, teammates_intuitive$data, teammates_optimized$data, "Teammates", "f1", "none")


mtext(text="Initial Mapping Size",side=1,line=0,outer=TRUE)
mtext(text="F1 Score",side=2,line=0,outer=TRUE)

graph2eps(file="f1_comparison_plots.eps", bg = "transparent", fallback_resolution = 600)



