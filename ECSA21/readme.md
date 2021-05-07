This is the replication package for mapping experiments using CountAttract with dependency weights

the genetic_optimization folder contains the output of the genetic algorithm optimizations. The output also shows the parameters used when running the genetic optimizer: g3n3z-1.3.jar. Please note that to run the optimizer the actual systems are also needed. These cannot however be distributed in this package due to licensing and size requirements. Please contact the authors for access to the systems used.

The data folder contains the final data as obtained by running the 50 000 instance of the experiments per system and weight the experiment is defined in ecsa_mega_13.xml and can be run using cmdexrunner-1.6.jar. Please note that to run the experiments the actual systems are also needed. These cannot however be distributed in this package due to licensing and size requirements. Please contact the authors for access to the systems used.

The main_script.r contains the r-script for analyzing the data and generating the statistical analysis and images (comparison.csv, f1_comparison_boxplots.eps, f1_comparison_plots.eps).

If you have any questions please contact:
Tobias Olsson, tobias.olsson@lnu.se