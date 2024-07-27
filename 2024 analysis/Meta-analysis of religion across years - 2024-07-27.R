# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author: Zehan Yang, PhD. zehan.yang@uconn.edu. You may also contact rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-07-27

# This file uses the datafile:
# "data_trusts.csv" or "sts_has.csv"

### Start =======================================
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
(current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))

##* Functions -----
function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())))
  for(package_name in packages)
  {
    #library(package_name,character.only=TRUE,quietly=TRUE);
    library(package_name,character.only=TRUE, quietly = FALSE);
  }
}
function_plot_print <- function (plotname, plotwidth, plotheight){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  (current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}
##* Essential packages -----
packages_essential <- c("tcltk",'stringr','openxlsx','grid','plyr')
function_libraries_install(packages_essential)

packages_zehan <- (c("betareg", "readr", "metafor", "meta"))
function_libraries_install(packages_zehan)

# Load the dataset
#dat <- read_csv("sts_has.csv")
#dat <- read_csv("data_trusts.csv") # thIS matches the regression
### Data grab (RATES) ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter      <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename        <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension  <- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
if (file.extension == 'csv'){
  dat   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  dat   <- read.xlsx(filename)}

### Data cleaning ===============================
# To match the regressions, use the file "data_trusts.csv"
##* ReName columns depending on which file loaded?---------------------
if (grepl("sts_has.csv", filename)) {
  names(dat)[names(dat) == "Religion"] <- "religion"
  names(dat)[names(dat) == "Year"] <- "year"
  names(dat)[names(dat) == "stress"] <- "stress.rate"
  names(dat)[names(dat) == "stress_base"] <- "stress.respondents"
} else if (grepl("data_trusts.csv", filename)) {
  names(dat)[names(dat) == "stress.stress"] <- "stress.rate"
  names(dat)[names(dat) == "stress.stress.base"] <- "stress.respondents"
} else {
  print("The filename does not contain the specified strings")
}

dat$stress.numerator <- round(dat$stress.respondents * dat$stress.rate,0)

#** Parameters: subgroup by years? ----
subgroup.by.years <- tk_select.list(c('No','Yes'), preselect = 'No', multiple = FALSE,
                                title = "\n\nSubgroup results by years?\n\n")
if (subgroup.by.years == 'Yes'){
  dat.religions <- ddply(dat, .(year), plyr::summarize, 
                         numerator = sum(stress.numerator), 
                         respondents = sum(stress.respondents))
  meta1 <- metaprop(numerator, respondents, byvar = year,  data=dat.religions,  verbose=FALSE, fixed=FALSE, method="GLMM", hakn=FALSE, title = "Random effects analkyss of work stress by religion")
  forest(meta1, addrows.below.overall = 1)
  # Plot the forest plot with only subgroup totals and the overall total
  # Suppress individual study rows and show only subgroup and overall totals
  forest(meta1)
  dat.religions <- ddply(dat, .(religion, year), plyr::summarize, 
                       numerator = sum(stress.numerator), 
                       respondents = sum(stress.respondents))
  meta1 <- metaprop(numerator, respondents, byvar = year,  data=dat.religions,  verbose=FALSE, fixed=FALSE, method="GLMM", hakn=FALSE, title = "Random effects analkyss of work stress by religion")
  (summary(meta1))
  # Plot the forest plot with only subgroup totals and the overall total
  # Suppress individual study rows and show only subgroup and overall totals
  forest(meta1, studlab = religion, addrows.below.overall = 1)
  #Footer
  grid.text('Notes:', 0.08, 0.04, hjust=0, gp=gpar(cex=1, font=2))
  Footer <- paste0("1. Filename: ",  basename(filename))
  grid.text(Footer,   0.08, 0.02, hjust=0, gp=gpar(cex=1))  #grid.text(Footer, 0.10, 0.075, hjust = 0, gp=gpar(cex=1))
  function_plot_print(paste0('Forest-plot_', basename(filename)), 1000,1000)
}else{
  dat.religions <- ddply(dat, .(religion), plyr::summarize, 
                         numerator = sum(stress.numerator), 
                         respondents = sum(stress.respondents))
  meta1 <- metaprop(numerator, respondents, studlab = religion, byvar = NULL,  data=dat.religions,  verbose=FALSE, fixed=FALSE, method="GLMM", hakn=FALSE, title = "Random effects analkyss of work stress by religion")
  (summary(meta1))
  forest(meta1)
  #Footer
  grid.text('Notes:', 0.08, 0.08, hjust=0, gp=gpar(cex=1, font=2))
  Footer <- paste0("1. Filename: ",  basename(filename))
  grid.text(Footer,   0.08, 0.04, hjust=0, gp=gpar(cex=1))  #grid.text(Footer, 0.10, 0.075, hjust = 0, gp=gpar(cex=1))
  function_plot_print(paste0('Forest-plot_', basename(filename)), 1000,400)
}


