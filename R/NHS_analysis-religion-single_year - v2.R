#This file is best used within R Studio
# rbadgett@kumc.edu
### Start =======================================
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
#Directory <- getwd()
getwd()
##* Rtools ------
##* # https://cran.rstudio.com/bin/windows/Rtools/rtools40.html
packageVersion("rtools")
write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
Sys.which("make")
packageVersion("rtools")

##* Functions -----
function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())))
  for(package_name in packages)
  {
    #library(package_name,character.only=TRUE,quietly=TRUE);
    library(package_name,character.only=TRUE, quietly = FALSE);
  }
}
function_plot_print <- function (plotname, plotheight, plotwidth){
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}

##* Packages, essential -----
packages_essential <- c("tcltk",'stringr','openxlsx')
function_libraries_install(packages_essential)

##* Everybody awake? -----
hour <- as.numeric(str_sub(Sys.time(), 12, 13))
daypart <- NULL
if (hour > 17) {
  daypart <- 'evening'
} else if ( hour > 11) {
  daypart <- 'afternoon'
} else if ( hour > 5) {
  daypart <- 'morning'
} else {
  daypart <- 'madrugada'
}
tk_messageBox(type = "ok", paste('1. ', 'Working directory:\n', getwd(), sepo=''), caption = paste("Hello and good",daypart))

##* Graphics --------------------------
#windows(600, 600, pointsize = 12) # Size 600x600
devAskNewPage(ask = FALSE)
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
plot.new()
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + 1.2*strheight("A")
ymax <- par("usr")[4] - strheight("A")

#*** Meta-analysis and positive deviance----
packages_meta <- c("metafor", #
                   'meta',   # Meta-analysis
                   'boot',   # inv.logit to identify deviants
                   'grid',   # Forest and blobbogram
                   'gemtc'   # Blobbogram
)
function_libraries_install(packages_meta)

##* Packages/libraries, other ------------------
library(plyr)

##* Constants declaration -------------------
`%notin%` <- Negate(`%in%`)
LF <- "\n"
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
KUCrimson = "#e8000d"
KUYellow = "#ffc82d"
current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
(current.date.pretty <- as.character(strftime (Sys.time(), format="%m/%d/%Y", tz="", usetz=FALSE)))
p.value <- sprintf(p.value, fmt='%#.3f')
I2.label <- expression(paste( plain("I") ^ plain("2"), plain("(%)")))
summary.I2.label <- bquote(paste("RE Model (I^2 = ",(formatC(res$I2, digits=1, format="f")), "%)"))

##* Encoding characters---------------
# https://www.unicodepedia.com/groups/
# http://www.unicode.org/charts/
##* Footnotes
# https://www.unicodepedia.com/groups/general-punctuation/
# Dagger  \u2020
# Double dagger  \u2021
# Section symbol \u00A7
# Double Vertical Line \u2016
# Para    \B6 or \u0086
##*Greek
# https://unicode.org/charts/nameslist/n_0370.html
##* Troubleshooting grab -----------------------
options(error=NULL)
# msgbox for troubleshooting: 
# tk_messageBox(type = "ok", paste(current.state,': ', nrow(data.state.current),sepo=''), caption = "Troubleshooting")
# browser()
# Finish, quit is c or Q
# enter f press enter and then enter Q

### Data grab ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
##* use Religions and burnout - groups (rates only) - 2018.xlsx
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension<- substr(filename, nchar(filename) - 2, nchar(filename))
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename)}
data.temp <- data.import
# Remove empty lines below the data in CSV files
data.temp <- data.temp[complete.cases(data.temp), ]

##* ReName columns?------------------------------
colnames(data.temp)[1] <- "Religion"

##* Conversions---------------------------------
cols <- c(8,36)
for (i in cols[1]:cols[2])
  {
  #Convert from factor to character
  #data.temp[i] <- lapply(data.temp[,i], as.character) # Not needed if file read with stringsAsFactors=FALSE
  #Remove commas
  data.temp[i] <- gsub(",", "",data.temp[,i], fixed = TRUE)
  # Convert to numbers
  data.temp[i] <- lapply(data.temp[i],as.numeric)
  }

##* ReMove rows or anyone? (left of comma -------
#DELETE WITHHELD???
#data.temp <- data.temp[data.temp$Religion != 'Withheld',]
#data.temp <- data.temp[!data.temp$Religion == 'Withheld',]
#data.temp <- data.temp[!data.temp$Religion == 'None',]

# Does total responses and rows check ok?
sum(data.temp$Total)
(nrow(data.temp))

### METAPROP -------------------------------
# Burnout (unwell)
titletext = "Burnout and religion in the NHS, 2018"
titletext = "Burnout, religion, and harassment by patients/public in the NHS, 2018"
titletext = "Burnout, religion, and harassment by colleagues in the NHS, 2018"
plotname  ="Forest plot - religions and burnout.png"
plotname  ="Forest plot - religions, burnout, harassment.png"
plotheight = 700
# method="GLMM" is assumed if not declared
meta1 <- metaprop(Burned.Out_Often.Always,Total, studlab = Religion, data=data.temp, byvar = Group, method="GLMM", comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")
# Engagement (Absorption)
titletext = ""
meta1 <- metaprop(Absorption_Often.Always,Total_A, studlab = Religion, data=data.temp, byvar = Group, comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")
# Engagement (Vigor)
titletext = ""
meta1 <- metaprop(Vigor_Often.Always,Total_V, studlab = Religion, data=data.temp, byvar = Group, comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")
# Engagement (Dedication)
titletext = ""
meta1 <- metaprop(Dedication_Often.Always,Total_D, studlab = Religion, data=data.temp, byvar = Group, comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")
# Harassment by patients/public
titletext = "Religion and harassment by patients/public in the NHS, 2018"
plotname = "Forest plot - religions and harassment by patients-public.png"
plotheight = 725
meta1 <- metaprop(data.temp$Harassment_patients,data.temp$Total_Harassment_patients, studlab = Religion, data=data.temp, byvar = Group, comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")
# Harassment by colleagues
titletext = "Religion and harassment by colleagues in the NHS, 2018"
plotname = "Forest plot - religions and harassment by colleagues.png"
plotheight = 725
meta1 <- metaprop(data.temp$Harassment_colleagues ,data.temp$Total_Harassment_colleagues, studlab = Religion, data=data.temp, byvar = Group, comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")
# Discrimination, ethnic
# Note that denom is discrim from patients/public since that was the bigger number and the type of discrim was based on either patients/public or colleagues
titletext = "Religion and ethnic discrimination in the NHS, 2018"
plotname = "Forest plot - religious and ethnic discrimination.png"
plotheight = 725

summary(meta1)
meta1 <- metaprop(data.temp$Discrimination_ethnic, data.temp$Total, studlab = Religion, data=data.temp, byvar = Group, comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")
# Discrimination, religious
# Note that denom is discrim from patients/public since that was the bigger number and the type of discrim was based on either patients/public or colleagues
titletext = "Religion and religious discrimination in the NHS, 2018"
plotname = "Forest plot - religious and religious discrimination.png"
plotheight = 725
meta1 <- metaprop(data.temp$Discrimination_religious, data.temp$Total, studlab = Religion, data=data.temp, byvar = Group, comb.fixed=FALSE,hakn=TRUE, title = "Rates of burnout")

# Labeling deviants
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  if (meta1$lower[i]>inv.logit(meta1$TE.random)){
    #meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");
    positive.deviants <- positive.deviants + 1
    }
  if (meta1$upper[i]<inv.logit(meta1$TE.random)){
    #meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");
    negative.deviants <- negative.deviants + 1
    }
}
(positive.deviants)
(negative.deviants)
##** Export plot------------------------
meta1$data$Total <- format(meta1$data$Total,big.mark=",",scientific=FALSE)
meta::forest(meta1, # Size: 1000 x 4000
  #leftlabs=c("Religion","Burned out","Members"), leftcols=c("studlab","Burned.Out_Often.Always","Total"),
  #leftlabs=c("Religion","Harassment\nrate","Members"), leftcols=c("studlab",'Harassment_patients_rate',"Total"),
  #leftlabs=c("Religion","Discriminated","Members"), leftcols=c("studlab","Discrimination_religious","Total"),
  leftlabs=c("Religion","Members"), leftcols=c("studlab","Total"),
  ref = NA, digits.addcols.left = 2, just.addcols.left = "center", colgap.left = "4mm",
  #rightlabs = c("Burnout\nrate",'95% CI'), xlab='Burnout rate',
  #rightlabs = c("Harassment\nrate",'95% CI'), xlab='Harassment rate',
  rightlabs = c("Discrimination\nrate",'95% CI'), xlab='Discrimination rate',
  subgroup = TRUE, test.subgroup.random = TRUE, print.Q.subgroup = FALSE, overall = TRUE, print.tau2 = FALSE, print.Q = FALSE, print.pval.Q = FALSE, print.subgroup.labels = TRUE, print.byvar = FALSE, col.diamond = "blue", xlim = c(0,1)) # Size: 800 x 1300
grid.text(titletext, 0.5, 0.95, gp=gpar(cex=1.4))
if (grepl('urnout', titletext, fixed=TRUE) == TRUE){
  grid.text("Burnout = 'During the last 12 months have you felt unwell as a result of work related stress'", 0.4, 0.03, gp=gpar(cex=1))
  }
rstudioapi::savePlotAsImage( # Print at 800*plotheight
  plotname,
  format = "png", width = 800, height = plotheight)

# Alternative method
# Meta-analysis
dat <- escalc(measure="PR", xi=Burned.Out_Often.Always, ni=Total, data=data.temp)
dat <- escalc(measure="PR", xi=Discrimination_religious, ni=Total_Discrimination_religious, data=data.temp)
res <- rma(yi, vi, method = "DL", data=dat, knha = TRUE)
summary(res)# res also can be used in the escalc metaregression below)
meta::forest(res,main=titletext) # Size: 800 x 1300

# Metaregression (preferred method)
## Patients-public
titletext = "Burnout, religion, and harassment by patients/public in the NHS, 2018"
data.temp$Cofactor <- data.temp$Harassment_patients_rate
xlabel = 'Proportion reporting harassment by patients/public'
plotname = "Metaregression of burnout by harassment by patients-public.png"
xlim=c(0.1,0.35)
# Colleagues
titletext = "Burnout, religion, and harassment by colleagues in the NHS, 2018"
data.temp$Cofactor <- data.temp$Harassment_colleagues_rate
xlabel = 'Proportion reporting harassment by colleagues'
plotname = "Metaregression of burnout by harassment by colleagues.png"
xlim=c(0.1,0.35)
# Discrimination, religious
titletext = "Burnout, religion, and religious discrimination in the NHS, 2018"
data.temp$Cofactor <- data.temp$Discrimination_religious_rate.adjusted.
xlabel = 'Proportion reporting religious discrimination'
plotname = "Metaregression of burnout by religious discrimination.png"
xlim=c(0.0,0.1)
# Discrimination, ethnic
titletext = "Burnout, religion, and ethnic discrimination in the NHS, 2018"
data.temp$Cofactor <- data.temp$Discrimination_ethnic_rate.adjusted.
xlabel = 'Proportion reporting ethnic discrimination'
plotname = "Metaregression of burnout by ethnic discrimination.png"
xlim=c(0.0,0.2)
#  Calculate, after running one of the sections above: Patients, Colleagues, Discrimination
dat <- escalc(measure="PR", xi=data.temp$Burned.Out_Often.Always, ni=data.temp$Total, data=data.temp)
dat <- dat[order(dat$yi),]
res <- rma.uni(yi, vi, mods = ~ dat$Cofactor, data=dat, method = "DL", knha = TRUE)
summary(res)
# https://stat.ethz.ch/pipermail/r-help/2011-January/265498.html
dat$Cofactor.std <- c(scale(dat$Cofactor))
res.std <- rma(yi, vi, mods= ~ Cofactor.std, data=dat, method="REML")
summary(res.std)
res$beta[2]
res.std$beta[2]
wi    <- 1/sqrt(dat$vi)
size  <- 0.5 + 2.0 * (wi - min(wi))/(max(wi) - min(wi))
margins.default <- par("mar")
labels <- substring(dat$Religion,1,1)
labels[1] = dat$Religion[1] # Hindu
labels[5] = dat$Religion[5] # None
labels[8] = dat$Religion[8] # Other
labels[9] = dat$Religion[9] # Withheld
pos    <- c(1,rep(4,7),3)
colors <- c(rep('black',9))
colors[5] <- 'darkred'
colors[1] <- 'darkgreen'
par(oma=c(5,1,0,0))
plot(dat$Cofactor, dat$yi, pch=19, cex=size, 
     xlab=xlabel, ylab="", main=titletext, 
     xlim=xlim, ylim=c(0.1,1),
     las=1, bty="l", col=colors)
mtext("Proportion reporting burnout", side = 2, line = 3.5)
## dotted line at overall rate
abline(h=0.39, lty="dotted", col="gray")
if (grepl('olleagues', titletext, fixed=TRUE) == TRUE){
  text  (par("usr")[2],0.39-1.4*strheight("A"),cex=1,adj=c(1,0),"Overall rate of burnout: 39%", font=1)
  text  (0.20,par("usr")[4]-3*strheight("A"),cex=1,adj=c(1,0),"Overall rate of\nharassment: 20%", font=1)
  abline(v=0.20, lty="dotted", col="gray")
  labels[c(2,3)] <- 'I,S'
  pos[c(2,3,5)] <- 1
  pos[9] <- 3
  cofactor.range = seq(0.15, 0.25, 0.01) #dat$Cofactor 
}else if (grepl('atients', titletext, fixed=TRUE) == TRUE){
  text  (par("usr")[1],0.39-1.4*strheight("A"),cex=1,adj=c(0,0),"Overall rate of burnout: 39%", font=1)
  text  (0.25,par("usr")[4]-3*strheight("A"),cex=1,adj=c(1,0),"Overall rate of\nharassment: 25%", font=1)
  abline(v=0.25, lty="dotted", col="gray")
  pos[5] <- 2
  cofactor.range = seq(0.2, 0.35, 0.01) #dat$Cofactor 
}else if (grepl('ethnic', titletext, fixed=TRUE) == TRUE){
  text  (par("usr")[2],0.39+strheight("A"),cex=1,adj=c(1,0),"Overall rate of burnout: 39%", font=1)
  text  (0.07,par("usr")[3]+0.5*strheight("A"),cex=1,adj=c(0,0),"Overall rate of\ndiscrimination: 7%", font=1)
  abline(v=0.07, lty="dotted", col="gray")
  cofactor.range = seq(0, 0.17, 0.01) #dat$Cofactor 
}else{ # Religious discrimination
  text  (par("usr")[2],0.39+strheight("A"),cex=1,adj=c(1,0),"Overall rate of burnout: 39%", font=1)
  text  (0.01,par("usr")[3]+0.5*strheight("A"),cex=1,adj=c(0,0),"Overall rate of\ndiscrimination: 1%", font=1)
  abline(v=0.01, lty="dotted", col="gray")
  pos[5] <- 2
  cofactor.range = seq(0, 0.08, 0.01) #dat$Cofactor 
}
text(dat$Cofactor, dat$yi,labels, adj=c(0.5,0.5), pos=pos,col=colors) # +1.8*strwidth("A")
#Regression line
#abline(res$beta[1],res$beta[2]) # This should overlay the predicted line below
preds <- predict(res, newmods=cofactor.range)
preds <- data.frame(cofactor.range,preds$pred,preds$ci.lb,preds$ci.ub)
preds <- preds[order(preds$cofactor.range),]
lines(preds$cofactor.range, preds$preds.pred, col=KUBlue)
lines(preds$cofactor.range, preds$preds.ci.lb, lty="dashed", col=KUBlue)
lines(preds$cofactor.range, preds$preds.ci.ub, lty="dashed", col=KUBlue)

polygon(c(preds$cofactor.range[1],preds$cofactor.range,preds$cofactor.range[length(preds$cofactor.range)]), c(preds$preds.pred[1],preds$preds.ci.ub,preds$preds.pred[length(preds$cofactor.range)]), col = rgb(115/255,203/255,242/255, alpha=0.2), border = NA)
polygon(c(preds$cofactor.range[1],preds$cofactor.range,preds$cofactor.range[length(preds$cofactor.range)]), c(preds$preds.pred[1],preds$preds.ci.lb,preds$preds.pred[length(preds$cofactor.range)]), col = rgb(115/255,203/255,242/255, alpha=0.2), border = NA)

#http://handbook.cochrane.org/chapter_9/9_5_4_incorporating_heterogeneity_into_random_effects_models.htm
text(par("usr")[2],(par("usr")[4]-1.4*strheight("A"))                     ,cex=1,adj=c(1,0),paste("Coefficient = ",sprintf(res$beta[2], fmt='%#.1f'), sep=""), font=1)
text(par("usr")[2],(par("usr")[4]-2.9*strheight("A"))                     ,cex=1,adj=c(1,0),paste("R2 = ",round(res$R2),"% (QM = ",sprintf(res$QM, fmt='%#.1f'),", p = ",sprintf(res$pval[2], fmt='%#.3f'), ")", sep=""), font=1)
text(par("usr")[2],(par("usr")[4]-3.7*strheight("A")-0.6*strheight("A"))  ,cex=1,adj=c(1,0),paste("Residual I2 = ",sprintf(res$I2, fmt='%#.1f'),'%', sep=""), font=1)
legend("topleft", adj = 0, xjust = 1,c("Regression line","Confidence limits"), pch = NULL, pt.bg = "white", bty = "n", border = "white", lty=c("solid","dashed"), col=c(KUBlue,KUBlue))
mtext("Notes", side=1, line=-2, adj=0.0, cex=1, font=2, outer = TRUE)
mtext("Religion labels: B = Buddhism; C = Christian; I = Islam; J = Judaism; S = Sikhism", side=1,line=-0.5, cex=0.9, font=1,adj=0, outer=TRUE)
rstudioapi::savePlotAsImage(
  plotname,
  format = "png", width = 1000, height = 800)

### BAR PLOT ---------------
par(oma=c(5,1,0,0))
values <- c(0.29,0.37,0.40,0.46,0.51)
barplot(height=values, space=c(0.4,0.4,0.1,0.1,0.1), names.arg=c('Hindu','Any major\naffiliation','None','Other\naffiliation','Response\nwithheld'), col=c('blue2','gray75','gray50','gray25','gray0'), ylab="Burnout (%)",main="Religion and Burnout in the NHS, 2018", ylim=c(0,1))
box()
text(c(0.9,2.3,3.4,4.5,5.6),0.2,sprintf(values, fmt = '%#.2f'), col='white',font=2)
text(0.9,0.6,"Hindu\nvs all other\np < 0.000")
text(2.3,0.6,"Any major\nvs all other\np < 0.000")
#axis(1, at=c(1.15,3.65,6.15),labels=c("Dharmic","Abrahamic","Any religion"), cex=1.2, line= 1.5, las = 1, hadj = 0.5, tick = 0)
abline(h=0.39, lty="dotted", col="gray")
#Notes
mtext("Notes", side=1, line=-2, adj=0.0, cex=1, font=2, outer = TRUE)
mtext("1. Burnout = 'During the last 12 months have you felt unwell as a result of work related stress'", side=1,line=-0.5, cex=0.9, font=1,adj=0, outer=TRUE)
#mtext("2. Withheld and other responses have been excluded from colored bars.", side=1,line=1.5, cex=0.9, font=1,adj=0, outer=TRUE)
mtext("2. Major religous affiliations surveyed were: Christian, Jewish, Muslim, Hindu, Sikh, and Buddhist.", side=1,line=0.5, cex=0.9, font=1,adj=0, outer=TRUE)
mtext("3. Overall rate of burnout is 39% (dotted line) and includes withheld and other affiliations\nto the question of religion.", side=1,line=2.5, cex=0.9, font=1,adj=0, outer=TRUE)
rstudioapi::savePlotAsImage(
  "Bar plot - religion and burnout.png",
  format = "png", width = 750, height = 500)

## Dedication
# Hindu only; adding 'Other'
values <- c(0.84,0.76,0.72,0.73,0.62)
barplot(height=values, space=c(0.4,0.4,0.1,0.1,0.1), names.arg=c('Hindu','Any major\naffiliation','None','Other\naffiliation','Withheld'), col=c('blue2','gray75','gray50','gray25','gray0'), ylab="Burnout (%)",main="Religion and Dedication in the NHS, 2018", ylim=c(0,1))
text(c(0.9,2.3,3.4,4.5,5.6),0.5,sprintf(values, fmt = '%#.2f'), col='white',font=2)
#text(0.9,0.6,"Hindu\nvs all other\np < 0.000")
#text(2.3,0.6,"Any major\nvs all other\np < 0.000")
abline(h=0.74, lty="dotted", col="gray")
#Notes
mtext("Notes", side=1, line=-2, adj=0.0, cex=1, font=2, outer = TRUE)
mtext("1. Dedication = 'I look forward to going to work.'", side=1,line=-0.5, cex=0.9, font=1,adj=0, outer=TRUE)
mtext("2. Major religous affiliations surveyed were: Christian, Jewish, Muslim, Hindu, Sikh, and Buddhist.", side=1,line=0.5, cex=0.9, font=1,adj=0, outer=TRUE)
mtext("3. Overall rate of dedication is 74% (dotted line) and includes withheld and other affiliations\nto question of religion.", side=1,line=2.5, cex=0.9, font=1,adj=0, outer=TRUE)
rstudioapi::savePlotAsImage(
  "Bar plot - religion and dedication.png",
  format = "png", width = 750, height = 500)

## Harassment by patients/public
# Hindu only; adding 'Other'
values <- c(0.23,0.24,0.23,0.12,0.27)
barplot(height=values, space=c(0.4,0.4,0.1,0.1,0.1), names.arg=c('Hindu','Any major\naffiliation','None','Other\naffiliation','Withheld'), col=c('blue2','gray75','gray50','gray25','gray0'), ylab="Burnout (%)",main="Religion and Harassment by patients/public in the NHS, 2018", ylim=c(0,1))
text(c(0.9,2.3,3.4,4.5,5.6),0.4,sprintf(values, fmt = '%#.2f'), col='black',font=1)
#text(0.9,0.6,"Hindu\nvs all other\np < 0.000")
#text(2.3,0.6,"Any major\nvs all other\np < 0.000")
abline(h=0.24, lty="dotted", col="gray")
#Notes
mtext("Notes", side=1, line=-2, adj=0.0, cex=1, font=2, outer = TRUE)
mtext("1. Harassment = In the last 12 months, 'have you personally experienced harassment,\nbullying or abuse at work from patients / service users, their relatives or other members of the public'", side=1,line=0.5, cex=0.9, font=1,adj=0, outer=TRUE)
mtext("2. Major religous affiliations surveyed were: Christian, Jewish, Muslim, Hindu, Sikh, and Buddhist.", side=1,line=1.5, cex=0.9, font=1,adj=0, outer=TRUE)
mtext("3. Overall rate of harassment is 24% (dotted line) and includes withheld and other affiliations\nto question of religion.", side=1,line=3.5, cex=0.9, font=1,adj=0, outer=TRUE)

# TWO BY TWOS
data.temp$Religion
totals <- c(sum(data.temp$Burned.Out_Often.Always),sum(data.temp$Total))
# Hindu
factor <- data.temp$Burned.Out_Often.Always[4]
factor <- c(factor,data.temp$Total[4])
# Any major religion
factor <- sum(data.temp$Burned.Out_Often.Always[c(1:6)])
factor <- c(factor,sum(data.temp$Total[c(1:6)]))
# Any religion
factor <- sum(data.temp$Burned.Out_Often.Always[c(1:7)])
factor <- c(factor,sum(data.temp$Total[c(1:7)]))

other <- totals - factor
twobytwo=rbind(factor, other)
chisq.test(twobytwo)

