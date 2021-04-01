directory <- "Z:/Covid-19 researches/Movement data"
setwd(directory)

library(chron)
library(gplots)
library(signal)
library(splines)
movement_data_table <- read.csv("2020_GR_Region_Mobility_Report.csv",stringsAsFactors=FALSE)
movement_data_table <- movement_data_table[,c(3,9:15)]
colnames(movement_data_table) <- c(colnames(movement_data_table)[1:2],sapply(strsplit(colnames(movement_data_table)[3:dim(movement_data_table)[2]],split="_percent"),'[',1))
movement_data_table[movement_data_table[,1] == "",1] <- "Decentralized Administration of Greece total"
movement_data_table[,1] <- sapply(strsplit(movement_data_table[,1],split=" of "),'[',2)

movement_percentage_changes <- matrix(as.numeric(unlist(movement_data_table[3:dim(movement_data_table)[2]])),nrow=dim(movement_data_table)[1],byrow=FALSE)
movement_percentage_changes <- movement_data_table[,3:8]

if(!file.exists(paste(directory,"Movement correlations",sep="/")))
  dir.create(paste(directory,"Movement correlations",sep="/"))
if(!file.exists(paste(directory,"Movement progression",sep="/")))
  dir.create(paste(directory,"Movement progression",sep="/"))

for (i in 1:length(levels(as.factor(movement_data_table[,1]))))
{
  png(file=file.path(paste(paste(getwd(),"Movement correlations",sep="/"),paste("Correlations [",levels(as.factor(movement_data_table[,1]))[i],"].png",sep=""),sep="/")),width=900,height=900)
  heatmap.2(cor(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,],use="complete.obs"),Rowv=FALSE,Colv=FALSE,dendrogram="none",main=paste("Correlations for",levels(as.factor(movement_data_table[,1]))[i],sep=" "),margins=c(15,15))
  dev.off()
  
  png(file=file.path(paste(paste(getwd(),"Movement progression",sep="/"),paste("Movement data [",levels(as.factor(movement_data_table[,1]))[i],"].png",sep=""),sep="/")),width=1600,height=1200)
  par(mar= c(5, 4, 7, 2) + 0.1)
  plot(as.numeric(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,1]),col="red2",pch=16,xaxt="n",las=2,xlab="",ylab="% change from baseline (average of 03/01/2020 to 06/02/2020)",main=paste("Movement data [",levels(as.factor(movement_data_table[,1]))[i],"]",sep=""))
  axis(1,at=seq(sum(as.integer(as.factor(movement_data_table[,1]))==i)/10)*10,labels=as.character(chron(dates.="14/02/2020",format="d/m/y") + seq(sum(as.integer(as.factor(movement_data_table[,1]))==i)/10)*10),las=2)
  for (j in 2:6)
    points(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,j],pch=16,col=colors()[j*24])
  legend("bottomleft",legend=colnames(movement_percentage_changes),pch=15,col=c("red2",colors()[(2:6)*24]))
  par(mar= c(5, 4, 4, 2) + 0.1)
  dev.off()
  
  png(file=file.path(paste(paste(getwd(),"Movement progression",sep="/"),paste("Movement data [",levels(as.factor(movement_data_table[,1]))[i],"] - smoothed.png",sep=""),sep="/")),width=1600,height=1200)
  par(mar= c(5, 4, 7, 2) + 0.1)
  plot(predict(lm(as.numeric(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,1])~ns(seq(length(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,1])),11)),x=seq(length(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,1]))),col="red2",type="l",lwd=3,xaxt="n",las=2,xlab="",ylab="% change from baseline (average of 03/01/2020 to 06/02/2020)",main=paste("Movement data [",levels(as.factor(movement_data_table[,1]))[i],"]",sep=""))
  axis(1,at=seq(sum(as.integer(as.factor(movement_data_table[,1]))==i)/10)*10,labels=as.character(chron(dates.="14/02/2020",format="d/m/y") + seq(sum(as.integer(as.factor(movement_data_table[,1]))==i)/10)*10),las=2)
  for (j in 2:6)
    lines(predict(lm(as.numeric(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,j])~ns(seq(length(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,j])),11)),x=seq(length(movement_percentage_changes[as.integer(as.factor(movement_data_table[,1]))==i,j]))),lwd=3,col=colors()[j*24])
  legend("bottomleft",legend=colnames(movement_percentage_changes),pch=15,col=c("red2",colors()[(2:6)*24]))
  par(mar= c(5, 4, 4, 2) + 0.1)
  dev.off()
}
rm(i,j)
