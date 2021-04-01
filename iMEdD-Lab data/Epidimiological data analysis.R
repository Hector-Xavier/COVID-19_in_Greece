library(chron)
#library(gplots)

directory <- "Z:/Covid-19 researches/iMEdD-Lab data"
setwd(directory)


#Epidimiological data
daily_timeline_data <- read.csv("greeceTimeline.csv",stringsAsFactors=FALSE)
daily_timeline_data <- daily_timeline_data[c(1:4,6:dim(daily_timeline_data)[1]),]
rownames(daily_timeline_data) <- daily_timeline_data[,1]
temp_table <- matrix(data=unlist(daily_timeline_data[,4:dim(daily_timeline_data)[2]]),nrow=dim(daily_timeline_data)[1],ncol=dim(daily_timeline_data)[2]-3,byrow=FALSE)
rownames(temp_table) <- rownames(daily_timeline_data)
colnames(temp_table) <- colnames(daily_timeline_data)[4:dim(daily_timeline_data)[2]]
daily_timeline_data <- temp_table
daily_timeline_data_dates <- chron(dates.=sapply(strsplit(colnames(daily_timeline_data),split="X"),'[',2),format=c(dates="m.d.Y"),out.format=c(dates="d/m/y"))

cumulative_cases_regional <- read.csv("greece_cases_v2.csv",encoding="UTF-8",stringsAsFactors=FALSE)
colnames(cumulative_cases_regional) <- c("geographical_subdivision","administrative_subdivision",colnames(cumulative_cases_regional)[3:dim(cumulative_cases_regional)[2]])
cumulative_cases_regional[,1] <- c("Greece","Attica","Central Greece","Peloponese","Peloponese","Epirus","Attica","Greece","Peloponese","Central Greece","Macedonia","Macedonia","Aegean","Thrace","Central Greece","Central Greece","Ionian","Peloponese","Macedonia","Crete","Epirus","Macedonia","Epirus","Macedonia","Thessaly","Macedonia","Ionian","Ionian","Macedonia","Macedonia","Peloponese","Greece","Aegean","Peloponese","Thessaly","Crete","Aegean","Ionian","Thessaly","Peloponese","Thrace","Macedonia","Macedonia","Epirus","Greece","Crete","Thrace","Aegean","Macedonia","Thessaly","Central Greece","Macedonia","Central Greece","Macedonia","Crete","Aegean")
cumulative_cases_regional[,2] <- c("Greece","Agion Oros","Western Greece","Peloponese","Peloponese","Epirus","Attica","Greece","Western Greece","Central Greece","Western Macedonia","Eastern Macedonia and Thrace","Southern Aegean","Eastern Macedonia and Thrace","Central Greece","Central Greece","Ionian","Western Greece","Central Macedonia","Crete","Epirus","Central Macedonia","Epirus","Eastern Macedonia and Thrace","Thessaly","Western Macedonia","Ionian","Ionian","Central Macedonia","Western Macedonia","Peloponese","Greece","Southern Aegean","Peloponese","Thessaly","Crete","Northern Aegean","Ionian","Thessaly","Peloponese","Eastern Macedonia and Thrace","Central Macedonia","Central Macedonia","Epirus","Greece","Crete","Eastern Macedonia and Thrace","Northern Aegean","Central Macedonia","Thessaly","Central Greece","Western Macedonia","Central Greece","Central Macedonia","Crete","Northern Aegean")
cumulative_cases_regional[,4] <- c("Unspecified","Agion_Oros","Aitoloakarnania","Argolida","Arkadia","Arta","Attica","Imported_under_own_volition","Achaia","Voiotia","Grevena","Drama","Dodekanisa","Evros","Evoia","Eurytania","Zakynthos","Ilia","Imathia","Iraklion","Thesprotia","Thessaloniki","Ioannina","Kavala","Karditsa","Kastoria","Kerkyra","Kafalonia","Kilkis","Kozani","Korinthia","Cruise_ship_Eleftherios_Venizelos","Kyklades","Lakonia","Larisa","Lasithi","Lesvos","Lefkada","Magnisia","Messinia","Xanthi","Pella","Pieria","Preveza","Imported_entry_gates","Rethymno","Rodopi","Samos","Serres","Trikala","Fthiotida","Florina","Fokida","Chalkidiki","Chania","Chios")
rownames(cumulative_cases_regional) <- cumulative_cases_regional[,4]
cumulative_cases_regional_context <- cumulative_cases_regional[,c(1,2,4,5)]
temp_table <- matrix(data=unlist(cumulative_cases_regional[,6:dim(cumulative_cases_regional)[2]]),nrow=dim(cumulative_cases_regional)[1],ncol=dim(cumulative_cases_regional)[2]-5,byrow=FALSE)
rownames(temp_table) <- rownames(cumulative_cases_regional)
colnames(temp_table) <- colnames(cumulative_cases_regional[,6:dim(cumulative_cases_regional)[2]])
cumulative_cases_regional <- temp_table
cumulative_cases_dates <- chron(dates.=sapply(strsplit(colnames(cumulative_cases_regional),split="X"),'[',2),format=c(dates="m.d.Y"),out.format=c(dates="d/m/y"))

cumulative_deaths_regional <- read.csv("greece_deaths_v2.csv",encoding="UTF-8",stringsAsFactors=FALSE)
colnames(cumulative_deaths_regional) <- c("geographical_subdivision","administrative_subdivision",colnames(cumulative_deaths_regional)[3:dim(cumulative_deaths_regional)[2]])
cumulative_deaths_regional[,1] <- c("Greece","Attica","Central Greece","Peloponese","Peloponese","Epirus","Attica","Peloponese","Central Greece","Macedonia","Macedonia","Aegean","Thrace","Central Greece","Central Greece","Ionian","Peloponese","Macedonia","Crete","Epirus","Macedonia","Epirus","Macedonia","Thessaly","Macedonia","Ionian","Ionian","Macedonia","Macedonia","Peloponese","Aegean","Peloponese","Thessaly","Crete","Aegean","Ionian","Thessaly","Peloponese","Thrace","Macedonia","Macedonia","Epirus","Crete","Thrace","Aegean","Macedonia","Thessaly","Central Greece","Macedonia","Central Greece","Macedonia","Crete","Aegean")
cumulative_deaths_regional[,2] <- c("Greece","Agion Oros","Western Greece","Peloponese","Peloponese","Epirus","Attica","Western Greece","Central Greece","Western Macedonia","Eastern Macedonia and Thrace","Southern Aegean","Eastern Macedonia and Thrace","Central Greece","Central Greece","Ionian","Western Greece","Central Macedonia","Crete","Epirus","Central Macedonia","Epirus","Eastern Macedonia and Thrace","Thessaly","Western Macedonia","Ionian","Ionian","Central Macedonia","Western Macedonia","Peloponese","Southern Aegean","Peloponese","Thessaly","Crete","Northern Aegean","Ionian","Thessaly","Peloponese","Eastern Macedonia and Thrace","Central Macedonia","Central Macedonia","Epirus","Crete","Eastern Macedonia and Thrace","Northern Aegean","Central Macedonia","Thessaly","Central Greece","Western Macedonia","Central Greece","Central Macedonia","Crete","Northern Aegean")
cumulative_deaths_regional[,4] <- c("Unspecified","Agion_Oros","Aitoloakarnania","Argolida","Arkadia","Arta","Attica","Achaia","Voiotia","Grevena","Drama","Dodekanisa","Evros","Evoia","Eurytania","Zakynthos","Ilia","Imathia","Iraklion","Thesprotia","Thessaloniki","Ioannina","Kavala","Karditsa","Kastoria","Kerkyra","Kafalonia","Kilkis","Kozani","Korinthia","Kyklades","Lakonia","Larisa","Lasithi","Lesvos","Lefkada","Magnisia","Messinia","Xanthi","Pella","Pieria","Preveza","Rethymno","Rodopi","Samos","Serres","Trikala","Fthiotida","Florina","Fokida","Chalkidiki","Chania","Chios")
cumulative_deaths_regional_context <- cumulative_deaths_regional[,c(1,2,4,5)]
rownames(cumulative_deaths_regional) <- cumulative_deaths_regional[,4]
temp_table <- matrix(data=unlist(cumulative_deaths_regional[,6:dim(cumulative_deaths_regional)[2]]),nrow=dim(cumulative_deaths_regional)[1],ncol=dim(cumulative_deaths_regional)[2]-5,byrow=FALSE)
rownames(temp_table) <- rownames(cumulative_deaths_regional)
colnames(temp_table) <- colnames(cumulative_deaths_regional[,6:dim(cumulative_deaths_regional)[2]])
cumulative_deaths_regional <- temp_table
cumulative_deaths_dates <- chron(dates.=sapply(strsplit(colnames(cumulative_deaths_regional),split="X"),'[',2),format=c(dates="m.d.Y"),out.format=c(dates="d/m/y"))
cumulative_deaths_regional <- cumulative_deaths_regional[,!is.na(cumulative_deaths_dates)]
cumulative_deaths_dates <- cumulative_deaths_dates[!is.na(cumulative_deaths_dates)]

rm(temp_table)

if(!file.exists(paste(directory,"Greece [total]",sep="/")))
  dir.create(paste(directory,"Greece [total]",sep="/"))
setwd(paste(directory,"Greece [total]",sep="/"))
if(!file.exists(paste(directory,"Greece [total]","Preliminary plots",sep="/")))
  dir.create(paste(directory,"Greece [total]","Preliminary plots",sep="/"))
setwd(paste(directory,"Greece [total]","Preliminary plots",sep="/"))

plot(daily_timeline_data[1,],type="l",xaxt="n",xlab="",ylab="Daily cases",main="Daily new cases [full timeline]")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)

temp_ok_days <- daily_timeline_data[1,]
temp_ok_days[as.integer(weekdays(daily_timeline_data_dates))<=2] <- NA
temp_bad_days <- daily_timeline_data[1,]
temp_bad_days[as.integer(weekdays(daily_timeline_data_dates))>2] <- NA

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - the weekend lack of testing.png",sep=""),sep="/")),width=1600,height=1200)
plot(temp_ok_days,type="p",pch=19,col="black",xaxt="n",xlab="",ylab="Daily cases",main="Daily new cases [Sunday-Monday lack of tests]")
points(temp_bad_days,type="p",pch=19,col="red")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
legend("topleft",legend=c("Tuesday to Thursday","Sunday & Monday"),pch=15,col=c("black","red"))
dev.off()

rm(temp_bad_days,temp_ok_days)

plot(daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))>2],type="p")
plot(daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))<=2],type="p")

weekday_correlations <- list()
for (index_of_comparison in 1:dim(daily_timeline_data)[1])
{
  weekday_temp_data <- cbind(daily_timeline_data[index_of_comparison,as.integer(weekdays(daily_timeline_data_dates))==1][1:55],daily_timeline_data[index_of_comparison,as.integer(weekdays(daily_timeline_data_dates))==2][1:55],daily_timeline_data[index_of_comparison,as.integer(weekdays(daily_timeline_data_dates))==3][1:55],daily_timeline_data[index_of_comparison,as.integer(weekdays(daily_timeline_data_dates))==4][1:55],daily_timeline_data[index_of_comparison,as.integer(weekdays(daily_timeline_data_dates))==5][1:55],daily_timeline_data[index_of_comparison,as.integer(weekdays(daily_timeline_data_dates))==6][1:55],daily_timeline_data[index_of_comparison,as.integer(weekdays(daily_timeline_data_dates))==7][1:55])
  colnames(weekday_temp_data) <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  rownames(weekday_temp_data) <- paste("Week",seq(dim(weekday_temp_data)[1]),sep="_")
  weekday_correlations[[rownames(daily_timeline_data)[index_of_comparison]]] <- cor(weekday_temp_data,use="complete.obs")
}

library(gplots)

for (i in 1:dim(daily_timeline_data)[1])
{
  png(file=file.path(paste(getwd(),paste("Correlations [",rownames(daily_timeline_data)[i],"].png",sep=""),sep="/")),width=900,height=900)
  heatmap.2(weekday_correlations[[i]],Rowv=FALSE,Colv=FALSE,dendrogram="none",main=rownames(daily_timeline_data)[i],margins=c(15,15))
  dev.off()
}

weekday_temp_data <- cbind(daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))==1][1:55],daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))==2][1:55],daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))==3][1:55],daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))==4][1:55],daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))==5][1:55],daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))==6][1:55],daily_timeline_data[1,as.integer(weekdays(daily_timeline_data_dates))==7][1:55])
heatmap.2(weekday_correlations$cases,Rowv=FALSE,Colv=FALSE,dendrogram="none",main="Daily new cases correlations",margins=c(15,15))

cor.test(weekday_temp_data[,1],weekday_temp_data[,7])
cor.test(weekday_temp_data[,2],weekday_temp_data[,6])

daily_timeline_data_correction <- daily_timeline_data[,1:dim(daily_timeline_data)[2]]
daily_timeline_data_correction[1,as.integer(weekdays(daily_timeline_data_dates))==1] <- median(weekday_temp_data[,7])/median(weekday_temp_data[,1])*daily_timeline_data_correction[1,as.integer(weekdays(daily_timeline_data_dates))==1]
daily_timeline_data_correction[1,as.integer(weekdays(daily_timeline_data_dates))==2] <- median(weekday_temp_data[,6])/median(weekday_temp_data[,2])*daily_timeline_data_correction[1,as.integer(weekdays(daily_timeline_data_dates))==2]

plot(daily_timeline_data[1,],type="l",main="Original data")
plot(daily_timeline_data_correction[1,],type="l",main="Median-based correction for Sunday & Monday")

daily_timeline_data_omission <- daily_timeline_data[1,]
daily_timeline_data_omission[as.integer(weekdays(daily_timeline_data_dates))<=2] <- NA
plot(daily_timeline_data_omission,type="p",main="Complete removal of Sunday & Monday")

rm(index_of_comparison,weekday_correlations,weekday_temp_data,i)

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - raw initial data.png",sep=""),sep="/")),width=1600,height=1200)
plot(daily_timeline_data[1,],type="l",xaxt="n",xlab="",ylab="Daily cases",main="Full, original data")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - raw median correction data.png",sep=""),sep="/")),width=1600,height=1200)
plot(daily_timeline_data_correction[1,],type="l",xaxt="n",xlab="",ylab="Dcaled daily cases",main="Median-based correction for Sunday & Monday")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - raw omission data.png",sep=""),sep="/")),width=1600,height=1200)
plot(daily_timeline_data_omission,type="l",xaxt="n",xlab="",ylab="Daily cases",main="Complete removal of Sunday & Monday")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - logged initial data.png",sep=""),sep="/")),width=1600,height=1200)
plot(log2(daily_timeline_data[1,]),type="l",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Full, original data")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - logged median correction data.png",sep=""),sep="/")),width=1600,height=1200)
plot(log2(daily_timeline_data_correction[1,]),type="l",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Median-based correction for Sunday & Monday")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - logged omission data.png",sep=""),sep="/")),width=1600,height=1200)
plot(log2(daily_timeline_data_omission),type="l",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Complete removal of Sunday & Monday")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - smoothed logged initial data.png",sep=""),sep="/")),width=1600,height=1200)
plot(filter(log2(daily_timeline_data[1,]),rep(1,5)/5),type="l",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Full, original data [smoothed]")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

png(file=file.path(paste(getwd(),paste("Daily cases [Greece total] - smoothed logged median correction data.png",sep=""),sep="/")),width=1600,height=1200)
plot(filter(log2(daily_timeline_data_correction[1,]),rep(1,5)/5),type="l",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Median-based correction for Sunday & Monday [smoothed]")
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

setwd(directory)

library(segmented)

#Greece, daily cases
if(!file.exists(paste(directory,"Greece [total]",sep="/")))
  dir.create(paste(directory,"Greece [total]",sep="/"))
setwd(paste(directory,"Greece [total]",sep="/"))
if(!file.exists(paste(directory,"Greece [total]","Daily cases",sep="/")))
  dir.create(paste(directory,"Greece [total]","Daily cases",sep="/"))
setwd(paste(directory,"Greece [total]","Daily cases",sep="/"))

timeline_for_segmentation <- log2(daily_timeline_data[1,])
timeline_for_segmentation_corrected <- log2(daily_timeline_data_correction[1,])
timeline_for_segmentation_omission <- log2(daily_timeline_data_omission)
timeline_for_segmentation_smoothed <- filter(log2(daily_timeline_data[1,]),rep(1,5)/5)
timeline_for_segmentation_corrected_smoothed <- filter(log2(daily_timeline_data_correction[1,]),rep(1,5)/5)

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation)),y=timeline_for_segmentation)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("01 - Optimal segmentation [Greece total] - initial data.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Greece daily new cases [initial data]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_corrected)),y=timeline_for_segmentation_corrected)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("03 - Optimal segmentation [Greece total] - initial corrected data.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation_corrected,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Greece daily new cases [correction for weekend testing]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_omission)),y=timeline_for_segmentation_omission)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("05 - Optimal segmentation [Greece total] - omission data.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation_omission,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Greece daily new cases [omission of Sunday & Monday]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates[as.integer(weekdays(daily_timeline_data_dates))>2])[1],daily_timeline_data_dates[as.integer(weekdays(daily_timeline_data_dates))>2][round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates[as.integer(weekdays(daily_timeline_data_dates))>2])),labels=as.character(daily_timeline_data_dates[as.integer(weekdays(daily_timeline_data_dates))>2]),las=2)
dev.off()

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_smoothed)),y=timeline_for_segmentation_smoothed)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("02 - Optimal segmentation [Greece total] - initial data smoothed.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation_smoothed,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Greece daily new cases [initial data, smoothing]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_corrected_smoothed)),y=timeline_for_segmentation_corrected_smoothed)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("04 - Optimal segmentation [Greece total] - initial corrected data smoothed.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation_corrected_smoothed,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main="Greece daily new cases [correction for weekend testing, smoothing]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

rm(temp_data_frame,temp_linear_model,temp_segmentation,timeline_for_segmentation,timeline_for_segmentation_smoothed)
setwd(directory)

rm(daily_timeline_data_correction,daily_timeline_data_omission,timeline_for_segmentation_corrected,timeline_for_segmentation_omission,timeline_for_segmentation_corrected_smoothed)



#Greece, intubated
if(!file.exists(paste(directory,"Greece [total]",sep="/")))
  dir.create(paste(directory,"Greece [total]",sep="/"))
setwd(paste(directory,"Greece [total]",sep="/"))
if(!file.exists(paste(directory,"Greece [total]","Intubated",sep="/")))
  dir.create(paste(directory,"Greece [total]","Intubated",sep="/"))
setwd(paste(directory,"Greece [total]","Intubated",sep="/"))

timeline_for_segmentation <- log2(daily_timeline_data[5,])
timeline_for_segmentation_smoothed <- filter(log2(daily_timeline_data[5,]),rep(1,5)/5)

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation)),y=timeline_for_segmentation)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("01 - Optimal segmentation [Greece total] - initial data.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation,type="p",xaxt="n",xlab="",ylab="log2 - scaled number of intubated patients",main="Greece intubated patients [initial data]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_smoothed)),y=timeline_for_segmentation_smoothed)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("02 - Optimal segmentation [Greece total] - initial data smoothed.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation_smoothed,type="p",xaxt="n",xlab="",ylab="log2 - scaled number of intubated patients",main="Greece intubated patients [smoothing]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

rm(temp_data_frame,temp_linear_model,temp_segmentation,timeline_for_segmentation,timeline_for_segmentation_smoothed)
setwd(directory)



#Greece, deaths
if(!file.exists(paste(directory,"Greece [total]",sep="/")))
  dir.create(paste(directory,"Greece [total]",sep="/"))
setwd(paste(directory,"Greece [total]",sep="/"))
if(!file.exists(paste(directory,"Greece [total]","Deaths",sep="/")))
  dir.create(paste(directory,"Greece [total]","Deaths",sep="/"))
setwd(paste(directory,"Greece [total]","Deaths",sep="/"))

timeline_for_segmentation <- log2(daily_timeline_data[2,])
timeline_for_segmentation_smoothed <- filter(log2(daily_timeline_data[2,]),rep(1,5)/5)

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation)),y=timeline_for_segmentation)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("01 - Optimal segmentation [Greece total] - initial data.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation,type="p",xaxt="n",xlab="",ylab="log2 - scaled number of deaths",main="Greece daily deaths [initial data]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_smoothed)),y=timeline_for_segmentation_smoothed)
temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
temp_linear_model <- lm(y~x,data=temp_data_frame)
#selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
#segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))

png(file=file.path(paste(getwd(),paste("02 - Optimal segmentation [Greece total] - initial data smoothed.png",sep=""),sep="/")),width=1600,height=1024)
plot(timeline_for_segmentation_smoothed,type="p",xaxt="n",xlab="",ylab="log2 - scaled number of deaths",main="Greece daily deaths [smoothing]")
plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
abline(v=round(temp_segmentation$psi[,2]))
legend("bottomright",legend=paste(c(range(daily_timeline_data_dates)[1],daily_timeline_data_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
axis(1,at=seq(length(daily_timeline_data_dates)),labels=as.character(daily_timeline_data_dates),las=2)
dev.off()

rm(temp_data_frame,temp_linear_model,temp_segmentation,timeline_for_segmentation,timeline_for_segmentation_smoothed)
setwd(directory)



#analysis of county data
for (i in 1:dim(cumulative_cases_regional)[1])
{
  tryCatch({
    #cases
    if(!file.exists(paste(directory,"Greece [by county]",sep="/")))
      dir.create(paste(directory,"Greece [by county]",sep="/"))
    setwd(paste(directory,"Greece [by county]",sep="/"))
    if(!file.exists(paste(directory,"Greece [by county]","Daily cases",sep="/")))
      dir.create(paste(directory,"Greece [by county]","Daily cases",sep="/"))
    setwd(paste(directory,"Greece [by county]","Daily cases",sep="/"))
    if(!file.exists(paste(directory,"Greece [by county]","Daily cases",rownames(cumulative_cases_regional)[i],sep="/")))
      dir.create(paste(directory,"Greece [by county]","Daily cases",rownames(cumulative_cases_regional)[i],sep="/"))
    setwd(paste(directory,"Greece [by county]","Daily cases",rownames(cumulative_cases_regional)[i],sep="/"))
    
    timeline_for_segmentation <- c(cumulative_cases_regional[i,1],diff(cumulative_cases_regional[i,]))
    names(timeline_for_segmentation) <- colnames(cumulative_cases_regional)
    timeline_for_segmentation[timeline_for_segmentation<0] <- NA
    
    temp_ok_days <- timeline_for_segmentation
    temp_ok_days[as.integer(weekdays(cumulative_cases_dates))<=2] <- NA
    temp_bad_days <- timeline_for_segmentation
    temp_bad_days[as.integer(weekdays(cumulative_cases_dates))>2] <- NA
    
    png(file=file.path(paste(getwd(),paste("Daily cases [",rownames(cumulative_cases_regional)[i],"] - the weekend lack of testing.png",sep=""),sep="/")),width=1600,height=1200)
    plot(temp_ok_days,type="p",pch=19,col="black",xaxt="n",xlab="",ylab="Daily cases",main=paste("Daily new cases: ",rownames(cumulative_cases_regional)[i]," [Sunday-Monday lack of tests]"))
    points(temp_bad_days,type="p",pch=19,col="red")
    axis(1,at=seq(length(cumulative_cases_dates)),labels=as.character(cumulative_cases_dates),las=2)
    legend("topleft",legend=c("Tuesday to Thursday","Sunday & Monday"),pch=15,col=c("black","red"))
    dev.off()
    
    weekday_temp_data <- cbind(timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==1][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==2][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==3][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==4][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==5][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==6][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==7][1:55])
    timeline_for_segmentation_corrected <- timeline_for_segmentation
    names(timeline_for_segmentation_corrected) <- colnames(cumulative_cases_regional)
    timeline_for_segmentation_corrected[as.integer(weekdays(cumulative_cases_dates))==1] <- median(weekday_temp_data[,7],na.rm=TRUE)/median(weekday_temp_data[,1],na.rm=TRUE)*timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==1]
    timeline_for_segmentation_corrected[as.integer(weekdays(cumulative_cases_dates))==2] <- median(weekday_temp_data[,6],na.rm=TRUE)/median(weekday_temp_data[,2],na.rm=TRUE)*timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==2]
    rm(temp_bad_days,temp_ok_days,weekday_temp_data)
    
    timeline_for_segmentation_omission <- timeline_for_segmentation
    timeline_for_segmentation_omission[as.integer(weekdays(cumulative_cases_dates))<=2] <- NA

    timeline_for_segmentation <- log2(c(cumulative_cases_regional[i,1],diff(cumulative_cases_regional[i,])))
    timeline_for_segmentation[timeline_for_segmentation==-Inf] <- NA
    timeline_for_segmentation_corrected <- log2(timeline_for_segmentation_corrected)
    timeline_for_segmentation_corrected[timeline_for_segmentation_corrected==-Inf] <- NA
    timeline_for_segmentation_omission <- log2(timeline_for_segmentation_omission)
    timeline_for_segmentation_omission[timeline_for_segmentation_omission==-Inf] <- NA
    
    temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation)),y=timeline_for_segmentation)
    temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
    temp_linear_model <- lm(y~x,data=temp_data_frame)
    #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
    #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
    temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
    
    png(file=file.path(paste(getwd(),paste("01 - Optimal segmentation [",rownames(cumulative_cases_regional)[i],"] - initial data.png",sep=""),sep="/")),width=1600,height=1024)
    plot(timeline_for_segmentation,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main=paste("Daily new cases [",rownames(cumulative_cases_regional)[i],"] - initial data",sep=""))
    plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
    abline(v=round(temp_segmentation$psi[,2]))
    legend("bottomright",legend=paste(c(range(cumulative_cases_dates)[1],cumulative_cases_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
    axis(1,at=seq(length(cumulative_cases_dates)),labels=as.character(cumulative_cases_dates),las=2)
    dev.off()
    
    temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_corrected)),y=timeline_for_segmentation_corrected)
    temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
    temp_linear_model <- lm(y~x,data=temp_data_frame)
    #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
    #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
    temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
    
    png(file=file.path(paste(getwd(),paste("02 - Optimal segmentation [",rownames(cumulative_cases_regional)[i],"] - initial corrected data.png",sep=""),sep="/")),width=1600,height=1024)
    plot(timeline_for_segmentation_corrected,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main=paste("Daily new cases [",rownames(cumulative_cases_regional)[i],"] - correction for weekend testing"))
    plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
    abline(v=round(temp_segmentation$psi[,2]))
    legend("bottomright",legend=paste(c(range(cumulative_cases_dates)[1],cumulative_cases_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
    axis(1,at=seq(length(cumulative_cases_dates)),labels=as.character(cumulative_cases_dates),las=2)
    dev.off()
    
    temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_omission)),y=timeline_for_segmentation_omission)
    temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
    temp_linear_model <- lm(y~x,data=temp_data_frame)
    #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
    #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
    temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
    
    png(file=file.path(paste(getwd(),paste("03 - Optimal segmentation [",rownames(cumulative_cases_regional)[i],"] - omission data.png",sep=""),sep="/")),width=1600,height=1024)
    plot(timeline_for_segmentation_omission,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main=paste("Daily new cases [",rownames(cumulative_cases_regional)[i],"] - omission of Sunday & Monday"))
    plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
    abline(v=round(temp_segmentation$psi[,2]))
    legend("bottomright",legend=paste(c(range(cumulative_cases_dates[as.integer(weekdays(cumulative_cases_dates))>2])[1],cumulative_cases_dates[as.integer(weekdays(cumulative_cases_dates))>2][round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
    axis(1,at=seq(length(cumulative_cases_dates)),labels=as.character(cumulative_cases_dates),las=2)
    dev.off()
    
    rm(temp_data_frame,temp_linear_model,temp_segmentation,timeline_for_segmentation,timeline_for_segmentation_corrected,timeline_for_segmentation_omission)
    setwd(directory)
    
    
    if (i <= dim(cumulative_deaths_regional)[1])
    {
      #deaths
      if(!file.exists(paste(directory,"Greece [by county]",sep="/")))
        dir.create(paste(directory,"Greece [by county]",sep="/"))
      setwd(paste(directory,"Greece [by county]",sep="/"))
      if(!file.exists(paste(directory,"Greece [by county]","Daily deaths",sep="/")))
        dir.create(paste(directory,"Greece [by county]","Daily deaths",sep="/"))
      setwd(paste(directory,"Greece [by county]","Daily deaths",sep="/"))
      if(!file.exists(paste(directory,"Greece [by county]","Daily deaths",rownames(cumulative_deaths_regional)[i],sep="/")))
        dir.create(paste(directory,"Greece [by county]","Daily deaths",rownames(cumulative_deaths_regional)[i],sep="/"))
      setwd(paste(directory,"Greece [by county]","Daily deaths",rownames(cumulative_deaths_regional)[i],sep="/"))
      
      timeline_for_segmentation <- c(cumulative_deaths_regional[i,1],diff(cumulative_deaths_regional[i,]))
      names(timeline_for_segmentation) <- colnames(cumulative_deaths_regional)
      timeline_for_segmentation[timeline_for_segmentation<0] <- NA
      
      timeline_for_segmentation <- log2(c(cumulative_deaths_regional[i,1],diff(cumulative_deaths_regional[i,])))
      timeline_for_segmentation[timeline_for_segmentation==-Inf] <- NA
      
      temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation)),y=timeline_for_segmentation)
      temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
      temp_linear_model <- lm(y~x,data=temp_data_frame)
      #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
      #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
      temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
      
      png(file=file.path(paste(getwd(),paste("01 - Optimal segmentation [",rownames(cumulative_deaths_regional)[i],"] - initial data.png",sep=""),sep="/")),width=1600,height=1024)
      plot(timeline_for_segmentation,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily deaths",main=paste("Daily new deaths [",rownames(cumulative_deaths_regional)[i],"] - initial data",sep=""))
      plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
      abline(v=round(temp_segmentation$psi[,2]))
      legend("bottomright",legend=paste(c(range(cumulative_deaths_dates)[1],cumulative_deaths_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
      axis(1,at=seq(length(cumulative_deaths_dates)),labels=as.character(cumulative_deaths_dates),las=2)
      dev.off()
      
      rm(temp_data_frame,temp_linear_model,temp_segmentation,timeline_for_segmentation)
      setwd(directory)
    }
  }, error=function(e){})
}

setwd(directory)
temporary_file_system <- list.dirs(paste(directory,"Greece [by county]","Daily cases",sep="/"))
for (i in 1:length(temporary_file_system))
  if(length(list.files(temporary_file_system[i])) < 2)
    unlink(temporary_file_system[i],recursive=TRUE)
temporary_file_system <- list.dirs(paste(directory,"Greece [by county]","Daily deaths",sep="/"))
for (i in 1:length(temporary_file_system))
  if(length(list.files(temporary_file_system[i])) < 1)
    unlink(temporary_file_system[i],recursive=TRUE)    
rm(temporary_file_system,i)



#analysis of geographic region data
temporary_cumulative_cases_regional_context <- cumulative_cases_regional_context
temporary_cumulative_cases_regional_context[cumulative_cases_regional_context[,1] == "Greece",1] <- temporary_cumulative_cases_regional_context[cumulative_cases_regional_context[,1] == "Greece",3]
for (i in 1:length(levels(as.factor(temporary_cumulative_cases_regional_context[,1]))))
{
  tryCatch({
    #cases
    if(!file.exists(paste(directory,"Greece [by geographic region]",sep="/")))
      dir.create(paste(directory,"Greece [by geographic region]",sep="/"))
    setwd(paste(directory,"Greece [by geographic region]",sep="/"))
    if(!file.exists(paste(directory,"Greece [by geographic region]","Daily cases",sep="/")))
      dir.create(paste(directory,"Greece [by geographic region]","Daily cases",sep="/"))
    setwd(paste(directory,"Greece [by geographic region]","Daily cases",sep="/"))
    if(!file.exists(paste(directory,"Greece [by geographic region]","Daily cases",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],sep="/")))
      dir.create(paste(directory,"Greece [by geographic region]","Daily cases",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],sep="/"))
    setwd(paste(directory,"Greece [by geographic region]","Daily cases",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],sep="/"))
    
    temporary_cumulative_cases <- cumulative_cases_regional
    temporary_cumulative_cases[is.na(temporary_cumulative_cases)] <- 0
    timeline_for_segmentation <- colSums(temporary_cumulative_cases[temporary_cumulative_cases_regional_context[,1] == levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],])
    rm(temporary_cumulative_cases)
    
    timeline_for_segmentation <- c(timeline_for_segmentation[1],diff(timeline_for_segmentation))
    names(timeline_for_segmentation) <- colnames(cumulative_cases_regional)
    timeline_for_segmentation[timeline_for_segmentation<0] <- NA
    
    temp_ok_days <- timeline_for_segmentation
    temp_ok_days[as.integer(weekdays(cumulative_cases_dates))<=2] <- NA
    temp_bad_days <- timeline_for_segmentation
    temp_bad_days[as.integer(weekdays(cumulative_cases_dates))>2] <- NA
    
    png(file=file.path(paste(getwd(),paste("Daily cases [",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],"] - the weekend lack of testing.png",sep=""),sep="/")),width=1600,height=1200)
    plot(temp_ok_days,type="p",pch=19,col="black",xaxt="n",xlab="",ylab="Daily cases",main=paste("Daily new cases: ",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i]," [Sunday-Monday lack of tests]"))
    points(temp_bad_days,type="p",pch=19,col="red")
    axis(1,at=seq(length(cumulative_cases_dates)),labels=as.character(cumulative_cases_dates),las=2)
    legend("topleft",legend=c("Tuesday to Thursday","Sunday & Monday"),pch=15,col=c("black","red"))
    dev.off()
    
    weekday_temp_data <- cbind(timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==1][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==2][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==3][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==4][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==5][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==6][1:55],timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==7][1:55])
    timeline_for_segmentation_corrected <- timeline_for_segmentation
    names(timeline_for_segmentation_corrected) <- colnames(cumulative_cases_regional)
    timeline_for_segmentation_corrected[as.integer(weekdays(cumulative_cases_dates))==1] <- median(weekday_temp_data[,7],na.rm=TRUE)/median(weekday_temp_data[,1],na.rm=TRUE)*timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==1]
    timeline_for_segmentation_corrected[as.integer(weekdays(cumulative_cases_dates))==2] <- median(weekday_temp_data[,6],na.rm=TRUE)/median(weekday_temp_data[,2],na.rm=TRUE)*timeline_for_segmentation[as.integer(weekdays(cumulative_cases_dates))==2]
    rm(temp_bad_days,temp_ok_days,weekday_temp_data)
    
    timeline_for_segmentation_omission <- timeline_for_segmentation
    timeline_for_segmentation_omission[as.integer(weekdays(cumulative_cases_dates))<=2] <- NA
    
    timeline_for_segmentation <- log2(timeline_for_segmentation)
    timeline_for_segmentation[timeline_for_segmentation==-Inf] <- NA
    timeline_for_segmentation_corrected <- log2(timeline_for_segmentation_corrected)
    timeline_for_segmentation_corrected[timeline_for_segmentation_corrected==-Inf] <- NA
    timeline_for_segmentation_omission <- log2(timeline_for_segmentation_omission)
    timeline_for_segmentation_omission[timeline_for_segmentation_omission==-Inf] <- NA
    
    temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation)),y=timeline_for_segmentation)
    temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
    temp_linear_model <- lm(y~x,data=temp_data_frame)
    #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
    #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
    temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
    
    png(file=file.path(paste(getwd(),paste("01 - Optimal segmentation [",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],"] - initial data.png",sep=""),sep="/")),width=1600,height=1024)
    plot(timeline_for_segmentation,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main=paste("Daily new cases [",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],"] - initial data",sep=""))
    plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
    abline(v=round(temp_segmentation$psi[,2]))
    legend("bottomright",legend=paste(c(range(cumulative_cases_dates)[1],cumulative_cases_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
    axis(1,at=seq(length(cumulative_cases_dates)),labels=as.character(cumulative_cases_dates),las=2)
    dev.off()
    
    temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_corrected)),y=timeline_for_segmentation_corrected)
    temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
    temp_linear_model <- lm(y~x,data=temp_data_frame)
    #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
    #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
    temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
    
    png(file=file.path(paste(getwd(),paste("02 - Optimal segmentation [",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],"] - initial corrected data.png",sep=""),sep="/")),width=1600,height=1024)
    plot(timeline_for_segmentation_corrected,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main=paste("Daily new cases [",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],"] - correction for weekend testing"))
    plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
    abline(v=round(temp_segmentation$psi[,2]))
    legend("bottomright",legend=paste(c(range(cumulative_cases_dates)[1],cumulative_cases_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
    axis(1,at=seq(length(cumulative_cases_dates)),labels=as.character(cumulative_cases_dates),las=2)
    dev.off()
    
    temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation_omission)),y=timeline_for_segmentation_omission)
    temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
    temp_linear_model <- lm(y~x,data=temp_data_frame)
    #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
    #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
    temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
    
    png(file=file.path(paste(getwd(),paste("03 - Optimal segmentation [",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],"] - omission data.png",sep=""),sep="/")),width=1600,height=1024)
    plot(timeline_for_segmentation_omission,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily cases",main=paste("Daily new cases [",levels(as.factor(temporary_cumulative_cases_regional_context[,1]))[i],"] - omission of Sunday & Monday"))
    plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
    abline(v=round(temp_segmentation$psi[,2]))
    legend("bottomright",legend=paste(c(range(cumulative_cases_dates[as.integer(weekdays(cumulative_cases_dates))>2])[1],cumulative_cases_dates[as.integer(weekdays(cumulative_cases_dates))>2][round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
    axis(1,at=seq(length(cumulative_cases_dates[as.integer(weekdays(cumulative_cases_dates))>2])),labels=as.character(cumulative_cases_dates[as.integer(weekdays(cumulative_cases_dates))>2]),las=2)
    dev.off()
    
    rm(temp_data_frame,temp_linear_model,temp_segmentation,timeline_for_segmentation,timeline_for_segmentation_corrected,timeline_for_segmentation_omission)
    setwd(directory)
    
    if (i <= length(levels(as.factor(cumulative_deaths_regional_context[,1]))))
    {
      #deaths
      if(!file.exists(paste(directory,"Greece [by geographic region]",sep="/")))
        dir.create(paste(directory,"Greece [by geographic region]",sep="/"))
      setwd(paste(directory,"Greece [by geographic region]",sep="/"))
      if(!file.exists(paste(directory,"Greece [by geographic region]","Daily deaths",sep="/")))
        dir.create(paste(directory,"Greece [by geographic region]","Daily deaths",sep="/"))
      setwd(paste(directory,"Greece [by geographic region]","Daily deaths",sep="/"))
      if(!file.exists(paste(directory,"Greece [by geographic region]","Daily deaths",levels(as.factor(cumulative_deaths_regional_context[,1]))[i],sep="/")))
        dir.create(paste(directory,"Greece [by geographic region]","Daily deaths",levels(as.factor(cumulative_deaths_regional_context[,1]))[i],sep="/"))
      setwd(paste(directory,"Greece [by geographic region]","Daily deaths",levels(as.factor(cumulative_deaths_regional_context[,1]))[i],sep="/"))
      
      temporary_cumulative_deaths <- cumulative_deaths_regional
      temporary_cumulative_deaths[is.na(temporary_cumulative_deaths)] <- 0
      timeline_for_segmentation <- colSums(temporary_cumulative_deaths[cumulative_deaths_regional_context[,1] == levels(as.factor(cumulative_deaths_regional_context[,1]))[i],])
      rm(temporary_cumulative_deaths)
      
      timeline_for_segmentation <- c(timeline_for_segmentation[1],diff(timeline_for_segmentation))
      names(timeline_for_segmentation) <- colnames(cumulative_deaths_regional)
      timeline_for_segmentation[timeline_for_segmentation<0] <- NA
      
      timeline_for_segmentation <- log2(timeline_for_segmentation)
      timeline_for_segmentation[timeline_for_segmentation==-Inf] <- NA
      
      temp_data_frame <- data.frame(x=seq(length(timeline_for_segmentation)),y=timeline_for_segmentation)
      temp_data_frame$y[temp_data_frame$y == -Inf] <- NA
      temp_linear_model <- lm(y~x,data=temp_data_frame)
      #selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi
      #segmented(temp_linear_model,seg.Z=~x, npsi=10, control=seg.control(display=FALSE))
      temp_segmentation <- segmented(temp_linear_model,seg.Z=~x, npsi=selgmented(temp_linear_model,seg.Z=~x,return.fit = FALSE,type="bic",Kmax=10)$n.psi, control=seg.control(display=FALSE))
      
      png(file=file.path(paste(getwd(),paste("01 - Optimal segmentation [",levels(as.factor(cumulative_deaths_regional_context[,1]))[i],"] - initial data.png",sep=""),sep="/")),width=1600,height=1024)
      plot(timeline_for_segmentation,type="p",xaxt="n",xlab="",ylab="log2 - scaled daily deaths",main=paste("Daily new deaths [",levels(as.factor(cumulative_deaths_regional_context[,1]))[i],"] - initial data",sep=""))
      plot.segmented(temp_segmentation,add=TRUE, res=FALSE,res.col=1,rug=FALSE,prev.trend=FALSE,conf.level=0.5,shade=TRUE,col=c("darkblue","red3")[as.integer(slope(temp_segmentation)$x[,1]>0)+1],lwd=2)
      abline(v=round(temp_segmentation$psi[,2]))
      legend("bottomright",legend=paste(c(range(cumulative_deaths_dates)[1],cumulative_deaths_dates[round(temp_segmentation$psi[,2])]),c(", now doubling every ",", now halved every ")[as.integer(1/slope(temp_segmentation)$x[,1]<=0)+1],abs(round(1/slope(temp_segmentation)$x[,1]))," days",sep=""),pch=15,col=c("darkblue","red3")[(as.integer(slope(temp_segmentation)$x[,1]>0)+1)])
      axis(1,at=seq(length(cumulative_deaths_dates)),labels=as.character(cumulative_deaths_dates),las=2)
      dev.off()
      
      rm(temp_data_frame,temp_linear_model,temp_segmentation,timeline_for_segmentation)
      setwd(directory)
    }
  }, error=function(e){})
}
rm(temporary_cumulative_cases_regional_context)

setwd(directory)
temporary_file_system <- list.dirs(paste(directory,"Greece [by geographic region]","Daily cases",sep="/"))
for (i in 1:length(temporary_file_system))
  if(length(list.files(temporary_file_system[i])) < 2)
    unlink(temporary_file_system[i],recursive=TRUE)
temporary_file_system <- list.dirs(paste(directory,"Greece [by geographic region]","Daily deaths",sep="/"))
for (i in 1:length(temporary_file_system))
  if(length(list.files(temporary_file_system[i])) < 1)
    unlink(temporary_file_system[i],recursive=TRUE)
rm(temporary_file_system,i)
