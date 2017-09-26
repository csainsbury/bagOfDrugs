library(survival)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

endRuninPeriod   <- '2013-01-01'


# import data
linkId_mortalityData <- read.csv("~/R/_workingDirectory/bagOfDrugs/local_py/5y_30bins_1yMort/Xtest_LinkIds.csv", header = F)
mortalityProb <- read.csv("~/R/_workingDirectory/bagOfDrugs/local_py/5y_30bins_1yMort/y_pred.csv", header = F)

# structure data for survival analysis
survivalDF <- data.frame(linkId_mortalityData, mortalityProb)
colnames(survivalDF) <- c("LinkId", "unix_deathDate", "age", "median", "cv", "mortProb")
survivalDF$isDead <- ifelse(survivalDF$unix_deathDate == 0, 0, 1)

# survival analysis - whole followup period

simpleSurvivalPlot<-function(inputFrame, endDateUnix, ylimMin, mortProb_thresh) {
  # inputFrame <- survivalDF; endDateUnix <- max(inputFrame$unix_deathDate)
  # sampleDateUnix <- 
  
  SurvivalData<-inputFrame

  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-returnUnixDateTime(endRuninPeriod)
  SurvivalData$timeToDeath<-ifelse(SurvivalData$isDead==1,(SurvivalData$unix_deathDate-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$isDead==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$isDead
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (mortProb > mortProb_thresh), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=5,ylim=c(ylimMin,1))
  
  # mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+medianHbA1cInRange+nValsPerIDinRange+(hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age + cv + (mortProb > mortProb_thresh), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
  print(summary(mfitAge50.coxph))
  
}
simpleSurvivalPlot(survivalDF, max(survivalDF$unix_deathDate), 0.7, quantile(survivalDF$mortProb)[3])
    
    # shorter follow up periods (n years)
    survivalDF_nY <- survivalDF
    n = 1
    
    generatedLastDeath <- returnUnixDateTime(endRuninPeriod) + (n * (60*60*24*365.25))
    survivalDF_nY$isDead <- ifelse(survivalDF_nY$unix_deathDate == 0 | survivalDF_nY$unix_deathDate > generatedLastDeath, 0, 1)
    
    survivalDF_nY$unix_deathDate <- ifelse(survivalDF_nY$unix_deathDate > generatedLastDeath, generatedLastDeath, survivalDF_nY$unix_deathDate)
    simpleSurvivalPlot(survivalDF_nY, max(survivalDF_nY$unix_deathDate), 0.9, quantile(survivalDF_nY$mortProb)[3])
    
print('total deaths: '); print(sum(survivalDF_nY$isDead))
print('total deaths in high prob group: '); print(sum(subset(survivalDF_nY, mortProb > quantile(survivalDF_nY$mortProb)[3])$isDead))

