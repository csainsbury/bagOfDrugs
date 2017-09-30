library(survival)
library(smoothHR)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

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

simpleSurvivalPlot_factorisedProb<-function(inputFrame, endDateUnix, ylimMin, prob_factor) {
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
  
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ prob_factor, data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=5,ylim=c(ylimMin,1))
  
  # mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+medianHbA1cInRange+nValsPerIDinRange+(hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age + cv + prob_factor, data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
  print(summary(mfitAge50.coxph))
  
}

smoothHR_plot <- function(inputFrame, endDateUnix) {
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
 
  
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age + cv + pspline(mortProb), data = SurvivalData, x = TRUE)
  smoothFit <- smoothHR(data=SurvivalData, coxfit=mfitAge50.coxph)
  plot(smoothFit, predictor = "mortProb")
}


endRuninPeriod   <- '2013-01-01'


# import data
linkId_mortalityData <- read.csv("~/R/_workingDirectory/bagOfDrugs/local_py/5y_30bins_1yMort_T1/Xtest_LinkIds.csv", header = F)
mortalityProb <- read.csv("~/R/_workingDirectory/bagOfDrugs/local_py/5y_30bins_1yMort_T1/y_pred.csv", header = F)

# structure data for survival analysis
survivalDF <- data.frame(linkId_mortalityData, mortalityProb)
colnames(survivalDF) <- c("LinkId", "unix_deathDate", "age", "median", "cv", "mortProb")
survivalDF$isDead <- ifelse(survivalDF$unix_deathDate == 0, 0, 1)

# survival analysis - whole followup period

simpleSurvivalPlot(survivalDF, max(survivalDF$unix_deathDate), 0.7, quantile(survivalDF$mortProb)[3])

print('total deaths: '); print(sum(survivalDF$isDead))
print('total deaths in high prob group: '); print(sum(subset(survivalDF, mortProb > quantile(survivalDF$mortProb)[3])$isDead))
summary(subset(survivalDF, mortProb > quantile(survivalDF$mortProb)[3]))
summary(subset(survivalDF, mortProb <= quantile(survivalDF$mortProb)[3]))

factorGroups = 2
survivalDF$prob_factor <- with(survivalDF, cut(mortProb, 
                                                     breaks=quantile(mortProb, probs=seq(0,1, by=(1 / factorGroups)), na.rm=TRUE),
                                                     include.lowest=TRUE))

simpleSurvivalPlot_factorisedProb(survivalDF, max(survivalDF$unix_deathDate), 0.6, survivalDF$prob_factor)


    
    # shorter follow up periods (n years)
    survivalDF_nY <- survivalDF
    n = 1
    
    generatedLastDeath <- returnUnixDateTime(endRuninPeriod) + (n * (60*60*24*365.25))
    survivalDF_nY$isDead <- ifelse(survivalDF_nY$unix_deathDate == 0 | survivalDF_nY$unix_deathDate > generatedLastDeath, 0, 1)
    
    survivalDF_nY$unix_deathDate <- ifelse(survivalDF_nY$unix_deathDate > generatedLastDeath, generatedLastDeath, survivalDF_nY$unix_deathDate)
    simpleSurvivalPlot(survivalDF_nY, max(survivalDF_nY$unix_deathDate), 0.9, quantile(survivalDF_nY$mortProb)[3])
    
print('total deaths: '); print(sum(survivalDF_nY$isDead))
print('total deaths in high prob group: '); print(sum(subset(survivalDF_nY, mortProb > quantile(survivalDF_nY$mortProb)[3])$isDead))
summary(subset(survivalDF_nY, mortProb > quantile(survivalDF_nY$mortProb)[3]))
summary(subset(survivalDF_nY, mortProb <= quantile(survivalDF_nY$mortProb)[3]))


## factorised probablility
factorGroups = 4
survivalDF_nY$prob_factor <- with(survivalDF_nY, cut(mortProb, 
                                            breaks=quantile(mortProb, probs=seq(0,1, by=(1 / factorGroups)), na.rm=TRUE),
                                            include.lowest=TRUE))

simpleSurvivalPlot_factorisedProb(survivalDF_nY, max(survivalDF_nY$unix_deathDate), 0.8, survivalDF_nY$prob_factor)

sum(subset(survivalDF_nY, mortProb > 0.039)$isDead)
sum(subset(survivalDF_nY, mortProb <= 0.039)$isDead)

## smoothHR

survivalDF_nY_forSmoothHR <- survivalDF_nY
survivalDF_nY_forSmoothHR$mortProb = log(survivalDF_nY_forSmoothHR$mortProb)
smoothHR_plot(survivalDF_nY_forSmoothHR, max(survivalDF_nY_forSmoothHR$unix_deathDate))

# try and maximise return from mort Prob
# 
# referenceValue <- (-3.20344) # enter from smoothHR plot = T2
# 
referenceValue <- (-4.75457) # enter from smoothHR plot = T1
survivalDF_nY_forSmoothHR$adjustedMortProb <- sqrt((survivalDF_nY_forSmoothHR$mortProb - referenceValue)^2)

# for the t2 data - need to manually generate the 2 factor plot. not needed for t1
survivalDF_nY_forSmoothHR$mortProb <- survivalDF_nY_forSmoothHR$adjustedMortProb
simpleSurvivalPlot(survivalDF_nY_forSmoothHR, max(survivalDF_nY_forSmoothHR$unix_deathDate), 0.9, quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3])

# total deaths in the 2 groups
sum(subset(survivalDF_nY_forSmoothHR, adjustedMortProb > quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3])$isDead)
sum(subset(survivalDF_nY_forSmoothHR, adjustedMortProb <= quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3])$isDead)

# summary of groups
summary(subset(survivalDF_nY_forSmoothHR, adjustedMortProb > quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3]))
summary(subset(survivalDF_nY_forSmoothHR, adjustedMortProb <= quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3]))

# stats tests for submissions
wilcox.test(subset(survivalDF_nY_forSmoothHR, adjustedMortProb > quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3])$age, 
            subset(survivalDF_nY_forSmoothHR, adjustedMortProb <= quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3])$age)

wilcox.test(subset(survivalDF_nY_forSmoothHR, adjustedMortProb > quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3])$cv, 
            subset(survivalDF_nY_forSmoothHR, adjustedMortProb <= quantile(survivalDF_nY_forSmoothHR$adjustedMortProb)[3])$cv)



# factorGroups = 4
# survivalDF_nY_forSmoothHR$prob_factor <- with(survivalDF_nY_forSmoothHR, cut(mortProb, 
#                                                      breaks=quantile(mortProb, probs=seq(0,1, by=(1 / factorGroups)), na.rm=TRUE),
#                                                      include.lowest=TRUE))
# 
# simpleSurvivalPlot_factorisedProb(survivalDF_nY_forSmoothHR, max(survivalDF_nY_forSmoothHR$unix_deathDate), 0.8, survivalDF_nY_forSmoothHR$prob_factor)

# 
# factorGroups = 2
# survivalDF_nY_forSmoothHR$adjusted_prob_factor <- with(survivalDF_nY_forSmoothHR, cut(adjustedMortProb, breaks=quantile(adjustedMortProb, probs=seq(0, 1, by=(1 / factorGroups)), na.rm=TRUE), include.lowest=TRUE))
# 
# simpleSurvivalPlot_factorisedProb(survivalDF_nY_forSmoothHR, max(survivalDF_nY_forSmoothHR$unix_deathDate), 0.9, survivalDF_nY_forSmoothHR$adjusted_prob_factor)
# 

