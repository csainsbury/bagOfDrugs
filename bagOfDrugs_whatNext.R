# source("~/R/_workingDirectory/_perAdmissionRewriteDataTableFunctions.R")
# library(gtools)
# library(igraph)
library(data.table)

id_per_location <- function(ID) {
  return(length(unique(ID)))
}

flagMove <- function(ID, charL) {
  
  charLreport <- charL
  charLnumeric <- as.numeric(factor(charL))
  
  testFrame <- data.frame(charLreport, charLnumeric)
  
  testFrame$flagMove <- 0
  testFrame$flagMove[1:nrow(testFrame)-1] <- diff(testFrame$charLnumeric)
  testFrame$nextL <- c("spacer")
  testFrame$nextL[1:(nrow(testFrame)-1)] <- charLreport[2:length(charLreport)]
  
  testFrame$charLreport <- as.character(factor(charL))
  
  outputList <- list(testFrame$charLreport, testFrame$nextL, testFrame$flagMove)
  
  return(outputList)
  
}

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

findSimilarDrugs <- function(inputFrame) {
  
  # inputFrame <- interestSet
  # inputFrame <- inputFrame[1:10000,]
  
  inputFrame$DrugName.original <- inputFrame$DrugName
  inputFrame$DrugNameNew <- inputFrame$DrugName
  
  inputFrame <- subset(inputFrame, DrugNameNew != "Disposable")
  
  inputFrame$DrugNameNew[grep("Glucose", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("Glucogel", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  
  inputFrame$DrugNameNew[grep("Glucagen Hypokit", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucagon"
  inputFrame$DrugNameNew[grep("Optium Plus", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  
  inputFrame$DrugNameNew[grep("Metformin", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin"
  inputFrame$DrugNameNew[grep("Glucophage", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin"
  
  inputFrame$DrugNameNew[grep("Gliclazide", inputFrame$DrugName, ignore.case = TRUE)] <- "Gliclazide"
  inputFrame$DrugNameNew[grep("Diamicron", inputFrame$DrugName, ignore.case = TRUE)] <- "Gliclazide"
  
  inputFrame$DrugNameNew[grep("Rosiglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "Rosiglitazone"
  inputFrame$DrugNameNew[grep("Avandia", inputFrame$DrugName, ignore.case = TRUE)] <- "Rosiglitazone"
  
  inputFrame$DrugNameNew[grep("Linagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "Linagliptin"
  
  inputFrame$DrugNameNew[grep("Victoza", inputFrame$DrugName, ignore.case = TRUE)] <- "Liraglutide"
  inputFrame$DrugNameNew[grep("Liraglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "Liraglutide"
  
  inputFrame$DrugNameNew[grep("Pioglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "Pioglitazone"
  
  inputFrame$DrugNameNew[grep("Sitagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "Sitagliptin"
  inputFrame$DrugNameNew[grep("Januvia", inputFrame$DrugName, ignore.case = TRUE)] <- "Sitagliptin"
  
  inputFrame$DrugNameNew[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "Dapagliflozin"
  
  inputFrame$DrugNameNew[grep("Humalog Mix 25", inputFrame$DrugName, ignore.case = TRUE)] <- "HumalogMix25"
  
  inputFrame$DrugNameNew[grep("Lantus", inputFrame$DrugName, ignore.case = TRUE)] <- "InsulinGlargine"
  inputFrame$DrugNameNew[grep("Levemir", inputFrame$DrugName, ignore.case = TRUE)] <- "InsulinDetemir"
  
  inputFrame$DrugNameNew[grep("Insulatard", inputFrame$DrugName, ignore.case = TRUE)] <- "HumanLongActing"
  inputFrame$DrugNameNew[grep("Ultratard", inputFrame$DrugName, ignore.case = TRUE)] <- "HumanLongActing"
  inputFrame$DrugNameNew[grep("Humulin I", inputFrame$DrugName, ignore.case = TRUE)] <- "HumanLongActing"
  
  
  inputFrame$DrugNameNew[grep("Actrapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Actrapid"
  inputFrame$DrugNameNew[grep("Humalog 100units/ml solution", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog"
  
  inputFrame$DrugNameNew[grep("Novorapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Novorapid"
  
  inputFrame$DrugNameNew[grep("Novomix 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Novomix30"
  
  inputFrame$DrugNameNew[grep("Mixtard 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard30"
  inputFrame$DrugNameNew[grep("Mixtard 20", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard20"
  
  inputFrame$DrugNameNew[grep("Humulin M3", inputFrame$DrugName, ignore.case = TRUE)] <- "HumulinM3"
  
  inputFrame$DrugNameNew[grep("Humalog Mix50", inputFrame$DrugName, ignore.case = TRUE)] <- "HumalogMix50"
  
  inputFrame$DrugNameNew[grep("strip", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  inputFrame$DrugNameNew[grep("Bd-Microfine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("Needle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  
  
  outputFrame <- inputFrame
  
  outputFrame$DrugName.original <- NULL
  outputFrame$DrugName <- outputFrame$DrugNameNew
  outputFrame$DrugNameNew <- NULL
  
  return(outputFrame)
}

# generate node and link files
drugDataSet <- read.csv("~/R/GlCoSy/SDsource/Export_all_diabetes_drugs.txt",header=TRUE,row.names=NULL)

# load and process mortality data
deathData <- read.csv("~/R/GlCoSy/SDsource/diagnosisDateDeathDate.txt", sep=",")
deathData$unix_deathDate <- returnUnixDateTime(deathData$DeathDate)
deathData$unix_deathDate[is.na(deathData$unix_deathDate)] <- 0
deathData$isDead <- ifelse(deathData$unix_deathDate > 0, 1, 0)
deathData$unix_diagnosisDate <- returnUnixDateTime(deathData$DateOfDiagnosisDiabetes_Date)

# drugDataSet <- read.csv("~/R/GlCoSy/SDsource/test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)
drugDataSet$BNFCode <- as.character(drugDataSet$BNFCode)
drugDataSet$DrugName <- as.character(drugDataSet$DrugName)
drugDataSet$LinkId <- as.numeric(levels(drugDataSet$LinkId))[drugDataSet$LinkId]
# drugDataSet$LinkId <- as.numeric(drugDataSet$LinkId)
# drugDataSet <- read.csv("./test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)

# restrict to diabetes drugs
interestSet <- subset(drugDataSet, substr(drugDataSet$BNFCode,1,3) == "6.1" | substr(drugDataSet$BNFCode,1,4) == "0601" | substr(drugDataSet$BNFCode,1,5) == "06.01")
interestSet <- findSimilarDrugs(interestSet)
interestSetDT <- data.table(interestSet)
interestSetDT$prescription_dateplustime1 <- returnUnixDateTime(interestSetDT$PrescriptionDateTime)

interestSetDT_original <- interestSetDT # run from here if altering runin period

# set runin period of interest
# startRuninPeriod <- '2002-01-01'
endRuninPeriod   <- '2015-01-01'

# testDeathDate    <- '2013-01-01'

# interestSetDT <- interestSetDT[prescription_dateplustime1 > returnUnixDateTime(startRuninPeriod) & prescription_dateplustime1 < returnUnixDateTime(endRuninPeriod)]

interestSetDF <- data.frame(interestSetDT)

# generate a top-100 etc list for merging back
# meeds a bit of data cleaning - merging synonymous drugs etc
n = 45
topNdrugs_DrugNames <- as.data.frame(table(interestSetDF$DrugName))
topNdrugs_DrugNames <- topNdrugs_DrugNames[order(topNdrugs_DrugNames$Freq), ]

topNdrugs <- tail(topNdrugs_DrugNames, n)

# topNdrugs$Var1 <- gsub(" ", "", topNdrugs$Var1, fixed = TRUE)
# topNdrugs$Var1 <- gsub("/", "", topNdrugs$Var1, fixed = TRUE)
# topNdrugs$Var1 <- gsub("-", "", topNdrugs$Var1, fixed = TRUE)

# merge top drugs back with interestSet to generate working data frame:
interestSet_topN_merge <- merge(interestSetDF, topNdrugs, by.x="DrugName", by.y="Var1")

###############################
## start drug data manipulation
###############################

### find prescription date of interest agents

findFirstSGLT2 <- function(prescription_dateplustime1, flagSGLT2use) {
  x <- data.table(prescription_dateplustime1, flagSGLT2use)
  x <- x[flagSGLT2use == 1]
  
  if (nrow(x) > 0) {
    firstPrescription <- min(x$prescription_dateplustime1)
  }
  
  if (nrow(x) == 0) {
    firstPrescription <- 0
  }
  
  return(firstPrescription)
}

drugsetDT_firstPrescribed <- interestSet_topN_merge
drugsetDT_firstPrescribed <- data.table(drugsetDT_firstPrescribed)

drugsetDT_firstPrescribed$flagSGLT2use <- ifelse(drugsetDT_firstPrescribed$DrugName == "Empagliflozin", 1, 0)
drugsetDT_firstPrescribed$flagSGLT2use <- ifelse(drugsetDT_firstPrescribed$DrugName == "Dapagliflozin", 1, drugsetDT_firstPrescribed$flagSGLT2use)

drugsetDT_firstPrescribed[, c("firstSGLT2Prescription") :=  findFirstSGLT2(prescription_dateplustime1, flagSGLT2use), by=.(LinkId)]

# limit set to SGLT2 prescribed IDs
drugsetDT_firstPrescribed_SGLT2only <- drugsetDT_firstPrescribed[firstSGLT2Prescription > 0]
# cut all data after first prescription
drugsetDT_firstPrescribed_SGLT2only <- drugsetDT_firstPrescribed_SGLT2only[prescription_dateplustime1 <= firstSGLT2Prescription]

#######################################
# start analysis
######################################
drugsetDT <- drugsetDT_firstPrescribed_SGLT2only
# drugsetDT$prescription_dateplustime1 <- returnUnixDateTime(drugsetDT$PrescriptionDateTime)
# drugsetDT_original <-drugsetDT # preserve an original full dataset incase needed
# drugsetDT$LinkId <- as.numeric(levels(drugsetDT$LinkId))[drugsetDT$LinkId]

drugsetDT <- drugsetDT[prescription_dateplustime1 > returnUnixDateTime("2005-01-01") & prescription_dateplustime1 < returnUnixDateTime("2017-01-01")]

# scale time to 0 to 1 range
drugsetDT$prescription_dateplustime1.original <- drugsetDT$prescription_dateplustime1

drugsetDT$prescription_dateplustime1 <- (drugsetDT$prescription_dateplustime1 - min(drugsetDT$prescription_dateplustime1)) / (max(drugsetDT$prescription_dateplustime1) - min(drugsetDT$prescription_dateplustime1))
# drugsetDT$LinkId<-as.numeric(levels(drugsetDT$LinkId))[drugsetDT$LinkId]
# drugsetDT$LinkId[is.na(drugsetDT$LinkId)] <- 0
# drugsetDT <-  drugsetDT[LinkId > 0]

# read out and in for testing
# write.table(drugsetDT, file = "~/R/GlCoSy/MLsource/drugsetDT_2002to12.csv", sep=",", row.names = FALSE)
# drugsetDT <- read.csv("~/R/GlCoSy/MLsource/drugsetDT.csv", stringsAsFactors = F, row.names = NULL); drugsetDT$row.names <- NULL; drugsetDT$diffLinkId <- NULL; drugsetDT <- data.table(drugsetDT)

drugsetDT <- transform(drugsetDT,id=as.numeric(factor(LinkId)))

# drugsetDT <- drugsetDT[prescription_dateplustime1.original > returnUnixDateTime('2005-01-01') & prescription_dateplustime1.original < returnUnixDateTime('2015-01-01')]

# calculate time interval:
interval <- max(drugsetDT$prescription_dateplustime1.original)- min(drugsetDT$prescription_dateplustime1.original)
intervalYears <- interval / (60*60*24*365.25)

# months = 3



# set time bins
# 
sequence <- seq(0, 1 , (1/40)) # 10y runin - in 3 month blocks
# sequence <- seq(0, 1 , 0.1) # 10y runin - in 12 month blocks
# sequence <- seq(0, 1 , (1/125)) # 10y runin - in 3 month blocks



# generate bag of drugs frame
drugWordFrame <- as.data.frame(matrix(nrow = length(unique(drugsetDT$LinkId)), ncol = (length(sequence)-1) ))
colnames(drugWordFrame) <- c(1:(length(sequence)-1))
drugWordFrame$LinkId <- 0

# function to generate drugwords for each time interval
returnIntervals <- function(LinkId, DrugName, prescription_dateplustime1, sequence, id) {
  
  # DrugName <- subset(drugsetDT, id == 2)$DrugName; prescription_dateplustime1 <- subset(drugsetDT, id == 2)$prescription_dateplustime1; id = 2; LinkId <- subset(drugsetDT, id == 2)$LinkId
  
  inputSet <- data.table(DrugName, prescription_dateplustime1)
  
  ## add nil values to fill time slots without any drugs
  nilFrame <- as.data.frame(matrix(nrow = length(sequence), ncol = ncol(inputSet)))
  colnames(nilFrame) <- colnames(inputSet)
  
  nilFrame$DrugName <- 'nil'
  nilFrame$prescription_dateplustime1 <- sequence
  
  outputSet <- rbind(nilFrame, inputSet)
  
  ## generate drug words
  
  interimSet <- outputSet
  
  interimSet <- interimSet[, interv := cut(prescription_dateplustime1, sequence)][, .(drugs = (unique(DrugName))), by = interv]
  interimSet[, drugWord := paste(drugs, collapse = ''), by = interv]
  
  interimSet <- interimSet[order(interimSet$interv), ]
  interimSet[, drugSequenceNumber := seq(1, .N, 1), by = interv]
  
  reportSet <- interimSet[drugSequenceNumber == 1]
  reportSet$drugWord <- ifelse(substr(reportSet$drugWord,1,3) == 'nil' & nchar(reportSet$drugWord) == 3, reportSet$drugWord, substr(reportSet$drugWord,4,nchar(reportSet$drugWord)))
  
  reportSet <- reportSet[1:nrow(reportSet)-1, ]
  reportSet$intervalNumber <- c(1:nrow(reportSet))
  
  #      print(reportSet$drugWord)
  
  return(c(reportSet$drugWord, LinkId[1]))
  
  
}

for (j in seq(1, max(drugsetDT$id), )) {
  
  if(j%%100 == 0) {print(j)}
  
  injectionSet <- drugsetDT[id == j]
  drugWordFrame[j, ] <- returnIntervals(injectionSet$LinkId, injectionSet$DrugName, injectionSet$prescription_dateplustime1, sequence, j)
}


drugWordFrame[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "1"




#import hba1c data
cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanHbA1cData$timeSeriesDataPoint <- cleanHbA1cData$hba1cNumeric
cleanHbA1cDataDT <- data.table(cleanHbA1cData)

# file to find hba1c values for 
findHbA1cValues <- function(LinkId_value, firstSGLT2Prescription, firstWindowMonths, IntervalMonths) {
  
 # print(LinkId_value)
  
  firstSGLT2time <- firstSGLT2Prescription[1]
 
  firstWindowSeconds <- firstWindowMonths * (60*60*24*(365.25/12))
  IntervalSeconds <- IntervalMonths * (60*60*24*(365.25/12))
  
  hb_sub <- cleanHbA1cDataDT[LinkId == LinkId_value]
  
  # find 1st hba1c
  hb_sub$firstDiff <- hb_sub$dateplustime1 - firstSGLT2time
  first_hb_sub <- hb_sub[sqrt(firstDiff^2) < (firstWindowSeconds/2)]
  if (nrow(first_hb_sub) > 0) {firstHb <- first_hb_sub[sqrt(firstDiff^2) == min(sqrt(firstDiff^2))]$timeSeriesDataPoint}
  if (nrow(first_hb_sub) == 0) {firstHb = 0}
  
  # find 2nd hba1c
  hb_sub$secondDiff <- hb_sub$dateplustime1 - (firstSGLT2time + IntervalSeconds)
  second_hb_sub <- hb_sub[sqrt(secondDiff^2) < (firstWindowSeconds/2)]
  if (nrow(second_hb_sub) > 0) {secondHb <- second_hb_sub[sqrt(firstDiff^2) == min(sqrt(firstDiff^2))]$timeSeriesDataPoint}
  if (nrow(second_hb_sub) == 0) {secondHb = 0}
  
  returnList <- list(firstHb, secondHb)
  
  return(returnList)
  
}

drugsetDT[, c("firstHbA1c", "secondHbA1c") :=  findHbA1cValues(LinkId, firstSGLT2Prescription, 3, 12), by=.(LinkId)]

drugsetDT$include <- ifelse(drugsetDT$firstHbA1c > 0 & drugsetDT$secondHbA1c >0, 1, 0)
drugsetDT$y <- ifelse(drugsetDT$include == 1 & (drugsetDT$firstHbA1c - drugsetDT$secondHbA1c) >= 10, 1, 0)

# flag single row per ID for merging back with combination data
drugsetDT$index <- seq(1, nrow(drugsetDT), 1)
drugsetDT[, c("firstRow") :=  ifelse(index == min(index), 1, 0), by=.(LinkId)]

meetsCriteriaDT <- drugsetDT[include == 1 & firstRow == 1]
mergeSet <- data.frame(meetsCriteriaDT$LinkId, meetsCriteriaDT$y); colnames(mergeSet) <- c("LinkId", "y")

exportMerge <- merge(mergeSet, drugWordFrame, by.x = "LinkId", by.y = "LinkId")

########################
# write drug names into numbers

  drugWordFrame_drugNames <- exportMerge[,3:ncol(exportMerge)]
  
  drugSentenceFrame <- as.data.frame(matrix(nrow = nrow(drugWordFrame_drugNames), ncol = 1))
  colnames(drugSentenceFrame) <- c("drugSentence")
  
  vectorWords <- as.vector(as.matrix(drugWordFrame_drugNames))
  vectorNumbers <- as.numeric(as.factor(vectorWords))
  lookup <- data.frame(vectorWords, vectorNumbers)
  lookup <- unique(lookup)
  lookup <- data.table(lookup)

  # vectorised lookup table use
  numericalDrugsFrame <- as.data.frame(matrix(0, nrow = nrow(drugWordFrame_drugNames), ncol = ncol(drugWordFrame_drugNames)))
  
  for (jj in seq(1, ncol(drugWordFrame_drugNames), 1)) {
    
    index <- match(drugWordFrame_drugNames[,jj], lookup$vectorWords)
    numericalDrugsFrame[,jj] <- lookup$vectorNumbers[index]
    
  }


# write out sequence for analysis
write.table(numericalDrugsFrame, file = "~/R/GlCoSy/MLsource/SGLT2_X.csv", sep=",", row.names = FALSE)

# write out dep variable (y)
write.table(exportMerge$y, file = "~/R/GlCoSy/MLsource/SGLT2_y.csv", sep = ",", row.names = FALSE)



















# write.table(drugWordFrame, file = "~/R/GlCoSy/MLsource/drugWordFrame_withID_2005_2015.csv", sep=",")
# drugWordFrame <- read.csv("~/R/GlCoSy/MLsource/drugWordFrame.csv", stringsAsFactors = F, row.names = NULL); drugWordFrame$row.names <- NULL

# here do analysis to select rows (IDs) for later analysis

# mortality outcome at 2017-01-01
drugWordFrame_mortality <- merge(drugWordFrame, deathData, by.x = "LinkId", by.y= "LinkId")
# remove those dead before end of FU
# analysis frame = those who are not dead, or those who have died after the end of the runin period. ie all individuals in analysis alive at the end of the runin period
drugWordFrame_mortality <- subset(drugWordFrame_mortality, isDead == 0 | (isDead == 1 & unix_deathDate > returnUnixDateTime(endRuninPeriod)) )
# remove those diagnosed after the end of the runin period
drugWordFrame_mortality <- subset(drugWordFrame_mortality, unix_diagnosisDate <= returnUnixDateTime(endRuninPeriod) )
# remove those diagnosed after the beginning of the runin period ie all in analysis have had DM throughout followup period
# drugWordFrame_mortality <- subset(drugWordFrame_mortality, unix_diagnosisDate <= returnUnixDateTime(startRuninPeriod) )


# set up drug sentences for analysis

drugWordFrame_forAnalysis <- drugWordFrame_mortality

drugWordFrame_drugNames <- drugWordFrame_forAnalysis[, 2:(1+(length(sequence)-1)) ]

drugSentenceFrame <- as.data.frame(matrix(nrow = nrow(drugWordFrame_forAnalysis), ncol = 1))
colnames(drugSentenceFrame) <- c("drugSentence")

vectorWords <- as.vector(as.matrix(drugWordFrame_drugNames))
vectorNumbers <- as.numeric(as.factor(vectorWords))
lookup <- data.frame(vectorWords, vectorNumbers)
lookup <- unique(lookup)
lookup <- data.table(lookup)

# vectorised lookup table use
numericalDrugsFrame <- as.data.frame(matrix(0, nrow = nrow(drugWordFrame_drugNames), ncol = ncol(drugWordFrame_drugNames)))

for (jj in seq(1, ncol(drugWordFrame_drugNames), 1)) {
  
  index <- match(drugWordFrame_drugNames[,jj], lookup$vectorWords)
  numericalDrugsFrame[,jj] <- lookup$vectorNumbers[index]
  
}

y_vector <- drugWordFrame_forAnalysis$isDead
y_vector_isType1 <- ifelse(drugWordFrame_forAnalysis$DiabetesMellitusType_Mapped == 'Type 1 Diabetes Mellitus', 1, 0)
y_vector_deadAt_1_year <- ifelse(drugWordFrame_forAnalysis$isDead == 1 & drugWordFrame_forAnalysis$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (1 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_2_year <- ifelse(drugWordFrame_forAnalysis$isDead == 1 & drugWordFrame_forAnalysis$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (2 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_3_year <- ifelse(drugWordFrame_forAnalysis$isDead == 1 & drugWordFrame_forAnalysis$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (3 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_4_year <- ifelse(drugWordFrame_forAnalysis$isDead == 1 & drugWordFrame_forAnalysis$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (4 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_5_year <- ifelse(drugWordFrame_forAnalysis$isDead == 1 & drugWordFrame_forAnalysis$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (5 * 365.25 * 24 * 60 * 60)), 1, 0)

# write out sequence for analysis
write.table(numericalDrugsFrame, file = "~/R/GlCoSy/MLsource/drugs_10y_runin.csv", sep=",", row.names = FALSE)

# write out sequence for analysis with LinkId
write.table(drugWordFrame, file = "~/R/GlCoSy/MLsource/drugs_22y_runin_rawWithId.csv", sep=",", row.names = FALSE)


# write out dep variable (y)
write.table(y_vector, file = "~/R/GlCoSy/MLsource/drugs_10y_runin_5y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_isType1, file = "~/R/GlCoSy/MLsource/isType1_drugs_10y_runin.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_1_year, file = "~/R/GlCoSy/MLsource/drugs_10y_runin_1y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_2_year, file = "~/R/GlCoSy/MLsource/drugs_10y_runin_2y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_3_year, file = "~/R/GlCoSy/MLsource/drugs_10y_runin_3y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_4_year, file = "~/R/GlCoSy/MLsource/drugs_10y_runin_4y_mortality.csv", sep = ",", row.names = FALSE)




