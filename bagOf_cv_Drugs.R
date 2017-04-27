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
  
  inputFrame$DrugNameNew[grep("Humalog Mix25", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog Mix 25"
  
  inputFrame$DrugNameNew[grep("Lantus", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulin Glargine"
  inputFrame$DrugNameNew[grep("Levemir", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulin Detemir"
  
  inputFrame$DrugNameNew[grep("Insulatard", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulatard"
  
  inputFrame$DrugNameNew[grep("Actrapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Actrapid"
  inputFrame$DrugNameNew[grep("Humalog 100units/ml solution", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog"
  
  inputFrame$DrugNameNew[grep("Novorapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Novorapid"
  
  inputFrame$DrugNameNew[grep("Novomix 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Novomix 30"
  
  inputFrame$DrugNameNew[grep("Mixtard 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard 30"
  inputFrame$DrugNameNew[grep("Mixtard 20", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard 20"
  
  inputFrame$DrugNameNew[grep("Humulin M3", inputFrame$DrugName, ignore.case = TRUE)] <- "Humulin M3"
  
  inputFrame$DrugNameNew[grep("Humalog Mix50", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog Mix50"
  
  inputFrame$DrugNameNew[grep("strip", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  inputFrame$DrugNameNew[grep("Bd-Microfine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("Needle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  
  
  outputFrame <- inputFrame
  
  outputFrame$DrugName.original <- NULL
  outputFrame$DrugName <- outputFrame$DrugNameNew
  outputFrame$DrugNameNew <- NULL
  
  return(outputFrame)
}

drugsByBNF_code <- function(inputFrame) {
  
  # inputFrame <- interestSet
  # inputFrame <- inputFrame[1:10000,]
  
  inputFrame$DrugName.original <- inputFrame$DrugName
  inputFrame$DrugNameNew <- inputFrame$DrugName
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,3) == "2.1" | substr(inputFrame$BNFCode,1,4) == "0201" | substr(inputFrame$BNFCode,1,5) == "02.01", "PositiveIonotrope", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.2.1" | substr(inputFrame$BNFCode,1,6) == "020201" | substr(inputFrame$BNFCode,1,8) == "02.02.01", "ThiazideDiuretic", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.2.2" | substr(inputFrame$BNFCode,1,6) == "020202" | substr(inputFrame$BNFCode,1,8) == "02.02.02", "LoopDiuretic", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.2.3" | substr(inputFrame$BNFCode,1,6) == "020203" | substr(inputFrame$BNFCode,1,8) == "02.02.03" | substr(inputFrame$BNFCode,1,5) == "2.2.4" | substr(inputFrame$BNFCode,1,6) == "020204" | substr(inputFrame$BNFCode,1,5) == "2.2.5" | substr(inputFrame$BNFCode,1,6) == "020205" | substr(inputFrame$BNFCode,1,8) == "02.02.05", "OtherDiuretic", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,3) == "2.3" | substr(inputFrame$BNFCode,1,4) == "0203" | substr(inputFrame$BNFCode,1,5) == "02.03", "AntiArythmic", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,3) == "2.4" | substr(inputFrame$BNFCode,1,4) == "0204" | substr(inputFrame$BNFCode,1,5) == "02.04", "BetaBlocker", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.5.1" | substr(inputFrame$BNFCode,1,6) == "020501" | substr(inputFrame$BNFCode,1,8) == "02.05.01", "VasodilatorAntiHypertensive", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.5.2" | substr(inputFrame$BNFCode,1,6) == "020502" | substr(inputFrame$BNFCode,1,8) == "02.05.02", "CentralActingAntiHypertensive", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.5.3" | substr(inputFrame$BNFCode,1,6) == "020503" | substr(inputFrame$BNFCode,1,8) == "02.05.03", "AdrenergicNeuroneBlocker", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.5.4" | substr(inputFrame$BNFCode,1,6) == "020504" | substr(inputFrame$BNFCode,1,8) == "02.05.04", "AlphaBlocker", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.5.5" | substr(inputFrame$BNFCode,1,6) == "020505" | substr(inputFrame$BNFCode,1,8) == "02.05.05", "ReninAngiotensinDrugs", inputFrame$DrugNameNew)
                                   
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.6.1" | substr(inputFrame$BNFCode,1,6) == "020601" | substr(inputFrame$BNFCode,1,8) == "02.06.01", "Nitrates", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.6.2" | substr(inputFrame$BNFCode,1,6) == "020602" | substr(inputFrame$BNFCode,1,8) == "02.06.02", "CalciumChannelBlocker", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.6.3" | substr(inputFrame$BNFCode,1,6) == "020603" | substr(inputFrame$BNFCode,1,8) == "02.06.03", "OtherAntiAnginal", inputFrame$DrugNameNew)
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,5) == "2.6.4" | substr(inputFrame$BNFCode,1,6) == "020604" | substr(inputFrame$BNFCode,1,8) == "02.06.04", "PeripheralVasodilators", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,3) == "2.7" | substr(inputFrame$BNFCode,1,4) == "0207" | substr(inputFrame$BNFCode,1,5) == "02.07", "Sympathomimetics", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,3) == "2.8" | substr(inputFrame$BNFCode,1,4) == "0208" | substr(inputFrame$BNFCode,1,5) == "02.08", "Anticoagulants", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,3) == "2.9" | substr(inputFrame$BNFCode,1,4) == "0209" | substr(inputFrame$BNFCode,1,5) == "02.09", "AntiPlatelets", inputFrame$DrugNameNew)
  
  inputFrame$DrugNameNew <- ifelse(substr(inputFrame$BNFCode,1,4) == "2.12" | substr(inputFrame$BNFCode,1,4) == "0212" | substr(inputFrame$BNFCode,1,5) == "02.12", "LipidLowering", inputFrame$DrugNameNew)

  outputFrame <- inputFrame
  
  outputFrame$DrugName.original <- NULL
  outputFrame$DrugName <- outputFrame$DrugNameNew
  outputFrame$DrugNameNew <- NULL
  
  return(outputFrame)
}

# generate node and link files
drugDataSet <- read.csv("~/R/GlCoSy/SDsource/cv_drugs3.txt",header=TRUE,row.names=NULL)
# truncate the drugset for testing.
# drugDataSet <- drugDataSet[1:100000,]

drugDataSetDT <- data.table(drugDataSet)

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

# define interest set and switch to standardised class names for drugs
interestSet <- drugDataSet
interestSet <- drugsByBNF_code(interestSet)
interestSetDT <- data.table(interestSet)

# write out interestSet with stadardised names
# write.table(interestSet, file = "~/R/GlCoSy/MLsource/dataset_cvDrugs_standardNames.csv", sep=",", row.names = FALSE)
# interestSet <- read.csv("~/R/GlCoSy/MLsource/dataset_cvDrugs_standardNames.csv",header=TRUE,row.names=NULL)

interestSet$prescription_dateplustime1 <- returnUnixDateTime(interestSet$PrescriptionDateTime)

interestSetDT <- data.table(interestSet)
interestSetDT_original <- interestSetDT # run from here if altering runin period

# set runin period of interest
startRuninPeriod <- '2002-01-01'
endRuninPeriod   <- '2012-01-01'

testDeathDate    <- '2013-01-01'

interestSetDT <- interestSetDT[prescription_dateplustime1 > returnUnixDateTime(startRuninPeriod) &
                         prescription_dateplustime1 < returnUnixDateTime(endRuninPeriod)]


# constrain to standardised drug names:
interestSetDT <- interestSetDT[DrugName == "VasodilatorAntiHypertensive" |
                                 DrugName == "CentralActingAntiHypertensive" |
                                 DrugName == "PeripheralVasodilators" |
                                 DrugName == "AntiArythmic" |
                                 DrugName == "OtherDiuretic" |
                                 DrugName == "PositiveIonotrope" |
                                 DrugName == "OtherAntiAnginal" |
                                 DrugName == "Anticoagulants" |
                                 DrugName == "Nitrates" |
                                 DrugName == "LoopDiuretic" |
                                 DrugName == "ThiazideDiuretic" |
                                 DrugName == "CalciumChannelBlocker" |
                                 DrugName == "BetaBlocker" |
                                 DrugName == "AntiPlatelets" |
                                 DrugName == "ReninAngiotensinDrugs" |
                                 DrugName == "LipidLowering" |
                                 DrugName == "AlphaBlocker"
                               ]

rm(interestSet)
rm(interestSetDT_original)

###############################
## start drug data manipulation
###############################

drugsetDT <- interestSetDT
# drugsetDT$prescription_dateplustime1 <- returnUnixDateTime(drugsetDT$PrescriptionDateTime)
drugsetDT_original <-drugsetDT # preserve an original full dataset incase needed
# drugsetDT$LinkId <- as.numeric(levels(drugsetDT$LinkId))[drugsetDT$LinkId]

rm(interestSetDT)

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
    write.table(numericalDrugsFrame, file = "~/R/GlCoSy/MLsource/CVdrugsSmall_10ydiabetes.csv", sep=",", row.names = FALSE)
    
    # write out dep variable (y)
    write.table(y_vector, file = "~/R/GlCoSy/MLsource/CVdrugs_10ydiabetes_5y_mortality.csv", sep = ",", row.names = FALSE)
    write.table(y_vector_isType1, file = "~/R/GlCoSy/MLsource/isType1_CVdrugs_10ydiabetes.csv", sep = ",", row.names = FALSE)
    write.table(y_vector_deadAt_1_year, file = "~/R/GlCoSy/MLsource/CVdrugs_10ydiabetes_1y_mortality.csv", sep = ",", row.names = FALSE)
    write.table(y_vector_deadAt_2_year, file = "~/R/GlCoSy/MLsource/CVdrugsSmall_10ydiabetes_2y_mortality.csv", sep = ",", row.names = FALSE)
    write.table(y_vector_deadAt_3_year, file = "~/R/GlCoSy/MLsource/CVdrugs_10ydiabetes_3y_mortality.csv", sep = ",", row.names = FALSE)
    write.table(y_vector_deadAt_4_year, file = "~/R/GlCoSy/MLsource/CVdrugs_10ydiabetes_4y_mortality.csv", sep = ",", row.names = FALSE)
    
    
    
    
    # featureFrame_deathMerge <- merge(featureFrame, deathData, by.x= "LinkId", by.y = "LinkId", all.x = TRUE)
    # death_outcome <- featureFrame_deathMerge$deadPostEndOfDrugData
    # death_outcome[is.na(death_outcome)] <- 0
    # 
    # write.table(death_outcome, file = "~/R/GlCoSy/MLsource/deathOutcome_for_numericalDrugsFrame_20.csv", sep = ",")
    # 
    # # write.table(numericalDrugsFrame, file = "~/R/GlCoSy/MLsource/numericalDrugsFrame_20.csv", sep=",", row.names = FALSE)
    # # numericalDrugsFrame <- read.csv("~/R/GlCoSy/MLsource/numericalDrugsFrame_20.csv", stringsAsFactors = F, row.names = FALSE); numericalDrugsFrame$row.names <- NULL
    # 
    # # runif(10, 0, 1)
    # # random y_train and y_test
    # random_y <- runif(nrow(numericalDrugsFrame), 0, 1)
    # random_y <- ifelse(random_y < 0.5, 0, 1)
    # 
    # write.table(random_y, file = "~/R/GlCoSy/MLsource/random_y.csv", sep = ",")
    # 
    # # generate(test_train)
    # X_train <- numericalDrugsFrame[1:1000, ]
    # y_train <- random_y[1:1000]
    # 
    # X_test <- numericalDrugsFrame[1001:2001, ]
    # y_test <- random_y[1001:2001]
    # 
    
    
## need to convert words to numbers - use text proc library, and feed into rnn


