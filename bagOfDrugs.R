source("~/R/_workingDirectory/_perAdmissionRewriteDataTableFunctions.R")
library(gtools)
library(igraph)
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

# generate node and link files
drugDataSet <- read.csv("~/R/GlCoSy/SDsource/Export_all_diabetes_drugs.txt",header=TRUE,row.names=NULL)

# drugDataSet <- read.csv("~/R/GlCoSy/SDsource/test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)
drugDataSet$BNFCode <- as.character(drugDataSet$BNFCode)
drugDataSet$DrugName <- as.character(drugDataSet$DrugName)
drugDataSet$LinkId <- as.numeric(levels(drugDataSet$LinkId))[drugDataSet$LinkId]
# drugDataSet$LinkId <- as.numeric(drugDataSet$LinkId)
# drugDataSet <- read.csv("./test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)

# restrict to diabetes drugs
interestSet <- subset(drugDataSet, substr(drugDataSet$BNFCode,1,3) == "6.1" | substr(drugDataSet$BNFCode,1,4) == "0601")
interestSet <- findSimilarDrugs(interestSet)
interestSetDT <- data.table(interestSet)
interestSetDT$prescription_dateplustime1 <- returnUnixDateTime(interestSetDT$PrescriptionDateTime)

# limit analysis period of interest to 10y period 1/1/2005 - 1/1/2015
interestSetDT <- interestSetDT[prescription_dateplustime1 > returnUnixDateTime('2006-01-01') &
                         prescription_dateplustime1 < returnUnixDateTime('2017-01-01')]

interestSetDF <- data.frame(interestSetDT)

# generate a top-100 etc list for merging back
# meeds a bit of data cleaning - merging synonymous drugs etc
n = 45
topNdrugs_DrugNames <- as.data.frame(table(interestSetDF$DrugName))
topNdrugs_DrugNames <- topNdrugs_DrugNames[order(topNdrugs_DrugNames$Freq), ]

topNdrugs <- tail(topNdrugs_DrugNames, n)

topNdrugs$Var1 <- gsub(" ", "", topNdrugs$Var1, fixed = TRUE)
topNdrugs$Var1 <- gsub("/", "", topNdrugs$Var1, fixed = TRUE)
topNdrugs$Var1 <- gsub("-", "", topNdrugs$Var1, fixed = TRUE)

# merge top drugs back with interestSet to generate working data frame:
interestSet_topN_merge <- merge(interestSetDF, topNdrugs, by.x="DrugName", by.y="Var1")

## start drug data manipulation

drugsetDT <- data.table(interestSet_topN_merge)
drugsetDT$prescription_dateplustime1 <- returnUnixDateTime(drugsetDT$PrescriptionDateTime)
drugsetDT_original <-drugsetDT # preserve an original full dataset incase needed
# drugsetDT$LinkId <- as.numeric(levels(drugsetDT$LinkId))[drugsetDT$LinkId]

# scale time to 0 to 1 range
drugsetDT$prescription_dateplustime1.original <- drugsetDT$prescription_dateplustime1
drugsetDT$prescription_dateplustime1 <- (drugsetDT$prescription_dateplustime1 - min(drugsetDT$prescription_dateplustime1)) / (max(drugsetDT$prescription_dateplustime1) - min(drugsetDT$prescription_dateplustime1))

drugsetDT <- transform(drugsetDT,id=as.numeric(factor(LinkId)))

sequence <- seq(0, 1 , 0.1)

# generate bag of drugs frame
drugWordFrame <- as.data.frame(matrix(nrow = length(unique(drugsetDT$LinkId)), ncol = (length(sequence)-1) ))

# function to generate drugwords for each time interval
returnIntervals <- function(inputSet, sequence) {
  
      ## add nil values to fill time slots without any drugs
      nilFrame <- as.data.frame(matrix(nrow = length(sequence), ncol = ncol(inputSet)))
      colnames(nilFrame) <- colnames(inputSet)
      
      nilFrame$DrugName <- nilValue
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
  
}

addNilDrugReferences <- function(inputSet, nilValue) {
  
  nilFrame <- as.data.frame(matrix(nrow = 10, ncol = ncol(inputSet)))
  colnames(nilFrame) <- colnames(inputSet)
  
  nilFrame$DrugName <- nilValue
  nilFrame$prescription_dateplustime1 <- seq(0, 1 - 0.1, 0.1)
  
  outputSet <- rbind(nilFrame, inputSet)
  
  return(outputSet)
  
}



