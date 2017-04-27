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

# generate node and link files
cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanHbA1cData$timeSeriesDataPoint <- cleanHbA1cData$hba1cNumeric

cleanSBPData <- read.csv("~/R/GlCoSy/SD_workingSource/SBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanSBPData$timeSeriesDataPoint <- cleanSBPData$sbpNumeric

cleanDBPData <- read.csv("~/R/GlCoSy/SD_workingSource/DBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanDBPData$timeSeriesDataPoint <- cleanDBPData$dbpNumeric

cleanBMIData <- read.csv("~/R/GlCoSy/SD_workingSource/BMISetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanBMIData$timeSeriesDataPoint <- cleanBMIData$bmiNumeric

# timeSeriesData <- cleanHbA1cData
# timeSeriesData <- cleanSBPData
# timeSeriesData <- cleanDBPData
# 
timeSeriesData <- cleanBMIData

timeSeriesDataDT <- data.table(timeSeriesData)


# load and process mortality data
deathData <- read.csv("~/R/GlCoSy/SDsource/diagnosisDateDeathDate.txt", sep=",")
deathData$unix_deathDate <- returnUnixDateTime(deathData$DeathDate)
deathData$unix_deathDate[is.na(deathData$unix_deathDate)] <- 0
deathData$isDead <- ifelse(deathData$unix_deathDate > 0, 1, 0)
deathData$unix_diagnosisDate <- returnUnixDateTime(deathData$DateOfDiagnosisDiabetes_Date)


# set runin period of interest
startRuninPeriod <- '2002-01-01'
endRuninPeriod   <- '2012-01-01'

# testDeathDate    <- '2013-01-01'

interestSetDT <- timeSeriesDataDT[dateplustime1 > returnUnixDateTime(startRuninPeriod) &
                                    dateplustime1 < returnUnixDateTime(endRuninPeriod)]

interestSetDF <- data.frame(interestSetDT)


###############################
## start data manipulation
###############################

# scale time to 0 to 1 range
interestSetDT$dateplustime1.original <- interestSetDT$dateplustime1
interestSetDT$dateplustime1 <- (interestSetDT$dateplustime1 - min(interestSetDT$dateplustime1)) / (max(interestSetDT$dateplustime1) - min(interestSetDT$dateplustime1))

interestSetDT <- transform(interestSetDT,id=as.numeric(factor(LinkId)))

# set time bins
# sequence <- seq(0, 1 , (1/40)) # 10y runin - in 3 month blocks
# sequence <- seq(0, 1 , 0.1) # 10y runin - in 12 month blocks
# 
sequence <- seq(0, 1 , (1/20)) # 10y runin - in 6 month blocks
# sequence <- seq(0, 1 , (1/125)) # 10y runin - in 1 month blocks

# generate bag of drugs frame
timesetWordFrame <- as.data.frame(matrix(nrow = length(unique(interestSetDT$LinkId)), ncol = (length(sequence)-1) ))
colnames(timesetWordFrame) <- c(1:(length(sequence)-1))
timesetWordFrame$LinkId <- 0

# function to generate drugwords for each time interval
returnIntervals <- function(LinkId, timeSeriesDataPoint, dateplustime1, sequence, id) {
  
  # timeSeriesDataPoint <- subset(interestSetDT, id == 2)$timeSeriesDataPoint; dateplustime1 <- subset(interestSetDT, id == 2)$dateplustime1; id = 2; LinkId <- subset(interestSetDT, id == 2)$LinkId
  
  inputSet <- data.table(timeSeriesDataPoint, dateplustime1)
  
  ## add nil values to fill time slots without any drugs
  nilFrame <- as.data.frame(matrix(nrow = length(sequence), ncol = ncol(inputSet)))
  colnames(nilFrame) <- colnames(inputSet)
  
  
  nilFrame$timeSeriesDataPoint <- 0
  nilFrame$dateplustime1 <- sequence
  
  outputSet <- rbind(nilFrame, inputSet)
  
  dataBreaks <- split(outputSet$timeSeriesDataPoint, cut(outputSet$dateplustime1, breaks = sequence))
  outputVector <- c(rep(0, length(sequence)- 1))
  
  for (kk in seq(1, length(dataBreaks), 1)) {
    values <- dataBreaks[[kk]]
    if (length(values) == 1) { outputVector[kk] = 0}
    if (length(values) > 0) { outputVector[kk] = quantile(values[values > 0])[3]}
  }
  
  return(c(outputVector, LinkId[1]))

}

for (j in seq(1, max(interestSetDT$id), )) {
  
  if(j%%100 == 0) {print(j)}
  
  injectionSet <- interestSetDT[id == j]
  timesetWordFrame[j, ] <- returnIntervals(injectionSet$LinkId, injectionSet$timeSeriesDataPoint, injectionSet$dateplustime1, sequence, j)
}

# need to add method of handling NAs - turn to 0
timesetWordFrame[is.na(timesetWordFrame)] <- 0

# write.table(drugWordFrame, file = "~/R/GlCoSy/MLsource/drugWordFrame_withID_2005_2015.csv", sep=",")
# drugWordFrame <- read.csv("~/R/GlCoSy/MLsource/drugWordFrame.csv", stringsAsFactors = F, row.names = NULL); drugWordFrame$row.names <- NULL

# here do analysis to select rows (IDs) for later analysis

# mortality outcome at 2017-01-01
timesetWordFrame_mortality <- merge(timesetWordFrame, deathData, by.x = "LinkId", by.y= "LinkId")
# remove those dead before end of FU
# analysis frame = those who are not dead, or those who have died after the end of the runin period. ie all individuals in analysis alive at the end of the runin period
timesetWordFrame_mortality <- subset(timesetWordFrame_mortality, isDead == 0 | (isDead == 1 & unix_deathDate > returnUnixDateTime(endRuninPeriod)) )
# remove those diagnosed after the end of the runin period
timesetWordFrame_mortality <- subset(timesetWordFrame_mortality, unix_diagnosisDate <= returnUnixDateTime(endRuninPeriod) )
# remove those diagnosed after the beginning of the runin period ie all in analysis have had DM throughout followup period
# timesetWordFrame_mortality <- subset(timesetWordFrame_mortality, unix_diagnosisDate <= returnUnixDateTime(startRuninPeriod) )

timesetWordFrame_forAnalysis <- timesetWordFrame_mortality[, 2:length(sequence)]

y_vector <- timesetWordFrame_mortality$isDead
y_vector_isType1 <- ifelse(timesetWordFrame_mortality$DiabetesMellitusType_Mapped == 'Type 1 Diabetes Mellitus', 1, 0)
y_vector_deadAt_1_year <- ifelse(timesetWordFrame_mortality$isDead == 1 & timesetWordFrame_mortality$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (1 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_2_year <- ifelse(timesetWordFrame_mortality$isDead == 1 & timesetWordFrame_mortality$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (2 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_3_year <- ifelse(timesetWordFrame_mortality$isDead == 1 & timesetWordFrame_mortality$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (3 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_4_year <- ifelse(timesetWordFrame_mortality$isDead == 1 & timesetWordFrame_mortality$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (4 * 365.25 * 24 * 60 * 60)), 1, 0)
y_vector_deadAt_5_year <- ifelse(timesetWordFrame_mortality$isDead == 1 & timesetWordFrame_mortality$unix_deathDate < (returnUnixDateTime(endRuninPeriod) + (5 * 365.25 * 24 * 60 * 60)), 1, 0)

# write out sequence for analysis
write.table(timesetWordFrame_forAnalysis, file = "~/R/GlCoSy/MLsource/bmi_10y_2002to2012_6mBins_chained_y.csv", sep=",", row.names = FALSE)

# write out sequence for analysis with LinkId
write.table(timesetWordFrame_mortality, file = "~/R/GlCoSy/MLsource/bmi_10y_2002to2012_6mBins_chained_y_rawWithId.csv", sep=",", row.names = FALSE)


# write out dep variable (y)
write.table(y_vector, file = "~/R/GlCoSy/MLsource/bmi_10y_runin_5y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_isType1, file = "~/R/GlCoSy/MLsource/isType1_bmi_10y_runin.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_1_year, file = "~/R/GlCoSy/MLsource/bmi_10y_runin_1y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_2_year, file = "~/R/GlCoSy/MLsource/bmi_10y_runin_2y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_3_year, file = "~/R/GlCoSy/MLsource/bmi_10y_runin_3y_mortality.csv", sep = ",", row.names = FALSE)
write.table(y_vector_deadAt_4_year, file = "~/R/GlCoSy/MLsource/bmi_10y_runin_4y_mortality.csv", sep = ",", row.names = FALSE)




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


