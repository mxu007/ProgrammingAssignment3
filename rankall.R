rankall <- function(outcome, num = "best") { 

	## Read outcome data
	## By colClasses, read.csv skips those columns with 'NULL' remark
	## In total, read.csv only reads five columns:
	## Hopital Names, State, Mortality Rates for heart attack, heart failure
	## and penumonia
	Data <- read.csv("outcome-of-care-measures.csv", colClasses = 
				c('NULL', 'character', rep('NULL', 4), 'character',
				rep('NULL', 3), 'character', rep('NULL', 5), 'character',
				rep('NULL', 5), 'character', rep('NULL', 23)
				))
	Data[,3] <- as.numeric(Data[,3])
	Data[,4] <- as.numeric(Data[,4])
	Data[,5] <- as.numeric(Data[,5])
	
	## Generate unique list from column G "State"
	## !duplicated() removes repetitive items in column 2
	StateList <- subset(Data[,2], !duplicated(Data[,2]))
	## Sort state list by alphabetical order
	StateList <- sort(StateList)
	
	## Init of vector hospitals as question requires the return of data frame
	## Init of vector states
	hospitals <- vector(mode = "character", length = length(StateList))
	states <- vector(mode = "character", length = length(StateList))
	
	## j is the index variable for looping 
	j <- 1
	## Check if outcome is valid, if not, stop with error message
	if (outcome == "heart attack") {
		outcome <- 3
	} else if (outcome == "heart failure") {
		outcome <- 4 
	} else if (outcome == "pneumonia") {
		outcome <- 5
	} else {
		stop("invalid outcome")
	}
	
	## looping inside StateList
	for (states in StateList) {
		StateData <- subset(Data, State == states)
		StateData <- StateData[order(StateData[,outcome], StateData[,1], na.last = TRUE),]
		if (num == "best") {
			SeqNo <- 1
		} else if (num == "worst") {
			SeqNo <- max(which(!is.na(StateData[,outcome])))
		} else if (num >= 1 && num <= max(which(!is.na(StateData[,outcome])))) {
			SeqNo <- num
		} else {
			hospitals[j] <- "NA"
		}
		## assign hospital column data with the input rank to vector hospitals
		hospitals[j] <- (StateData[SeqNo,1])
		## index j update
		j <- j + 1
	}
	## return the dataframe combining hospitals and StateList
	return (data.frame(hospital = hospitals, state = StateList))	
}