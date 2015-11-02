rankhospital <- function(state, outcome, num = "best") {

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

	## convert column 3, 4, 5 from character to numeric
	## In the original csv file, these columns are characters
	Data[,3] <- as.numeric(Data[,3])
	Data[,4] <- as.numeric(Data[,4])
	Data[,5] <- as.numeric(Data[,5])
	
	## Generate unique list from column G "State"
	## !duplicated() removes repetitive items in column 2
	StateList <- subset(Data[,2], !duplicated(Data[,2]))
	
	## Check that state and outcome are valid
	## If input state does not match with any item in StateList
	## stop with error
	## If input outcome matches with heart attack, heart failure and pneumonia
	## assign respective column numbers to variable outcome
	if (state %in% StateList) {
		if (outcome == "heart attack") {
			outcome <- 3
		} else if (outcome == "heart failure") {
			outcome <- 4 
		} else if (outcome == "pneumonia") {
			outcome <- 5
		} else {
			## cat("Error in best(\"", state, "\", \"", outcome,  "\") : invalid outcome", sep="")
			stop("invalid outcome")
			}
	}
	else {
		## cat("Error in best(\"", state, "\", \"", outcome,  "\") : invalid state", sep="")
		stop("invalid state")
	}
	
	## Rank based on outcome column and state column, If NA, put rank to last
	## Return hospital name in that state with the given rank
	StateData <- subset(Data, State == state)
	StateData <- StateData[order(StateData[,outcome], StateData[,1], na.last = TRUE),]
	
	## Based on input num, assign the sequence number
	## If input num is greater than the number of hospitals in that state, return NA
	## If input num is "worst", return the hospital with last in rank
	if (num == "best") {
		SeqNo <- 1
	} else if (num == "worst") {
		SeqNo <- max(which(!is.na(StateData[,outcome])))
	} else if (num >= 1 && num <= max(which(!is.na(StateData[,outcome])))) {
		SeqNo <- num
	} else {
		return(NA)
		}
	
	return (StateData[SeqNo,1])

}