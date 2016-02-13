rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomevector <- c("heart attack" , "heart failure" , "pneumonia")
	rankNum <- c("best", "worst")
	
	## Check that state and outcome are valid
	if(!(outcome %in% outcomevector)){
		stop("invalid outcome")
	}

	if(!(state %in% data$State)){
		stop("invalid state")
	}
	
	## Return hospital name in that state with the given rank
	dataState <- subset(data, data$State == state)

	if(outcome == outcomevector[1]){
		lapply(dataState, function(x) min(x[, 11]))
	}
	else if (outcome == outcomevector[2]){
      	outMin <- min(dataState[,17], na.rm=TRUE)
        outcomeData <- subset(dataState, dataState[,17] == outMin)
      	result <- outcomeData[2]
      	return(result)
	}
	else {
      	outMin <- min(dataState[,23], na.rm=TRUE)
        outcomeData <- subset(dataState, dataState[,23] == outMin)
      	result <- outcomeData[2]
      	return(result)
	}
	
## 30-day death rate
}