best<-function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  
  if(state %in% data[,7] == FALSE) {stop("invalid state")}
  if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {stop("invalid outcome")}
  
  he = data.frame(NN=c(11,17,23), OO=c("heart attack", "heart failure", "pneumonia"))
  
  y = subset(he,he$OO == outcome)[,1]
  
  dataofstate <- subset(data,data[,7] == state)
  dataofmin <- subset(dataofstate,as.numeric(dataofstate[,y]) == min(as.numeric(dataofstate[,y]),na.rm=TRUE))
  
  dataofmintie <- dataofmin[c(order(dataofmin[,2])),]   ## handling ties
  dataofmintie[1,2]
  
}