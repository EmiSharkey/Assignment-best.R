#               best.R
#       console application used to find the hospitals with the lowest
#       morality rate within the state and condition specified.  
# *	@author		Emilie Sharkey 
# *	@version	2015.01
# *	@since		2015-06-27

best <- function(state, outcome) {
        #reads the csv and inputs data into raw_data
        raw_data <- read.csv(file="C:/Users/Emilie/Desktop/Coursera/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", header=TRUE)
        
        #raw_data is filted by user specified state and inputted into state_filtered
        state_filtered <- raw_data[raw_data$State == state, ]
        
        #if user inputted state is not found
        if(length(state_filtered$State) == 0){
                #output error message to user                
                stop("invalid state")
        }
        #if user inputted state is found
        else if(length(state_filtered$State) != 0){
        
                #if user inputted outcome meets criteria contine on
                if(outcome== "heart attack" | outcome== "pneumonia" | outcome== "heart failure") { 
                        
                        #column_outcome determined by user inputted outcome
                        if(outcome== "heart attack" ){
                                column_outcome= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"       
                        } 
                        else  if(outcome== "pneumonia"){
                                column_outcome= "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"       
                        } 
                        else{ 
                                column_outcome= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"       
                        } 
                        #outcome_filtered stores the hospital names and mortality rate
                        outcome_filtered  <-  data.frame(state_filtered$Hospital.Name, state_filtered[, column_outcome])
                        
                        #change the colnames for outcome_filtered
                        colnames(outcome_filtered) <- c("Hospital", "Rate")
                        
                        #rate_vect stores omits the NAs and stores Rate as numeric value
                        rate_vect <- suppressWarnings(na.omit(as.numeric(as.character(outcome_filtered$Rate))))
                        
                        #lowest to highest values sorted within rank
                        rank <- sort(rate_vect)
                        
                        #filters for lowest Rate and stores within outcome_filtered
                        outcome_filtered <- outcome_filtered[outcome_filtered$Rate == rank[1], ]
                        
                        #outputs Hospital within outcome_filtered to user
                        as.character(outcome_filtered$Hospital)
                        
                }
                
                #if user inputted outcome does not meets criteria contine on
                else {
                        #output error message to user
                        stop("invalid outcome")
                }       
        }
}