#               rankhospital.R
#       console application used to find the hospitals of a user
#       specified ranked and match it to it's name within a user
#       specified state.
# *     @author        	Emilie Sharkey 
# *	@version	2015.01
# *	@since		2015-06-27

rankhospital <- function(state, outcome, num = "best") {
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
                        else{source("submitscript1.R")
                             column_outcome= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"       
                        } 
                        #outcome_filtered stores the hospital names and mortality rate
                        outcome_filtered  <-  data.frame(state_filtered$Hospital.Name, state_filtered[, column_outcome])
                        
                        #change the colnames for outcome_filtered
                        colnames(outcome_filtered) <- c("Hospital", "Rate")
                        
                        #rate_vect stores omits the NAs and stores Rate as numeric value
                        outcome_filtered$Rate <- suppressWarnings(as.numeric(as.character(outcome_filtered$Rate)))
                        
                        #if num is equal to worst continue
                        if(num== "worst"){
                                #rankings ordered descendingly regarding first Rate value then Hospital name ascendingly 
                                rankings<- outcome_filtered[order(-outcome_filtered$Rate, outcome_filtered$Hospital), ]
                                #find first item in ranking and store within answer
                                answer <- rankings[1,]
                        }
                        
                        #if num is not equal to worst continue
                        else{
                                #rankings ordered ascendingly regarding first Rate value then Hospital name ascendingly 
                                rankings<- outcome_filtered[order(outcome_filtered$Rate, outcome_filtered$Hospital), ]
                                
                                #if num is equal to best continue
                                if(num=="best"){
                                        #find first item in ranking and store within answer
                                        answer <- rankings[1,] 
                                }
                                
                                #if num is not equal to best continue
                                else{
                                        #find num valued item in ranking and store within answer
                                        answer <- rankings[num ,]
                                }
                                
                        }
                        #Outputs Hospital name to user as character
                        as.character(answer$Hospital)
                }
                
                #if user inputted outcome does not meets criteria contine on
                else {
                        #output error message to user
                        stop("invalid outcome")
                }       
        }
}