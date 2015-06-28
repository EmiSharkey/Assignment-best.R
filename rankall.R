#               rankall.R
#       Application that retrieves the information for a specified conditions
#       computing hospital's of the same rank within each state (lowest morality)
#       in order to output the hospitals in alphabetical order of states to the user.
# *     @author         Emilie Sharkey 
# *     @version	2015.01
# *	@since		2015-06-27

rankall <- function(outcome, num = "best") {
        #stores empty data.frame as variable final
        final <- data.frame()
  
        #reads the csv and inputs data into raw_data
        raw_data <- read.csv(file="C:/Users/Emilie/Desktop/Coursera/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", header=TRUE)
        
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
                outcome_filtered  <-  data.frame(raw_data$Hospital.Name, raw_data[, column_outcome], raw_data$State)
                
                #change the colnames for outcome_filtered
                colnames(outcome_filtered) <- c( "Hospital", "Rate", "State")
                
                #rate_vect stores omits the NAs and stores Rate as numeric value
                outcome_filtered$Rate <- suppressWarnings(as.numeric(as.character(outcome_filtered$Rate)))
                
                #retrieves each unique state and stores it as character within state_names
                state_name <- unique(as.character(outcome_filtered$State))
                
                #loops until each state has been processed for data
                for (x in 1:length(state_name)){ 
                        #state_data temporarily stores data for each state so it's rank can be determined
                        state_data <-  outcome_filtered[outcome_filtered$State == state_name[x], ]
                        
                                                #if num is equal to worst continue
                                                if(num== "worst"){
                                                        
                                                        #rankings ordered descendingly regarding first Rate value then Hospital name ascendingly 
                                                        rankings<- state_data[order(-state_data$Rate, state_data$Hospital), ]
                                                        #first row in rankings is stored in answer
                                                        answer <- rankings[1,]
                                          
                                                }
                                                
                                                #if num is not equal to worst continue
                                                else{
                                                        #rankings ordered ascendingly regarding first Rate value then Hospital name ascendingly 
                                                        rankings<- state_data[order(state_data$Rate, state_data$Hospital), ]
                                                        
                                                        #if num is equal to best continue
                                                        if(num=="best"){
                                                                
                                                        #first row in rankings is stored in answer
                                                                answer <- rankings[1,] 
                                                        }
                                                        
                                                        #if num is not equal to best continue
                                                        else{
                                                        #value of user inputted num is the row in rankings is stored in answer
                                                                answer <- rankings[num,] 
                                                        }
                                                        
                                                }
                        #all the answers processed are stored within the data.frame final
                        final<- rbind(final, answer)
                }
                #final_rank comprises of final$Hospital and state_name
                final_rank <- data.frame(final$Hospital, state_name)
                
                #final_rank is alphabetically sorted by state_name
                final_rank<- final_rank[order(state_name), ]
                
                #colnames for final_rank are changed to hospital and state
                colnames(final_rank) <- c("hospital", "state")
                
                #rownames for final_rank are changed to alphabetically sorted state_name 
                rownames(final_rank) <- c(sort(state_name))
                final_rank
        }
        
        #if user inputted outcome does not meets criteria contine on
        else {
                #output error message to user
                stop("invalid outcome")
                
        }
}