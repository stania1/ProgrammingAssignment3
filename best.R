best <- function(abbr_state, outcome) {
  hospitals_in_state <- filter_hospital_data(abbr_state, outcome)
  hospitals_in_state <- hospitals_in_state[order(hospitals_in_state$Hospital.Name),]
  lowest_mortality_rate <- min(hospitals_in_state[outcome_column(outcome)], na.rm = TRUE)
  best_hospital <- subset(hospitals_in_state, subset = hospitals_in_state[outcome_column(outcome)] == lowest_mortality_rate, select = 'Hospital.Name')
  best_hospital$Hospital.Name
}

filter_hospital_data <- function(abbr_state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  filtered_data <- subset(data, subset = data$State == abbr_state, select = c('Hospital.Name', 'State', outcome_column(outcome)))
  filtered_data[, outcome_column(outcome)] <- as.numeric(filtered_data[, outcome_column(outcome)])
  
  if (nrow(filtered_data) == 0) { stop('invalid state') }
  filtered_data
}

outcome_column <- function(outcome) {
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  outcome_columns <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
                       'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 
                       'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
  outcome_index <- match(outcome, outcomes)
  if (is.na(outcome_index)) { stop('invalid outcome') }
  outcome_columns[outcome_index]
}
