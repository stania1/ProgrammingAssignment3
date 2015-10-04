rankall <- function(outcome, num = "best") {
  hospitals <- filter_hospital_data(outcome)
  hospitals <- hospitals[order(hospitals$Hospital.Name),]
  
  hospitals <- hospitals[order(hospitals[outcome_column(outcome)]),]
  
  hospitals_by_state <- split(hospitals, hospitals$State)
  hospitals_by_state_with_specified_rank <- lapply(hospitals_by_state, get_rank, num)
  hospitals_by_state_with_specified_rank <- do.call("rbind", hospitals_by_state_with_specified_rank)
  
  hospitals_by_state_with_specified_rank <- hospitals_by_state_with_specified_rank[, c(1,2)]
  names(hospitals_by_state_with_specified_rank) <- c('hospital', 'state')
  hospitals_by_state_with_specified_rank
}

filter_hospital_data <- function(outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  filtered_data <- subset(data, select = c('Hospital.Name', 'State', outcome_column(outcome)))
  filtered_data[, outcome_column(outcome)] <- as.numeric(filtered_data[, outcome_column(outcome)])
  
  if (nrow(filtered_data) == 0) { stop('invalid state') }
  filtered_data <- subset(filtered_data, subset = !is.na(filtered_data[outcome_column(outcome)]))
  filtered_data
}

get_rank <- function(ranked_hospitals, num) {
  if (num == "best") { num = 1 }
  if (num == "worst") { num = nrow(ranked_hospitals) }
  ranked_hospitals[num, ]
}