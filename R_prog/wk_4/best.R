# determines the hospital with the lowest 30-day mortality
# by specified state and outcome

best <- function(state, outcome) {
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% outcomes$State)) {
        stop("invalid state")
    }
    else if (!(outcome %in% pos_outcomes)) {
        stop("invalid outcome")
    }
    outcome_ind <- which(outcome == pos_outcomes)
    outcome_col <- c(11, 17, 23)
    state_outcomes <- outcomes[outcomes$State %in% state, ]
    column_no <- outcome_col[[outcome_ind]]
    ind1 <- which.min(state_outcomes[,column_no])
    entry<-state_outcomes[ind1,]
    entry$Hospital.Name
}