rankall <- function(outcome, num = "best") {
    # Determines the hospitals with the specified rank by 30-day mortality rate
    #   for the specified outcome in every state
    #
    # Args:
    #   outcome: "heart failure", "pneumonia" or "heart attack"
    #   num: the rank of the desired hospital
    #
    # Returns:
    #   A 2-col data frame containing the hospital-state pair with the
    #       specified ranking
    if (!(outcome %in% pos_outcomes)) {
        stop("invalid outcome")
    }
    outcomes <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states <- sort(unique(outcomes$State))
    hospitals <- lapply(states, rankhospital, outcome, num)
    hospitals <- unlist(hospitals)
    data.frame(hospital = hospitals, state = states)
}