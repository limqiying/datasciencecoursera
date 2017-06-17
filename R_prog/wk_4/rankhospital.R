rankhospital <- function(state, outcome, num = 'best') {
    # Determines the hospital with the specified rank by 30-day mortality rate
    # for the specified outcome and state
    #
    # Args:
    #   state: state in which the hospital is in
    #   outcome: "heart failure", "pneumonia" or "heart attack"
    #   num: the rank of the desired hospital
    #
    # Returns:
    #   The name of hospital corresponding to the specified parameters.
    if (num == 'best') {
        return (best(state, outcome))
    }
    outcomes <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
    # Error Handling
    if (!(state %in% outcomes$State)) {
        stop("invalid state")
    }
    else if (!(outcome %in% pos_outcomes)) {
        stop("invalid outcome")
    }
    # Filters out state and outcome entries
    outcome_ind <- which(outcome == pos_outcomes)
    outcome_col <- c(11, 17, 23)
    state_outcomes <-
        outcomes[outcomes$State %in% state, ]  # state subset
    column_no <-
        outcome_col[[outcome_ind]]  # column corresponding to outcome
    
    if (num == 'worst') {
        ind1 <- which.max(state_outcomes[, column_no])
        entry <- state_outcomes[ind1,]
    }
    else {
        state_outcomes[, column_no] <-
            as.numeric(state_outcomes[, column_no])
        ordered <-
            state_outcomes[order(state_outcomes[, column_no], state_outcomes$Hospital.Name),]
        entry <- ordered[num,]
    }
    entry$Hospital.Name
    
}