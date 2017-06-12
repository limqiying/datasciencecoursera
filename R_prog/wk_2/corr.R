#Author: Qi Ying Lim
#Coursera Data Science R-Programming Week2(3)

corr <- function(directory, threshold = 0) {
    comp_data<- complete(directory)
    applicable_d <- subset(comp_data, nobs >= threshold)[[1]]
    if (length(applicable_d) <= 0) {
        return(vector(mode = "numeric", length = 0))
    }
    else {
        old.dir <- getwd()
        setwd(directory)
        file_list <- dir()
        correlations <- c()
        for (file in file_list[applicable_d]) {
            dat <- read.csv(file)
            file_cor <- cor(dat[2],dat[3], use = "na.or.complete")
            if (!is.na(file_cor)) {
                correlations <- c(correlations, file_cor)
            }
        }
        setwd(old.dir)
        correlations
    }
}