#Author: Qi Ying Lim
#Coursera Data Science R-Programming Week2(1)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    old.dir <- getwd()
    setwd(directory)
    file_list <- dir()
    moniter_mean <- c()
    for (data_file in file_list[id]) {
        data <- read.csv(data_file)
        moniter_mean <- c(moniter_mean,data[[pollutant]])
    }
    setwd(old.dir)
    mean(moniter_mean, na.rm = TRUE)
}