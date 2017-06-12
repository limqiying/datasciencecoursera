#Author: Qi Ying Lim
#Coursera Data Science R-Programming Week2(2)

complete <- function(directory, id = 1:332) {
    old.dir <- getwd()
    setwd(directory)
    file_list <- dir()
    comc <- data.frame(id=integer(0), nobs=integer(0))
    id_counter <-1
    for (data_file in file_list[id]) {
        data <- read.csv(data_file)
        cc <- sum(as.numeric(complete.cases(data)))
        comc<- rbind(comc,data.frame(id[[id_counter]],cc))
        id_counter <- id_counter +1
    }
    names(comc) <- c("id","nobs")
    setwd(old.dir)
    comc
}