library(lubridate)

trades <- read.csv(file = "C:/Users/David Jaggi/Desktop/BATest.txt", header = TRUE, 
                   sep = "\t")
colnames(trades) <- c("Index", "Entry_Datetime", "Exit_Datetime",
                                 "Entry_Price","Exit_Price","Profit")
trades$Profit <- as.numeric(sub("'", "", trades$Profit, fixed = TRUE))
trades$Entry_Datetime <- ymd_hms(trades$Entry_Datetime)
trades$Exit_Datetime <- ymd_hms(trades$Exit_Datetime)


