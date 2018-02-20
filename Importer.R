source('Installer.R')

data <- read.csv(file = 'C:/Users/Administrator/Desktop/Data/USDNOK.FXCM.asc', header = TRUE, sep = ',')

diff <- data.frame(data$Date, data$Time, data$Close)
colnames(diff) <- c('Date', 'Time', 'Close')
diff$Date <- strptime(paste(diff$Date, diff$Time), "%d/%m/%Y %H:%M")
diff <- diff[,-2]

df <- diff
df$Delt <- apply(df,2,diff)
df$Weekday <- as.POSIXlt(df$Date)$wday

df %>%
  mutate(time  = as.POSIXct(strptime(Date, '%H:%M:%S'))) %>% 
  filter(Weekday == 1) %>% 
  group_by(time) %>% 
  summarise(meanValue = mean(Delt))


ts <- as.xts(x = diff$Close, order.by = diff$Date)
colnames(ts)<- c('Close')
ts$Delt <- diff(ts$Close, lag = 1)
ts[1,2] <- 0
head(ts)
ts$Weekday <- as.POSIXlt(index(ts$Close))$wday
head(ts)
Mon <- ts[.indexwday(ts) %in%1]
plot.xts(Mon)

Mon %>%
  mutate(min_of_day = format(index(Mon),"%H:%M")) %>% 
  group_by(min_of_day) %>% 
  summarise(meanValue = mean(Delt))

Delt <- Mon %>%
  select(Delt, Close) %>%
  group_by(format(index(Mon), '%H:%M')) %>%
  summarise(Delt = mean(Delt))
