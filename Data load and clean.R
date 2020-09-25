# MA615 Boston-Buoy-Data-Analysis
# Data load and clean 
library(tidyverse)
library(lubridate)

url1 <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1987:2019)
urls <- str_c(url1, years, url2, sep = "")

#### read table
## download data
# 87-98 original: 16 variables | data1 15 variables
merge.data1 <-read_table2(urls[1],col_names = TRUE)
for (i in 2:12){
  new.data <- read_table2(urls[i],col_names = TRUE)
  merge.data1 <- rbind(merge.data1,new.data)
}

YY <- rep("19",nrow(merge.data1))
mm <- rep("00",nrow(merge.data1))
DATE <- str_c(YY,merge.data1$YY,merge.data1$MM,merge.data1$DD)
DATE <- ymd(DATE)
MONTH <- round_date(DATE, "month")
TIME <- hms::hms(min= as.numeric(mm),
                 hours = as.numeric(merge.data1$hh))
merge.data1 <- merge.data1[,-c(1:4)]
merge.data1 <- cbind(DATE,MONTH,TIME,merge.data1)
head(merge.data1)

# 99 original: 16 variables | data2 15 variables
merge.data2 <-read_table2(urls[13],col_names = TRUE)
mm <- rep("00",nrow(merge.data2))
DATE <- str_c(merge.data2$YYYY,merge.data2$MM,merge.data2$DD)
DATE <- ymd(DATE)
MONTH <- round_date(DATE, "month")
TIME <- hms::hms(min= as.numeric(mm),
                 hours = as.numeric(merge.data2$hh))
merge.data2 <- merge.data2[,-c(1:4)]
merge.data2 <- cbind(DATE,MONTH,TIME,merge.data2)
head(merge.data2)

# 00-04 original: 17 variables | data3 16 variables
merge.data3 <-read_table2(urls[14],col_names = TRUE)
for (i in 15:18){
  new.data <- read_table2(urls[i],col_names = TRUE)
  merge.data3 <- rbind(merge.data3,new.data)
}
mm <- rep("00",nrow(merge.data3))

DATE <- str_c(merge.data3$YYYY,merge.data3$MM,merge.data3$DD)
DATE <- ymd(DATE)
MONTH <- round_date(DATE, "month")
TIME <- hms::hms(min= as.numeric(mm),
                 hours = as.numeric(merge.data3$hh))

merge.data3 <- merge.data3[,-c(1:4)]
merge.data3 <- cbind(DATE,MONTH,TIME,merge.data3)


# 05-06 original: 17 variables | data4 16 variables
merge.data4 <-read_table2(urls[19],col_names = TRUE)
new.data <- read_table2(urls[20],col_names = TRUE)
merge.data4 <- rbind(merge.data4,new.data)
mm <- as.numeric(merge.data4$mm)
mean(mm)

DATE <- str_c(merge.data4$YYYY,merge.data4$MM,merge.data4$DD)
DATE <- ymd(DATE)
MONTH <- round_date(DATE, "month")
TIME <- hms::hms(min= as.numeric(mm),
                 hours = as.numeric(merge.data4$hh))

merge.data4 <- merge.data4[,-c(1:5)]
merge.data4 <- cbind(DATE,MONTH,TIME,merge.data4)


# 07-19 original: 17 variables | data5 16 variables
merge.data5 <-read_table2(urls[21],col_names = TRUE)
merge.data5 <- merge.data5[-1,]
for (i in 22:33){
  new.data <- read_table2(urls[i],col_names = TRUE)
  new.data <- new.data[-1,]
  merge.data5 <- rbind(merge.data5,new.data)
}
mm <- as.numeric(merge.data5$mm)
mean(mm)

DATE <- str_c(merge.data5$`#YY`,merge.data5$MM,merge.data5$DD)
DATE <- ymd(DATE)
MONTH <- round_date(DATE, "month")
TIME <- hms::hms(min= as.numeric(mm),
                 hours = as.numeric(merge.data5$hh))

merge.data5 <- merge.data5[,-c(1:5)]
merge.data5 <- cbind(DATE,MONTH,TIME,merge.data5)

################################################################################
data_87_99 <- rbind(merge.data1,merge.data2)
data_00_06 <- rbind(merge.data3,merge.data4)
data_07_19 <- merge.data5

colnames(data_87_99)
colnames(data_00_06)
colnames(data_07_19)

names(data_87_99)[names(data_87_99)=="BAR"]="PRES"
names(data_00_06)[names(data_00_06)=="BAR"]="PRES"


TIDE <- rep(NA,nrow(data_87_99))
data_87_99<-cbind(data_87_99,TIDE)

WDIR <- rep(NA,nrow(data_87_99))
data_87_99<-cbind(data_87_99[,1:4],WDIR,data_87_99[,5:16])
WDIR <- rep(NA,nrow(data_00_06))
data_00_06<-cbind(data_00_06[,1:4],WDIR,data_00_06[,5:16])
WD <- rep(NA,nrow(data_07_19))
data_07_19<-cbind(data_07_19[,1:3],WD,data_07_19[,4:16])


data_87_19 <- rbind(data_87_99,data_00_06,data_07_19)

str(data_87_19)

for (i in 4:17){
  data_87_19[,i] <- as.numeric(data_87_19[,i])
  
}
str(data_87_19)


summary(data_87_19,na.rm = TRUE)

data_87_19$WD[data_87_19$WD==999] <- NA
data_87_19$WDIR[data_87_19$WDIR==999] <- NA
data_87_19$WSPD[data_87_19$WSPD==99] <- NA
data_87_19$GST[data_87_19$GST==99] <- NA
data_87_19$WVHT[data_87_19$WVHT==99] <- NA
data_87_19$DPD[data_87_19$DPD==99] <- NA
data_87_19$APD[data_87_19$APD==99] <- NA
data_87_19$MWD[data_87_19$MWD==999] <- NA
data_87_19$PRES[data_87_19$PRES==9999] <- NA
data_87_19$ATMP[data_87_19$ATMP==999] <- NA
data_87_19$WTMP[data_87_19$WTMP==999] <- NA
data_87_19$DEWP[data_87_19$DEWP==999] <- NA
data_87_19$VIS[data_87_19$VIS==99] <- NA
data_87_19$TIDE[data_87_19$TIDE==99] <- NA
summary(data_87_19,na.rm = TRUE)

save(data_87_19,file='BuoyData.Rdata')

