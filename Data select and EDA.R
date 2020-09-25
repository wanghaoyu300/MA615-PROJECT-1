# MA615 Boston-Buoy-Data-Analysis
# Data select and EDA

library(Rmisc) #rmisc first
library(tidyverse)
library(corrplot)
library(lubridate)
library(GGally)


load("BuoyData.Rdata")
tmpdata  <-  data_87_19[ , !names(data_87_19) %in% c("WD","WDIR","MWD","VIS","TIDE","DEWP")]


# delay: mean by day with NA
planes <-  group_by(tmpdata, DATE)
delay  <-  summarise(planes, WSPD = mean(WSPD, na.rm = TRUE),
                     GST = mean(GST, na.rm = TRUE), 
                     WVHT = mean(WVHT, na.rm = TRUE), 
                     DPD = mean(DPD, na.rm = TRUE),
                     APD = mean(APD, na.rm = TRUE), 
                     PRES = mean(PRES, na.rm = TRUE),
                     ATMP = mean(ATMP, na.rm = TRUE), 
                     WTMP = mean(WTMP, na.rm = TRUE))

# delay2: mean by month with NA
planes2 <-  group_by(tmpdata, MONTH)
delay2  <-  summarise(planes2, WSPD = mean(WSPD, na.rm = TRUE),
                      GST = mean(GST, na.rm = TRUE), 
                      WVHT = mean(WVHT, na.rm = TRUE), 
                      DPD = mean(DPD, na.rm = TRUE),
                      APD = mean(APD, na.rm = TRUE), 
                      PRES = mean(PRES, na.rm = TRUE),
                      ATMP = mean(ATMP, na.rm = TRUE), 
                      WTMP = mean(WTMP, na.rm = TRUE))

# delay3: mean by day without NA
tmpdata  <-  na.omit(tmpdata)
planes3 <-  group_by(tmpdata, DATE)
delay3  <-  summarise(planes3, WSPD = mean(WSPD, na.rm = TRUE),
                      GST = mean(GST, na.rm = TRUE), 
                      WVHT = mean(WVHT, na.rm = TRUE), 
                      DPD = mean(DPD, na.rm = TRUE),
                      APD = mean(APD, na.rm = TRUE), 
                      PRES = mean(PRES, na.rm = TRUE),
                      ATMP = mean(ATMP, na.rm = TRUE), 
                      WTMP = mean(WTMP, na.rm = TRUE))

# delay4: mean by month without NA
planes4 <-  group_by(tmpdata, MONTH)
delay4  <-  summarise(planes4, WSPD = mean(WSPD, na.rm = TRUE),
                      GST = mean(GST, na.rm = TRUE), 
                      WVHT = mean(WVHT, na.rm = TRUE), 
                      DPD = mean(DPD, na.rm = TRUE),
                      APD = mean(APD, na.rm = TRUE), 
                      PRES = mean(PRES, na.rm = TRUE),
                      ATMP = mean(ATMP, na.rm = TRUE), 
                      WTMP = mean(WTMP, na.rm = TRUE))


# correlation
# plot
cor_de <- cor(delay3[,2:9])
res_cor <- cor(cor_de)
corrplot(corr=res_cor)

corrplot(corr = res_cor,order = "AOE",type="upper",method="pie",tl.pos = "d",tl.cex = 0.75,tl.col = "black")
corrplot(corr = res_cor,add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")

## matrix plot #day
## smdata <- data.frame(delay3[2:9])
## ggscatmat(smdata) + theme_bw(base_family = "STKaiti") +
## theme(plot.title = element_text(hjust = 0.5)) +
## labs(x = "",y = "")

# by month
smdata <- data.frame(delay3[2:9])
ggscatmat(smdata) + theme_bw(base_family = "STKaiti") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "",y = "")

## plot selected coefficient verses ATMP 
fit1 <- lm(data = delay3, ATMP ~ WSPD)
p1 <- ggplot(data = delay3, aes(WSPD, ATMP))+
          geom_point()+
          geom_abline(intercept = coef(fit1)[1],
                    slope = coef(fit1)[2])+
        labs(x = "WSPD", y = "ATMP")

fit2 <- lm(data = delay3, ATMP ~ GST)
p2 <- ggplot(data = delay4, aes(GST, ATMP))+
          geom_point()+
          geom_abline(intercept = coef(fit2)[1],
              slope = coef(fit2)[2])+
        labs(x = "GST", y = "ATMP")

fit3 <- lm(data = delay3, ATMP ~ WVHT)
p3 <- ggplot(data = delay4, aes(WVHT, ATMP))+
          geom_point()+
          geom_abline(intercept = coef(fit3)[1],
              slope = coef(fit3)[2])+
        labs(x = "WVHT", y = "ATMP")

fit4 <- lm(data = delay3, ATMP ~ DPD)
p4 <- ggplot(data = delay4, aes(DPD, ATMP))+
  geom_point()+
  geom_abline(intercept = coef(fit4)[1],
              slope = coef(fit4)[2])+
  labs(x = "DPD", y = "ATMP")


multiplot(p1, p2, p3, p4, cols = 2)


#plot(delay$DATE,delay$ATMP,type = "l")
ggplot(data = delay, aes(x = DATE, y = ATMP))+
  geom_line()+
  geom_smooth(method = lm, formula = y ~ x)

#plot(delay$GST,delay$ATMP)
#plot(delay2$MONTH,delay2$WTMP,type = "l")


year <- year(delay2$MONTH)
month <- month(delay2$MONTH)
aveTEM <- delay2$ATMP
Tdata <- data.frame(year, month, aveTEM)
names(Tdata) <- c("year","month","MeanTemp")

# heat map
ggplot(data=Tdata, aes(x=year,y=month)) + 
  theme_bw(base_family = "STKaiti") +
  geom_tile(aes(fill = MeanTemp),colour = "white") + 
## geom_text(aes(label = round(MeanTemp,1))) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(10,'Spectral'))) + 
  theme(legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position="top") + 
  ggtitle("Mean Temp") +
  labs(x="Year",y = "Month") +
  theme(plot.title = element_text(hjust = 0.5))


# linear regression

 fit_month<-lm(data=delay3, ATMP~MONTH)
