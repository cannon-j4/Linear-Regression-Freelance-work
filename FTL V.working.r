#change wd
rm(list=ls())
setwd("\\Users\\canno\\OneDrive\\Desktop\\R Script")
#install packages 
library(ggplot2)
library(dplyr)
library(car)

#read csv
FTL <- read.csv('FTL Ecore Shipping Data.csv',header=TRUE, sep = ",")
FTL <- na.omit(FTL)
head(FTL)
summary(FTL)
FTL['Distance'] <- as.numeric(FTL$Distance)
summary(FTL)

#varibles
plot(FTL$Distance,FTL$BILLED)
plot(FTL$WEIGHT,FTL$BILLED)
cor(FTL$WEIGHT,FTL$BILLED,method=c("pearson")) #.0889
cor(FTL$Distance,FTL$BILLED,method=c("pearson")) #.072

#distance lm
distance.fit <- lm(BILLED ~ Distance, data = FTL)
summary(distance.fit)
anova(distance.fit)
#basic data visualize 
plot(BILLED ~ Distance , data = FTL)
abline(distance.fit)


#ggplot lm 
billed_distance <- ggplot(FTL,aes(x=Distance,y=BILLED))+
  geom_point(size=3,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="solid",lwd=2,formula="y~x")
billed_distance
summary(billed_distance)

#FTL weight and distance lm
best.fit <- lm(BILLED ~ Distance + WEIGHT , data = FTL)
summary(best.fit2)
anova(best.fit)

# Zone 1
FTLO <- subset(FTL, FedEx.Zone<'4')
FTL.zone1 <- lm(BILLED ~ Distance, data = FTLO)
summary(FTL.zone1)
plot(BILLED ~ Distance , data = FTL)
abline(FTL.zone1)
zone1.best <- lm(BILLED ~ Distance + WEIGHT , data = FTLO)
summary(zone1.best)
avPlots(zone1.best)
#Zone 2
FTLO <- subset(FTL, FedEx.Zone>'3')
FTL.zone2 <- lm(BILLED ~ Distance, data = FTLO)
summary(FTL.zone2)
plot(BILLED ~ Distance , data = FTL)
abline(FTL.zone2)
zone2.best <- lm(BILLED ~ Distance + WEIGHT , data = FTLO)
summary(zone2.best)
avPlots(zone2.best)



