#change wd
rm(list=ls())
setwd("\\Users\\canno\\OneDrive\\Desktop\\R Script")
#install packages 
library(ggplot2)
library(dplyr)
library(car)

#read csv
LTL <- read.csv('Data-source.csv',header=TRUE, sep = ",")
LTL <- na.omit(LTL)
head(LTL)
summary(LTL)
LTL['Distance'] <- as.numeric(LTL$Distance)
summary(LTL)

#varibles
plot(LTL$Distance,LTL$BILLED)
plot(LTL$WEIGHT,LTL$BILLED)
cor(LTL$WEIGHT,LTL$BILLED,method=c("pearson")) #.682
cor(LTL$Distance,LTL$BILLED,method=c("pearson")) #.249

#distance lm
distance.fit <- lm(BILLED ~ Distance, data = LTL)
summary(distance.fit)
anova(distance.fit)
#basic data visualize 
plot(BILLED ~ Distance , data = LTL)
abline(distance.fit)


#ggplot lm 
billed_distance <- ggplot(LTL,aes(x=Distance,y=BILLED))+
  geom_point(size=3,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="solid",lwd=2,formula="y~x")
billed_distance
summary(billed_distance)

#LTL weight and distance lm
best.fit <- lm(BILLED ~ Distance + WEIGHT , data = LTL)
summary(best.fit)
anova(best.fit)

# Zone 1
LTLO <- subset(LTL, FedEx.Zone<'4')
LTL.zone1 <- lm(BILLED ~ Distance, data = LTLO)
summary(LTL.zone1)
plot(BILLED ~ Distance , data = LTL)
abline(LTL.zone1)
zone1.best <- lm(BILLED ~ Distance + WEIGHT , data = LTLO)
summary(zone1.best)
avPlots(zone1.best)

#Zone 2
LTLO <- subset(LTL, FedEx.Zone>'3')
LTL.zone2 <- lm(BILLED ~ Distance, data = LTLO)
summary(LTL.zone2)
plot(BILLED ~ Distance , data = LTL)
abline(LTL.zone2)
zone2.best <- lm(BILLED ~ Distance + WEIGHT , data = LTLO)
summary(zone2.best)
avPlots(zone2.best)
