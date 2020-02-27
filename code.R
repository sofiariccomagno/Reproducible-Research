#Load libraries and dataset
library(ggplot2)
library(dplyr)
library(viridis)

data<-read.csv("repdata-data-StormData.csv")

#Find out the Event Types with the highest number of Fatalities
fatalities<-data %>% select(EVTYPE, FATALITIES) %>% group_by(EVTYPE) %>% summarise(total.fatalities = sum(FATALITIES)) %>% arrange(-total.fatalities)
head(fatalities, 10)

g<-ggplot(fatalities[1:10,], aes(x=reorder(EVTYPE, -total.fatalities), y=total.fatalities))+
  geom_bar(stat="identity",fill=plasma(10))+
  theme(plot.title=element_text(face="bold",hjust=0.5),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
  ggtitle("Top 10 Events with Highest Total Fatalities")+
  labs(x="Event Type", y="Total Fatalities")
g

#Find out the Event Types with the highest number of Injuries
injuries<-data %>% select(EVTYPE, INJURIES) %>% group_by(EVTYPE) %>% summarise(total.injuries = sum(INJURIES)) %>% arrange(-total.injuries)
head(injuries, 10)

h<-ggplot(injuries[1:10,], aes(x=reorder(EVTYPE, -total.injuries), y=total.injuries))+
  geom_bar(stat="identity",fill=magma(10))+
  theme(plot.title=element_text(face="bold",hjust=0.5),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
  ggtitle("Top 10 Events with Highest Total Injuries")+
  labs(x="Event Type", y="Total Injuries")
h

#Find out the Event Types with the biggest economic damages
damage<-data %>% select(EVTYPE, PROPDMG,PROPDMGEXP,CROPDMG)
Symbol<-sort(unique(as.character(data$PROPDMGEXP)))
Multiplier<-c(0,0,1,10,10,10,10,10,10,10,10,10,10^9,10^2,10^2,10^3,10^6,10^6)
convert.Multiplier<-data.frame(Symbol, Multiplier)
damage$Prop.Multiplier<-convert.Multiplier$Multiplier[match(damage$PROPDMGEXP, convert.Multiplier$Symbol)]
damage<-damage %>% mutate(PROPDMG = PROPDMG*Prop.Multiplier)%>% mutate(TOTAL.DMG = PROPDMG+CROPDMG)
damage.total<-damage %>% group_by(EVTYPE) %>% summarize(TOTAL.DMG.EVTYPE = sum(TOTAL.DMG))%>% arrange(-TOTAL.DMG.EVTYPE) 
head(damage.total,10)

j<-ggplot(damage.total[1:10,], aes(x=reorder(EVTYPE, -TOTAL.DMG.EVTYPE), y=TOTAL.DMG.EVTYPE))+
  geom_bar(stat="identity",fill=viridis(10))+
  theme(plot.title=element_text(face="bold",hjust=0.5),axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
  ggtitle("Top 10 Events with Largest Economic Damage")+
  labs(x="Event Type", y="Total Cost")

j
