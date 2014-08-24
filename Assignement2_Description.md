---
output: pdf_document
---
# DATA ANALYSIS OF STORM DATA
## The data is analysed to determine which event affects the health of humans most(injuries and fatalities) and which events cause the greatest economic damage.

##Synopsis:
The data analysis was done in R. I have used the plyr,ggplot2,knitr and the base libraries for this analysis. The data has been cleaned only for the top 25 events in each case. This may produce some inaccuracy if the data was cleaned fully but my analysis suggests that it won't have a large amount of effect. The cleaning of the data involved grouping the same kind of events together. The injuries and fatalities caused by the events was plotted seperately.All the plots are barplots.The results have been summarised at the end of this document.

## Data Processing
### Reading the data into R.
The code chunk below reads the data into R and summmarises it using the ddply function:

```r
data<-read.csv("repdata-data-StormData.csv.bz2")
data2 <- data[,c("EVTYPE","INJURIES","FATALITIES")]
##TAKING SUM OF ALL THE INJURIES/FATALITIES CAUSED BY A PARTICULAR EVENT
library(plyr)
data2<-ddply(data2,.(EVTYPE),summarize,INJURIES=sum(INJURIES),
             FATALITIES = sum(FATALITIES))
```
### Analysis of Injury data:
The top 25 events have been cleaned and analysed.

1. FLOOD and FLASH FLOOD have been grouped together under FLOOD.

2. THUNDERSTORM WINDS,THUNDERSTORM WIND and TSTM WIND have been grouped together under THUNDERSTORM WINDS.

3. HEAT,EXCESSIVE HEAT and HEAT WAVE have been grouped togther under Heat.

4. WILDFIRE and WILD/FOREST FIRE have been grouped together under WILDFIRE.

```r
library(ggplot2)
library(plyr)
#SELECTING INJURY DATA
dataInjury <- data2[,1:2]
dataInjury <-dataInjury[order(dataInjury$INJURIES,decreasing = TRUE),]

#SELECTING TOP 25
dataInjury<-dataInjury[1:25,]

##CLEANING THE EVTYPE COLUMN AND CLUBBING THE SIMILIAR EVENTS TOGETHER

d<-dataInjury[dataInjury$EVTYPE == "FLOOD" | 
                      dataInjury$EVTYPE == "FLASH FLOOD",]
d$EVTYPE="FLOOD"
d$INJURIES=sum(d$INJURIES)
d<-d[1,]
dataInjury[dataInjury$EVTYPE == "FLOOD" |
                   dataInjury$EVTYPE == "FLASH FLOOD",]<-d[1,]
dataInjury<-dataInjury[-8,]


d<-dataInjury[dataInjury$EVTYPE == "THUNDERSTORM WIND" |
                      dataInjury$EVTYPE == "THUNDERSTORM WINDS"|
                      dataInjury$EVTYPE == "TSTM WIND",]
d$INJURIES<-sum(d$INJURIES)
d$EVTYPE <-"TSTM WIND"
d<-d[1,1:2]
dataInjury[dataInjury$EVTYPE == "THUNDERSTORM WIND" |
                   dataInjury$EVTYPE == "THUNDERSTORM WINDS"|
                   dataInjury$EVTYPE == "TSTM WIND",]<-d
dataInjury<-dataInjury[-15,]
dataInjury<-dataInjury[-8,]


d<-dataInjury[dataInjury$EVTYPE == "HEAT" | 
                      dataInjury$EVTYPE == "EXCESSIVE HEAT"
              |dataInjury$EVTYPE == "HEAT WAVE",]
d$EVTYPE<-d$EVTYPE[2]
d$INJURIES<-sum(d$INJURIES)
d<-d[1,]
dataInjury[dataInjury$EVTYPE == "HEAT" | dataInjury$EVTYPE == "EXCESSIVE HEAT"
           |dataInjury$EVTYPE == "HEAT WAVE",]<-d
dataInjury<-dataInjury[-21,]
dataInjury<-dataInjury[-6,]

d<-dataInjury[dataInjury$EVTYPE == "WILD/FOREST FIRE" | 
                      dataInjury$EVTYPE == "WILDFIRE",]
d$EVTYPE <- d$EVTYPE[1]
d$INJURIES<-sum(d$INJURIES)
dataInjury[dataInjury$EVTYPE == "WILD/FOREST FIRE" |
                   dataInjury$EVTYPE == "WILDFIRE",]<-d
dataInjury<-dataInjury[-15,]

#SORTING
dataInjury <-dataInjury[order(dataInjury$INJURIES,decreasing = TRUE),]

##ADDING A NEW LINE ESCAPE CHARACTER WHENEVER SPACE IS ENCOUNTERED IN THE NAMES
name <- dataInjury$EVTYPE
name<-sapply(name, function(x) gsub("\\s", "\\\n", x))
dataInjury$EVTYPE <- name
dataInjury[dataInjury$EVTYPE=="HURRICANE/TYPHOON",]$EVTYPE <-
        "HURRICANE\\\nTYPHOON"

#PLOT
qplot(x=EVTYPE,y=INJURIES,data = dataInjury,
      geom ="bar",stat = "identity")
```

![plot of chunk Injury data](png_fig/Injury.png) 
As can be seen from the above plot,events causing most injuries are(in order):
TORNADO, TSTM
WIND, HEAT, FLOOD, LIGHTNING, ICE
STORM, WILDFIRE, HAIL, WINTER
STORM, HURRICANE\
TYPHOON, HIGH
WIND, HEAVY
SNOW, BLIZZARD, FOG, DUST
STORM, WINTER
WEATHER, DENSE
FOG, TROPICAL
STORM, HIGH
WINDS.

### Analysis of Fatality data:
Similar analysis has been done for the fatality data.


```r
library(plyr)
library(ggplot2)

dataFatality <- data2[,c(1,3)]
dataFatality <-dataFatality[order(dataFatality$FATALITIES,decreasing = TRUE),]

##SELECTING THE TOP 25.
dataFatality<-dataFatality[1:25,]

##CLEANING THE EVTYPE COLUMN AND CLUBBING THE SIMILIAR EVENTS TOGETHER
d<-dataFatality[dataFatality$EVTYPE == "FLOOD" |
                      dataFatality$EVTYPE == "FLASH FLOOD",]
d$EVTYPE="FLOOD"
d$FATALITIES=sum(d$FATALITIES)
d<-d[1,]
dataFatality[dataFatality$EVTYPE == "FLOOD" | 
                     dataFatality$EVTYPE == "FLASH FLOOD",]<-d[1,]
dataFatality<-dataFatality[-7,]


d<-dataFatality[dataFatality$EVTYPE == "THUNDERSTORM WIND" | 
                      dataFatality$EVTYPE == "THUNDERSTORM WINDS"|
                      dataFatality$EVTYPE == "TSTM WIND",]
d$FATALITIES<-sum(d$FATALITIES)
d$EVTYPE <-"TSTM WIND"
d<-d[1,1:2]
dataFatality[dataFatality$EVTYPE == "THUNDERSTORM WIND" | 
                   dataFatality$EVTYPE == "THUNDERSTORM WINDS"|
                     dataFatality$EVTYPE == "TSTM WIND",]<-d
dataFatality<-dataFatality[-14,]



d<-dataFatality[dataFatality$EVTYPE == "HEAT" | 
                        dataFatality$EVTYPE == "EXCESSIVE HEAT"|
                        dataFatality$EVTYPE == "HEAT WAVE"|
                        dataFatality$EVTYPE == "EXTREME HEAT",]
d$EVTYPE<-d$EVTYPE[2]
d$FATALITIES<-sum(d$FATALITIES)
d<-d[1,]
dataFatality[dataFatality$EVTYPE == "HEAT" | 
                   dataFatality$EVTYPE == "EXCESSIVE HEAT"|
                     dataFatality$EVTYPE == "HEAT WAVE"|
                     dataFatality$EVTYPE == "EXTREME HEAT",]<-d
dataFatality<-dataFatality[-4,]
dataFatality<-dataFatality[-11,]
dataFatality<-dataFatality[-18,]

d<-dataFatality[dataFatality$EVTYPE == "HIGH WIND" 
              | dataFatality$EVTYPE == "STRONG WIND",]
d$EVTYPE <- d$EVTYPE[1]
d$FATALITIES<-sum(d$FATALITIES)
dataFatality[dataFatality$EVTYPE == "HIGH WIND" 
             | dataFatality$EVTYPE == "STRONG WIND",]<-d
dataFatality<-dataFatality[-14,]


d<-dataFatality[dataFatality$EVTYPE == "ICE STORM" 
                | dataFatality$EVTYPE == "WINTER STORM",]
d$EVTYPE <- d$EVTYPE[1]
d$FATALITIES<-sum(d$FATALITIES)
dataFatality[dataFatality$EVTYPE == "ICE STORM" 
             | dataFatality$EVTYPE == "WINTER STORM",]<-d
dataFatality<-dataFatality[-18,]


d<-dataFatality[dataFatality$EVTYPE == "EXTREME COLD" 
                | dataFatality$EVTYPE == "EXTREME COLD/WIND CHILL"
                | dataFatality$EVTYPE == "COLD/WIND CHILL",]
d$EVTYPE <- d$EVTYPE[1]
d$FATALITIES<-sum(d$FATALITIES)
dataFatality[dataFatality$EVTYPE == "EXTREME COLD" 
             | dataFatality$EVTYPE == "EXTREME COLD/WIND CHILL"
             | dataFatality$EVTYPE == "COLD/WIND CHILL",]<-d
dataFatality<-dataFatality[-17,]
dataFatality<-dataFatality[-13,]

dataFatality <-dataFatality[order(dataFatality$FATALITIES,decreasing = TRUE),]

##ADDING A NEW LINE ESCAPE CHARACTER WHENEVER SPACE IS ENCOUNTERED IN THE NAMES
name <- dataFatality$EVTYPE
name<-sapply(name, function(x) gsub("\\s", "\\\n", x))
dataFatality$EVTYPE <- name


qplot(x=EVTYPE,y=FATALITIES,data = dataFatality,
      geom ="bar",stat = "identity")
```

![plot of chunk Fatality data](png_fig/Fatality.png) 
As can be seen from the above plot,events causing most fatalities are(in order):
TORNADO, HEAT, FLOOD, LIGHTNING, TSTM
WIND, EXTREME
COLD, RIP
CURRENT, HIGH
WIND, WINTER
STORM, AVALANCHE, RIP
CURRENTS, HEAVY
SNOW, BLIZZARD, HIGH
SURF, HEAVY
RAIN, WILDFIRE.

### Analysis of Economic data:

The top 25 events causing economic damage have been summarised using the following code chunk. The exponents containing "K","M","m","B","b and "k" were considered. All the other exponents have been ignored. K/k was taken to be as 10^3,M/m was taken to be as 10^6 and B/b was taken to be as 10^9 according to the Standard Convections. All other exponents were ignored because of the possibility of typos which would have skewed the results. 

```r
library(plyr)
library(ggplot2)
data2<-data[,c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
data2<-data2[data2$PROPDMGEXP == "K"|data2$PROPDMGEXP == "k"|
                     data2$PROPDMGEXP == "M"|
                     data2$PROPDMGEXP == "m"|
                     data2$PROPDMGEXP == "B"|
                     data2$PROPDMGEXP == "b",]
data2$PROPDMGEXP <- gsub("K",10^3,data2$PROPDMGEXP)
data2$PROPDMGEXP <- gsub("M",10^6,data2$PROPDMGEXP)
data2$PROPDMGEXP <- gsub("m",10^6,data2$PROPDMGEXP)
data2$PROPDMGEXP <- gsub("B",10^9,data2$PROPDMGEXP)
data2$PROPDMGEXP<-as.numeric(data2$PROPDMGEXP)
data2<-mutate(data2,"Propcost"= data2$PROPDMG * data2$PROPDMGEXP)


data2<-data2[data2$CROPDMGEXP == "K"|data2$CROPDMGEXP == "k"|
                     data2$CROPDMGEXP == "M"|
                     data2$CROPDMGEXP == "m"|
                     data2$CROPDMGEXP == "B"|
                     data2$CROPDMGEXP == "b",]
data2$CROPDMGEXP <- gsub("K",10^3,data2$CROPDMGEXP)
data2$CROPDMGEXP <- gsub("M",10^6,data2$CROPDMGEXP)
data2$CROPDMGEXP <- gsub("m",10^6,data2$CROPDMGEXP)
data2$CROPDMGEXP <- gsub("B",10^9,data2$CROPDMGEXP)
data2$CROPDMGEXP<-as.numeric(data2$CROPDMGEXP)
data2<-mutate(data2,"CROPcost"= data2$CROPDMG * data2$CROPDMGEXP)


data2<-data2[complete.cases(data2),]

data2<-mutate(data2,"TotalCost"=CROPcost+Propcost)
data2<-data2[,c("EVTYPE","TotalCost")]


data2<-ddply(data2,.(EVTYPE),summarize,TotalCost=sum(TotalCost))

data2 <-data2[order(data2$TotalCost,decreasing = TRUE),]

data2 <- data2[1:25,]

d<-data2[data2$EVTYPE == "FLOOD" | 
                      data2$EVTYPE == "FLASH FLOOD"|
                 data2$EVTYPE == "RIVER FLOOD"|
                 data2$EVTYPE=="FLASH FLOOD/FLOOD",]
d$EVTYPE="FLOOD"
d$TotalCost=sum(d$TotalCost)
d<-d[1,]
data2[data2$EVTYPE == "FLOOD" | 
              data2$EVTYPE == "FLASH FLOOD"|
              data2$EVTYPE == "RIVER FLOOD"|
              data2$EVTYPE=="FLASH FLOOD/FLOOD",]<-d[1,]

data2<-data2[c(-25,-7,-5),]

d<-data2[data2$EVTYPE == "HURRICANE/TYPHOON" | 
                 data2$EVTYPE == "HURRICANE",]
d$EVTYPE="HURRICANE"
d$TotalCost=sum(d$TotalCost)
d<-d[1,]
data2[data2$EVTYPE == "HURRICANE/TYPHOON" | 
              data2$EVTYPE == "HURRICANE",]<-d[1,]
data2<-data2[-4,]

data2 <-data2[order(data2$TotalCost,decreasing = TRUE),]

name <- data2$EVTYPE
name<-sapply(name, function(x) gsub("\\s", "\\\n", x))
data2$EVTYPE <- name
data2[data2$EVTYPE=="FROST/FREEZE",]$EVTYPE <-
        "FROST\\\nFREEZE"


qplot(x=EVTYPE,y=TotalCost,data = data2[1:15,],
      geom ="bar",stat = "identity")
```

![plot of chunk Economic data](png_fig/Economic.png) 
As can be seen from the plot the events causing the most Economic Cost are(in order):
FLOOD, HURRICANE, TORNADO, HAIL, ICE
STORM, STORM
SURGE/TIDE, THUNDERSTORM
WIND, WILDFIRE, HIGH
WIND, HURRICANE
OPAL, DROUGHT, TORNADOES,
TSTM
WIND,
HAIL, TROPICAL
STORM, TSTM
WIND, WINTER
STORM

## RESULTS
1. The event causing most injuries is:TORNADO.
2. The event causing most fatalities is:TORNADO.
3. The event causing most economic cost is:FLOOD.
