firedata <- read.csv("PropertyLossEdited.csv")
head(firedata)
#Season
head(firedata[3])
#Type
head(firedata[6])

libary(dplry)

anova(firedata)

#Anova test of total loss based on season
aovSeason = aov(Total_Loss ~ Season, data = firedata)
summary(aovSeason)

#Anova test of total loss based on Incident.Type
aovI = aov(Total_Loss ~ Incident.Type, data = firedata)
summary(aovI)

