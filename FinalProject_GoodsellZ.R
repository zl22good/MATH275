library(dplyr)
library(stringr)
library(readr)
library(lubridate)


firedata <- read.csv("PropertyLoss.csv")
#head(firedata)
firedata$Incident.Num <- strtoi(substr(firedata$Inci_type,0,3))
#head(firedata)
firedata$Incident.Type <- transform(firedata, c= ifelse(Incident.Num < "124", "Structure Fire", ifelse(Incident.Num < "139", "Vehicle Fire",ifelse(Incident.Num < "165", "Outside Fire", "Not a Fire"))))
#head(firedata)
firedata$Month<- toString(firedata$Alm_Date)
head(firedata)


for (i in length(firedata$Month)) {
  firedata$Month[i] = "a"
}


firedata <- read.csv("PropertyLossEdited.csv")
head(firedata)
#Season
head(firedata[3])
#Type
head(firedata[6])

library(dplyr)



#Anova test of total loss based on season
aovSeason = aov(Total_Loss ~ Season, data = firedata)
summary(aovSeason)

#Anova test of total loss based on Incident.Type
aovI = aov(Total_Loss ~ Incident.Type, data = firedata)
summary(aovI)

#Total loss summary of each season
firedata %>%
  group_by(Season) %>%
  summarize(Md = median(Total_Loss), Mean = mean(Total_Loss), SD = sd(Total_Loss), N = n())

#Winter Subset
winter <- firedata %>%
  filter(Season == "Winter") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss)

#Spring Subset
spring <- firedata %>%
  filter(Season == "Spring") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss)

#Summer Subset
summer <- firedata %>%
  filter(Season == "Summer") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss)

#Fall Subset
fall <- firedata %>%
  filter(Season == "Autumn") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss)

#Box plot of all the subsets
boxplot(winter$Total_Loss,spring$Total_Loss,summer$Total_Loss,fall$Total_Loss,
        names = c("Winter","Spring","Summer","Fall"), xlab = "Season", ylab = "Total Loss")

#T test, winter and spring 
t.test(winter$Total_Loss,spring$Total_Loss)

#T test, winter and summer 
t.test(winter$Total_Loss,summer$Total_Loss)

#T test, winter and fall 
t.test(winter$Total_Loss,fall$Total_Loss)


#Total loss summary of each season based on incident type
firedata %>%
  group_by(Season, Incident.Type) %>%
  summarize(Md = median(Total_Loss), Mean = mean(Total_Loss), SD = sd(Total_Loss), N = n())
