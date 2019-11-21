library(dplyr)
#library(stringr)
#library(readr)
#library(lubridate)

#read in the csv file
firedata <- read.csv("PropertyLoss.csv")

#parse out the incident numbers
firedata$Incident.Num <- strtoi(substr(firedata$Inci_type,0,3))

#collum with the incident type
firedata <- transform(firedata,Incident.Type = ifelse(Incident.Num < "124", "Structure Fire", 
                   ifelse(Incident.Num < "139", "Vehicle Fire",
                   ifelse(Incident.Num < "165", "Outside Fire", "Not a Fire"))))

#creat the month collum
firedata$Month <-  months(as.Date(firedata$Alm_Date, "%m/%d/%Y"))

#Turn month into the season
firedata <- transform(firedata, Season= ifelse(firedata$Month == "January", "Winter",
                  ifelse(firedata$Month == "February", "Winter", 
                  ifelse(firedata$Month == "March", "Spring",
                  ifelse(firedata$Month == "April", "Spring",
                  ifelse(firedata$Month == "May", "Spring",
                  ifelse(firedata$Month == "June", "Summer",
                  ifelse(firedata$Month == "July", "Summer",
                  ifelse(firedata$Month == "August", "Summer",
                  ifelse(firedata$Month == "September", "Fall",
                  ifelse(firedata$Month == "October", "Fall",
                  ifelse(firedata$Month == "November", "Fall",
                  ifelse(firedata$Month == "December", "Winter","NULL")))))))))))))

#Create the percent loss
firedata$PercentLoss <- as.numeric(firedata$Pre_Inci_Value) / as.numeric(firedata$Total_Loss)

#filter out non fires
firedata <- filter(firedata, Incident.Type != "Not a Fire")
      
head(firedata)

#old data
#firedata <- read.csv("PropertyLossEdited.csv")

#Anova test of total loss based on season
aovSeason = aov(PercentLoss ~ Season, data = firedata)
summary(aovSeason)

#Anova test of total loss based on Incident.Type
aovI = aov(PercentLoss ~ Incident.Type, data = firedata)
summary(aovI)

#Percent loss summary of each season
firedata %>%
  group_by(Season) %>%
  summarize(Md = median(PercentLoss), Mean = mean(PercentLoss), SD = sd(PercentLoss), N = n())

#Winter Subset
winter <- firedata %>%
  filter(Season == "Winter") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss,PercentLoss)

#Spring Subset
spring <- firedata %>%
  filter(Season == "Spring") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss,PercentLoss)

#Summer Subset
summer <- firedata %>%
  filter(Season == "Summer") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss,PercentLoss)

#Fall Subset
fall <- firedata %>%
  filter(Season == "Fall") %>%
  group_by(Season) %>%
  select(Season, Incident.Type,Total_Loss,PercentLoss)

#Box plot of all the subsets
boxplot(winter$PercentLoss,spring$PercentLoss,summer$PercentLoss,fall$PercentLoss,
        names = c("Winter","Spring","Summer","Fall"), xlab = "Season", ylab = "Total Loss")

#T test, winter and spring 
t.test(winter$PercentLoss,spring$PercentLoss)

#T test, winter and summer 
t.test(winter$PercentLoss,summer$PercentLoss)

#T test, winter and fall 
t.test(winter$PercentLoss,fall$PercentLoss)

#T test, spring and summer 
t.test(spring$PercentLoss,summer$PercentLoss)

#T test, spring and fall 
t.test(spring$PercentLoss,fall$PercentLoss)

#T test, summer and fall 
t.test(summer$PercentLoss,fall$PercentLoss)


#Percent loss summary of each season based on incident type
firedata %>%
  group_by(Season, Incident.Type) %>%
  summarize(Md = median(PercentLoss), Mean = mean(PercentLoss), SD = sd(PercentLoss), N = n())
