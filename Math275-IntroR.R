library(readxl)
data1 <- read_excel("fatty.xlsx")
#View(data1)
names(data1)
nrow(data1)
ncol(data1)
dim(data1)
data1$Ratio
summary(data1$Ratio)
barplot(data1$Ratio)
hist(data1$Ratio)

library(readxl)
heart <- read_excel("Heart_attack_stays.xlsx")
#View(heart)
hist(heart$Stay)
hist(heart$Stay,breaks = 30, xlab = "Stay (days)", ylab = "# of Female Patients", col = "light green",
     main = "Histogram of 126 Female Myocardial Infratcion Patients", cex.main = 0.80)
boxplot(heart$Stay, horizontal = TRUE, col = "red",xlab = "Stay (days)",
        main = "Histogram of 126 Female Myocardial Infratcion Patients")

