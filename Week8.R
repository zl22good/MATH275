progeria = read.csv("progeria.csv")
xbar = mean(progeria$PWV)
mu0 = 6.6
s = sd(progeria$PWV)
n = nrow(progeria)
se = s/sqrt(n)
tstat = (xbar-mu0)/se
pvalue=pt(tstat,df=n-1,lower.tail=FALSE)
pvalue

t.test(progeria$PWV,mu=6.6, alternative="greater")

adolescents = read.csv("adolescents.csv")
t.test(adolescents$Testosterone)
t.test(adolescents$Testosterone, conf.level = 0.99)

##############################################

library(readxl)
brainsize <- read_excel("brainsize.xlsx")
View(brainsize)

head(brainsize)
summary(brainsize)
boxplot(brainsize, horizontal = TRUE
        ,medcol = "sienna", 
        col = c("light blue","tomato"))
t.test(brainsize$Autistic,brainsize$Control)
#Our null hypothesis that mu of the Auticis boys
#is equal the mu of the healthey boys

#t.test(brainsize$Control,brainsize$Autistic)


#####################
library(devtools)
install_github("kassambara/easyGgplot2")
library(easyGgplot2)

brainsize2 <- read_excel("brainsize2.xlsx")
head(brainsize2)
tail(brainsize2)

aut=subset(brainsize2,group=='Autistic')
cont=subset(brainsize2,group=='Control')
t.test(aut$brainvolume,cont$brainvolume)
#1
summary(aut)
summary(cont)

#2
boxplot(brainsize2$brainvolume~brainsize2$group, horizontal = TRUE
        ,medcol = "sienna", 
        col = c("light blue","tomato"))


ggplot2.histogram(data=brainsize2, xName='brainvolume', groupName='group', legendPosition="right",
                  alpha=0.5, addDensity=TRUE, 
                  addMeanLine=TRUE, meanLineSize=1.5, bins = 10, xtitle = "Brain Volume",
                  ytitle = "Frequency (%)",xtitleFont=c(13,"bold", "#993333"), 
                  ytitleFont=c(13,"bold", "#993333"))

#3
t.test(brainsize2$brainvolume~brainsize2$group)


#####################################

NeuralActivity <- read.csv("NeuralActivity.csv")
head(NeuralActivity)

attach(NeuralActivity)

hist(abs(NeuralActivity$SA-NeuralActivity$R))

wilcox.test(R-SA , alternative = "greater" , exact = FALSE)
wilcox.test(R-SA, a = "g", e = F)
wilcox.test(R-SA, a = "g", paired = TRUE)

