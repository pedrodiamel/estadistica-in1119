



# Initialization ----------------------------------------------------------

#extern
library(moments)
library(nortest)
library(ggplot2)
library(MASS) # For the data set
library(Sleuth2)
library(PMCMR)

#local
source("rcode\\axlib.R")
source("rcode\\stlib.R")

# Load data ---------------------------------------------------------------


db = loaddata.csv("data\\SG_S3.csv")
names(db)
head(db)


# struct data -------------------------------------------------------------



tab = db;
tab = tab[,c(1,2,5,6)]; #O,PS,700, 1100
tab = stack(tab);

anova(lm(values~ind, data=tab ))

#Pós teste anova
pairwise.t.test(tab$values, tab$ind, p.adjust="bonferroni")

#Turkey Teste
resultsT = aov(values~ind, data=tab)
TukeyHSD(resultsT, conf.level = 0.95)

