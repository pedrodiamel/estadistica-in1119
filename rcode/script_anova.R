



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
dbs4 = loaddata.csv("data\\SG_S4.csv")
names(db)
head(db)


# struct data -------------------------------------------------------------


cor(dbs4);
pairs(db);


# Anova analyce -----------------------------------------------------------


tab = db;
tab = tab[,c(1,2,5,6)]; #O,PS,700, 1100
tab = stack(tab);

tabs4 = dbs4;
tabs4 = tabs4[,c(1,3,5,7)]; #P, PS, 702, 1890
tabs4 = stack(tabs4);

#ANOVA S3
anova(lm(values~ind, data=tab ))

#Pós teste anova S3
pairwise.t.test(tab$values, tab$ind, p.adjust="bonferroni")

#Turkey Teste S3
resultsT = aov(values~ind, data=tab)
TukeyHSD(resultsT, conf.level = 0.95)




#----------------------------------------------------

#ANOVA S4
anova(lm(values~ind, data=tabs4 ))

#Pós teste anova S4
pairwise.t.test(tabs4$values, tabs4$ind, p.adjust="bonferroni")

#Turkey Teste S4
resultsTs4 = aov(values~ind, data=tabs4)
TukeyHSD(resultsTs4, conf.level = 0.95)

