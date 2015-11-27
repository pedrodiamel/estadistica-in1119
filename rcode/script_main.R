
#includes
library(moments)
library(nortest)
library(ggplot2)

#local
source("rcode\\axlib.R")


#loading data
# DB = loadData("data\\SF_S3.csv");
# DB = loadData("data\\SF_S4.csv");
# DB = loadData("data\\SG_S3.csv");
DB = loadData("data\\SG_S4.csv");

print(names(DB)); #names colum
print(head(DB));  #print 6 first row


X = DB$P;

#Analisis de normalidad
source("rcode\\raw_scrip_analysis_norm.R")











