


# Initialization ----------------------------------------------------------

#extern
library(moments)
library(nortest)
library(ggplot2)
library(MASS) # For the data set
library(Sleuth2)

#local
source("rcode\\axlib.R")
source("rcode\\stlib.R")


# Load data ---------------------------------------------------------------

db = loaddata.csv("data\\SG_S3.csv")


# Analisis descriptivo ----------------------------------------------------

names(db)
head(db)


## Para tudos os dados

summary(db) #sumario


d = dim(db);
catg = rep(1,d[1]);
tab1  = c(catg, catg*2,catg*3,catg*4)
tab2  = c(db[,1],db[,2],db[,3],db[,4]) 
tab = cbind(tab2,tab1);
tab = as.data.frame(tab);
names(tab) <- c("datos","categorias");
tab$categorias <- factor(tab$categorias,levels=1:4,labels=c("PS","O","P","PQ")) 


# Kernel density plots
# grouped by number of categorias (indicated by color)
qplot(tab$datos, data=tab, geom="density", fill=categorias, alpha=I(.5), 
      main="Energy consumption", xlab="Consumption", 
      ylab="Density") + 
      labs(fill="Algoritmos")


# Boxplots by number of categoria 
# observations (points) are overlayed and jittered , "jitter"
qplot(categorias,tab$datos, data=tab, geom=c("boxplot"), 
      fill=categorias, main="Energy consumption",
      xlab="", ylab="Consumption") +
      labs(fill="Algoritmos")


#Histogramas
consumo = tab$datos;
m = ggplot(tab, aes(x=consumo, y=..density.. , fill=categorias)) 
m + geom_histogram(alpha=0.4, binwidth = 1) +
  geom_line(stat="density", adjust=.55) +
  expand_limits(y=0) +
  facet_grid(categorias ~ .) +
  labs(fill="Algoritmos")


## PowerSave
PS = db$PS
ms = st.descrip_measure(PS);

#Histograma
ggplot(NULL, aes(x=PS)) +
  geom_histogram(binwidth=1, fill="cornsilk", colour="grey60", size=.2) +
  geom_line(stat="density", adjust=.25)
  
#Q-Q
ggplot(db, aes(sample = db$PS)) + 
  stat_qq() +
  ylab("PS")

#Box-plot
ggplot(NULL, aes(x=1, y=PS)) + 
  geom_boxplot(notch=TRUE) +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


#Test Adherencia
test_adh = st.test.adherencia(PS)












