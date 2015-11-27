


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



# Analisis descriptivo ----------------------------------------------------

summary(db) #sumario
ms_ps = st.descrip_measure(db$PS);
ms_o = st.descrip_measure(db$O);
ms_p = st.descrip_measure(db$P);
ms_pq = st.descrip_measure(db$PQ);
ms_fx7 = st.descrip_measure(db$X700MHz);
ms_fx11 = st.descrip_measure(db$X1100MHz);
ms_fx14 = st.descrip_measure(db$X1400MHz);

#print
st.descrip_measure.toSting(db$PS)
st.descrip_measure.toSting(db$O)
st.descrip_measure.toSting(db$P)
st.descrip_measure.toSting(db$PQ)
st.descrip_measure.toSting(db$X700MHz)
st.descrip_measure.toSting(db$X1100MHz)
st.descrip_measure.toSting(db$X1400MHz)


d = dim(db);
catg = rep(1,d[1]);
tab1  = c(catg, catg*2,catg*3,catg*4,catg*5,catg*6,catg*7)
tab2  = c(db[,1],db[,2],db[,3],db[,4],db[,5],db[,6],db[,7]) 
tab = cbind(tab2,tab1);
tab = as.data.frame(tab);
names(tab) <- c("datos","categorias");
tab$categorias <- factor(tab$categorias,levels=1:7,
        labels=c("PS","O","P","PQ","700MHz","1100MHz","1400MHz")) 


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
      xlab="", ylab="Consumption in Joules") +
      labs(fill="Algoritmos")

#Box-plot
consumo = tab$datos;
p <- ggplot(tab, aes(categorias, consumo))
p + geom_boxplot( aes(fill=categorias),notch = TRUE) +
  coord_flip() +
  labs(fill="Algoritmos")



#Histogramas
consumo = tab$datos;
m = ggplot(tab, aes(x=consumo, y=..density.. , fill=categorias)) 
m + geom_histogram(alpha=0.8, binwidth = 1) +
  geom_line(stat="density", adjust=.55, alpha=0.4) +
  expand_limits(y=0) +
  facet_grid(categorias ~ .) +
  labs(fill="Algoritmos")


## Analisis individual


# Power Save --------------------------------------------------------------

name_alg = "PS"
PS = db$PS

#Histograma (500x400)
ggplot(db, aes(x=PS)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 1,
    colour="black", 
    fill="white" 
    ) +
  stat_function(fun=dnorm, 
    args=list(mean=mean(db$PS), sd=sd(db$PS)), 
    color ="red") +
    xlab(name_alg)
  #geom_line(stat="density", adjust=.55)


#Q-Q
ggplot(db, aes(sample = db$PS)) + 
  stat_qq() +
  ylab(name_alg)

#Box-plot
ggplot(NULL, aes(x=1, y=PS)) + 
  geom_boxplot(notch=TRUE) +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


#Test Adherencia
test_adh = st.test.adherencia(db$PS)
test_adh$shapiro
test_adh$lillie
test_adh$anderson


 

# ANALISIS DE OnDemand ----------------------------------------------------

name_alg = "O"
O = db$O


#Histograma (500x400)
ggplot(db, aes(x=O)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 2,
    colour="black", 
    fill="white" 
  ) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(db$O), sd=sd(db$O)), 
                color ="red") +
  xlab(name_alg)
#geom_line(stat="density", adjust=.55)

#png
# png(file="ohistnorm.png",height=500,width=(500))
# dev.off()
ggsave("graph/ohistnorm.png", width=4, height=4, dpi=300)

#Q-Q
ggplot(db, aes(sample = db$O)) + 
  stat_qq() +
  ylab(name_alg)

ggsave("graph/oqqnorm.png", width=4, height=4, dpi=300)


#Box-plot
ggplot(NULL, aes(x=1, y=O)) + 
  geom_boxplot(notch=TRUE) +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


#Test Adherencia
test_adh = st.test.adherencia(db$O)
test_adh$shapiro
test_adh$lillie
test_adh$anderson




# ANALISIS DE Performance  ------------------------------------------------

name_alg = "P"
P = db$P

#Histograma (500x400)
ggplot(db, aes(x=P)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 2,
    colour="black", 
    fill="white" 
  ) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(db$P), sd=sd(db$P)), 
                color ="red") +
  xlab(name_alg)
#geom_line(stat="density", adjust=.55)

ggsave("graph/phistnorm.png", width=4, height=4, dpi=300)



#Q-Q
ggplot(db, aes(sample = db$P)) + 
  stat_qq() +
  ylab(name_alg)

ggsave("graph/pqqnorm.png", width=4, height=4, dpi=300)


#Box-plot
ggplot(NULL, aes(x=1, y=P)) + 
  geom_boxplot(notch=TRUE) +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


#Test Adherencia
test_adh = st.test.adherencia(db$P)
test_adh$shapiro
test_adh$lillie
test_adh$anderson




# ANALISIS DE PegasusQ ----------------------------------------------------

name_alg = "PQ"
PQ = db$PQ

#Histograma (500x400)
ggplot(db, aes(x=PQ)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 1,
    colour="black", 
    fill="white" 
  ) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(db$PQ), sd=sd(db$PQ)), 
                color ="red") +
  xlab(name_alg)
#geom_line(stat="density", adjust=.55)

ggsave("graph/pqhistnorm.png", width=4, height=4, dpi=300)


#Q-Q
ggplot(db, aes(sample = db$PQ)) + 
  stat_qq() +
  ylab(name_alg)

ggsave("graph/pqqqnorm.png", width=4, height=4, dpi=300)

#Box-plot
ggplot(NULL, aes(x=1, y=PQ)) + 
  geom_boxplot(notch=TRUE) +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


#Test Adherencia
test_adh = st.test.adherencia(db$PQ)
test_adh$shapiro
test_adh$lillie
test_adh$anderson



# X700MHz -----------------------------------------------------------------

name_alg = "X700MHz"
X700MHz = db$X700MHz

#Histograma (500x400)
ggplot(db, aes(x=X700MHz)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 1,
    colour="black", 
    fill="white" 
  ) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(db$X700MHz), sd=sd(db$X700MHz)), 
                color ="red") +
  xlab(name_alg)
#geom_line(stat="density", adjust=.55)

ggsave("graph/x700histnorm.png", width=4, height=4, dpi=300)



#Q-Q
ggplot(db, aes(sample = db$X700MHz)) + 
  stat_qq() +
  ylab(name_alg)

ggsave("graph/x700qqnorm.png", width=4, height=4, dpi=300)

#Box-plot
ggplot(NULL, aes(x=1, y=X700MHz)) + 
  geom_boxplot(notch=TRUE) +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


#Test Adherencia
test_adh = st.test.adherencia(db$X700MHz)
test_adh$shapiro
test_adh$lillie
test_adh$anderson


# X1100MHz ----------------------------------------------------------------

name_alg = "X1100MHz"
X1100MHz = db$X1100MHz

#Histograma (500x400)
ggplot(db, aes(x=X1100MHz)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 1,
    colour="black", 
    fill="white" 
  ) +
  stat_function(fun=dnorm, 
                args=list(mean=mean(db$X1100MHz), sd=sd(db$X1100MHz)), 
                color ="red") +
  xlab(name_alg)
#geom_line(stat="density", adjust=.55)

ggsave("graph/X1100MHzhistnorm.png", width=4, height=4, dpi=300)



#Q-Q
ggplot(db, aes(sample = db$X1100MHz)) + 
  stat_qq() +
  ylab(name_alg)

ggsave("graph/X1100MHzqqnorm.png", width=4, height=4, dpi=300)

#Box-plot
ggplot(NULL, aes(x=1, y=X1100MHz)) + 
  geom_boxplot(notch=TRUE) +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


#Test Adherencia
test_adh = st.test.adherencia(db$X1100MHz)
test_adh$shapiro
test_adh$lillie
test_adh$anderson


# X1400MHz ----------------------------------------------------------------




















# Analisis de as asmostras ------------------------------------------------


#Test de Friedman con post test de nemenyi
X = as.matrix(db);
test_friedman = st.test.friedman(X,0.05)
test_friedman$tfriedman
test_friedman$ptnemenyi






