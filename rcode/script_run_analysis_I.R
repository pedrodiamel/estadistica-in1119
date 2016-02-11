

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
        labels=c("PS","OD","PE","PQ","700MHz","1100MHz","1400MHz")) 




# Kernel density plots
# grouped by number of categorias (indicated by color)
qplot(tab$datos, data=tab, geom="density", fill=categorias, alpha=I(.5), 
      main="", xlab="Energy Consumption (Joules)", 
      ylab="Density") + 
      labs(fill="Algorithms")


# Boxplots by number of categoria 
# observations (points) are overlayed and jittered , "jitter"
qplot(categorias,tab$datos, data=tab, geom=c("boxplot"), 
      fill=categorias, main="",
      xlab="", ylab="Energy Consumption (Joules)") +
      labs(fill="Algorithms")

#Box-plot
consumo = tab$datos;
p = ggplot(tab, aes(categorias, consumo))
p + geom_boxplot( aes(fill=categorias),notch = TRUE) +
  coord_flip() + labs(fill="Algorithms", x = "", y = "Energy Consumption (Joules)")


#Histogramas
consumo = tab$datos;
m = ggplot(tab, aes(x=consumo, y=..density.. , fill=categorias)) 
m + geom_histogram(alpha=0.8, binwidth = 1) +
  geom_line(stat="density", adjust=.55, alpha=0.4) +
  expand_limits(y=0) +
  facet_grid(categorias ~ .) +
  labs(fill="Algorithms", x = "Energy Consumption (Joules)", y = "Density")


## Analisis individual


# Power Save --------------------------------------------------------------

X = db$PS
xlab = "PS"; ylab = "Y";
st.graph.hist(db, X,xlab,ylab)
# ggsave("graph/histnorm.png", width=4, height=4, dpi=300)

st.graph.qqnorm(db, X, xlab, ylab)
# ggsave("graph/qqnorm.png", width=4, height=4, dpi=300)

#Test Adherencia
test_adh = st.test.adherencia(X)
test_adh$shapiro
test_adh$lillie
test_adh$anderson


 

# ANALISIS DE OnDemand ----------------------------------------------------

X = db$O
xlab = "O"; ylab = "Y";
st.graph.hist(db, X,xlab,ylab)
# ggsave("graph/histnorm.png", width=4, height=4, dpi=300)

st.graph.qqnorm(db, X, xlab, ylab)
# ggsave("graph/qqnorm.png", width=4, height=4, dpi=300)

#Test Adherencia
test_adh = st.test.adherencia(X)
test_adh$shapiro
test_adh$lillie
test_adh$anderson




# ANALISIS DE Performance  ------------------------------------------------

X = db$P
xlab = "P"; ylab = "Y";
st.graph.hist(db, X,xlab,ylab)
# ggsave("graph/histnorm.png", width=4, height=4, dpi=300)

st.graph.qqnorm(db, X, xlab, ylab)
# ggsave("graph/qqnorm.png", width=4, height=4, dpi=300)

#Test Adherencia
test_adh = st.test.adherencia(X)
test_adh$shapiro
test_adh$lillie
test_adh$anderson


# ANALISIS DE PegasusQ ----------------------------------------------------

X = db$PQ
xlab = "Pq"; ylab = "Y";
st.graph.hist(db, X,xlab,ylab)
# ggsave("graph/histnorm.png", width=4, height=4, dpi=300)

st.graph.qqnorm(db, X, xlab, ylab)
# ggsave("graph/qqnorm.png", width=4, height=4, dpi=300)

#Test Adherencia
test_adh = st.test.adherencia(X)
test_adh$shapiro
test_adh$lillie
test_adh$anderson



# X700MHz -----------------------------------------------------------------

X = db$X700MHz
xlab = "700MHz"; ylab = "Y";
st.graph.hist(db, X,xlab,ylab)
# ggsave("graph/histnorm.png", width=4, height=4, dpi=300)

st.graph.qqnorm(db, X, xlab, ylab)
# ggsave("graph/qqnorm.png", width=4, height=4, dpi=300)

#Test Adherencia
test_adh = st.test.adherencia(X)
test_adh$shapiro
test_adh$lillie
test_adh$anderson



# X1100MHz ----------------------------------------------------------------

X = db$X1100MHz
xlab = "1100MHz"; ylab = "Y";
st.graph.hist(db, X,xlab,ylab)
# ggsave("graph/histnorm.png", width=4, height=4, dpi=300)

st.graph.qqnorm(db, X, xlab, ylab)
# ggsave("graph/qqnorm.png", width=4, height=4, dpi=300)

#Test Adherencia
test_adh = st.test.adherencia(X)
test_adh$shapiro
test_adh$lillie
test_adh$anderson


# X1400MHz ----------------------------------------------------------------


X = db$X1400MHz
xlab = "1400MHz"; ylab = "Density";
st.graph.hist(db, X,xlab,ylab)
# ggsave("graph/histnorm.png", width=4, height=4, dpi=300)

st.graph.qqnorm(db, X, xlab, ylab)
# ggsave("graph/qqnorm.png", width=4, height=4, dpi=300)

#Test Adherencia
test_adh = st.test.adherencia(X)
test_adh$shapiro
test_adh$lillie
test_adh$anderson



# Analisis de as asmostras ------------------------------------------------


#Test de Friedman con post test de nemenyi
X = as.matrix(db[,3:6]);
test_friedman = st.test.friedman(X, 0.05)
test_friedman$tfriedman
test_friedman$ptnemenyi


#





