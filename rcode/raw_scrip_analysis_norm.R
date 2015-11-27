


########################################################################
##Analisis Exploratorio de los datos 
cat("\n\n");
print("Analisis Exploratorio de los datos");

cat("Variables descriptivas:", "\n");
cat("Mean:", mean(X), "\n"); 
cat("Sdv:", sd(X), "\n"); 
cat("N:", length(X), "\n"); 

cat("Summary:", "\n");
print(summary(X));


########################################################################
##Analisis de KURTOSIS/SKEWNESS
print("Analisis de KURTOSIS/SKEWNESS:");
cat(" Kurtosis:",  kurtosis(X), "\n");
cat(" Skewness:", skewness(X), "\n"); 


########################################################################
##Analisis Garfico
print("Analisis Garfico:");
par(mfcol = c(1,4)); # []1x4

#Hist plot
#h = hist(X, col="blue",breaks=10, main="Histograma");
h = hist(X, col="blue", main="Histograma");




#Density plots
dens <- density(X);
plot(dens,lwd=3,col="blue", main="Density");

#Box plot
bx = boxplot(X, col="blue", main="Box");

#QQ norm
qq = qqnorm(X, main="QQ-norm");
ql = qqline(X);



########################################################################
##Test Adherencia
# H_0: X es normal
# H_1: X no es normal

print("Test Aderencia:");

# Shapiro Wilk test
swt = shapiro.test(X);
cat("Shapiro Wilk test:", "\n");
cat("P-value:", swt$p.value, "\n");

# Lillie test
#Performs the Lilliefors (Kolmogorov-Smirnov) test 
#for the composite hypothesis of normality, see e.g. 
#Thode (2002, Sec. 5.1.1).
llt = lillie.test(X);
cat("Lillie test:", "\n");
cat("P-value:", llt$p.value, "\n");

#Performs the Anderson-Darling test for 
#the composite hypothesis of normality, see e.g. 
#Thode (2002, Sec. 5.1.4).
adt = ad.test(X)
cat("Anderson-Darling test:", "\n");
cat("P-value:", adt$p.value, "\n");

# Kormogorov Smirnov test
kst = ks.test(X, "pnorm", mean(X), sd(X));
cat("Kormogorov Smirnov test:", "\n");
cat("P-value:", kst$p.value, "\n");

# Qui-Quadrada
#chisq.test(X)

#person.test
#sf

########################################################################