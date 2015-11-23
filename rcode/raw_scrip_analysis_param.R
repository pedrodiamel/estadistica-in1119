
#definidas: X1, X2

##Analisis parametrico
# H_0: miu_1 == miu_2
# H_1: miu_1 != miu_2

##Graficar
plot(X1,X2,pch=19,col="blue",cex=0.5, xlab="X1",ylab="X2");
#smoothScatter(X1,X2);
boxplot(X1,X2,col="blue");

#T-Student tests
#Performs one and two sample t-tests on vectors of data.
alts = c("two.sided", "less", "greater");
alt = alts[1];
tt = t.test(X1,X2, alternative = alt);
cat("T-Student \n");
cat("P-value =", tt$p.value, "\n" );