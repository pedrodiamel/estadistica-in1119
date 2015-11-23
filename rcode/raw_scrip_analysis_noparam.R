

#definidas: X1, X2

##Analisis no parametrico
# H_0: miu_1 == miu_2
# H_1: miu_1 != miu_2

##Graficar
plot(X1,X2,pch=19,col="blue",cex=0.5, xlab="X1",ylab="X2");
#smoothScatter(X1,X2);
boxplot(X1,X2,col="blue");


#Wilcoxon tests
#Performs one- and two-sample Wilcoxon tests on vectors of data;
#the latter is also known as ‘Mann-Whitney’ test.
alts = c("two.sided", "less", "greater");
alt = alts[3];
wlkt = wilcox.test(X1,X2, alternative = alt, paired = T);
cat("Wilcoxon tests \n");
cat("P-value =", wlkt$p.value, "\n" );
print(wlkt)

