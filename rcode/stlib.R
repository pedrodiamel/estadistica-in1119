
#' Estadística descriptiva
#'
#' @param X dados de la tabla
#'
#' @return 
#' @export
#'
#' @examples
st.descrip_measure <- function( X ){
  
  media = mean(X);  
  desviacion = sd(X);  
  varianza = var(X); 
  minimo = min(X);  
  maximo = max(X); 
  mediana = median(X); 
  rango = range(X); 
  quartiles = quantile(X); 
  n = length(X);
  kt = kurtosis(X);
  skw = skewness(X);
  
  res = list(media = media, desviacion = desviacion,  
             varianza = varianza, minimo = minimo, maximo = maximo,
             mediana = mediana, rango = rango, quartiles =quartiles,
             n = n, kt = kt, skw = skw);
  
  return (res)
  
  
}

st.descrip_measure.toSting <- function(X){
  
  media = mean(X);  
  desviacion = sd(X);  
  varianza = var(X); 
  minimo = min(X);  
  maximo = max(X); 
  mediana = median(X); 
  rango = range(X); 
  quartiles = quantile(X); 
  n = length(X);
  kt = kurtosis(X);
  skw = skewness(X);
  
  cat("media:", media ,"\n")
  cat("desviacion:", desviacion,"\n")
  cat("varianza:", varianza,"\n")
  cat("minimo:", minimo,"\n")
  cat("maximo:", maximo,"\n")
  cat("mediana:", mediana,"\n")
  cat("rango:", rango[1],rango[2],"\n")
  cat("quartiles:", quartiles[1], quartiles[2], quartiles[3], quartiles[4], quartiles[5],"\n")
  cat("cout:", n,"\n")
  cat("kurtosis:", kt,"\n")
  cat("skewness:", skw,"\n")
  
  
}



#' Analisis 
#'
#' @param Analisis Garfico
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
st.graph.hist <- function(X){

  
  
}


#' Title
#'
#' @param Test Adherencia
#' H_0: X es normal
#' H_1: X no es normal 
#'
#' @return
#' @export
#'
#' @examples
st.test.adherencia <- function ( X ){
  
  
  # Shapiro Wilk test
  swt = shapiro.test(X);
  
  # Lillie test
  #Performs the Lilliefors (Kolmogorov-Smirnov) test 
  #for the composite hypothesis of normality, see e.g. 
  #Thode (2002, Sec. 5.1.1).
  llt = lillie.test(X);
  
  
  #Performs the Anderson-Darling test for 
  #the composite hypothesis of normality, see e.g. 
  #Thode (2002, Sec. 5.1.4).
  adt = ad.test(X)
  
  
  # Kormogorov Smirnov test
  #kst = ks.test(X, "pnorm", mean(X), sd(X));
  kst = 0;
  
  
  res = list(shapiro = swt, lillie = llt, anderson = adt, kormogorov = kst );
  return (res)
  
}




#' Test no parametrico de friedman con post test de nemenyi
#' H_0: mu_1 = mu_2 = ... m_n
#' H_1: Emu_i != mu_j, i!=j 
#' 
#' @param X datos
#' @param p nivel de significancia 
#'
#' @return
#' @export
#'
#' @examples
st.test.friedman <- function(X, p){
  
  
  #Test de friedman
  tf = friedman.test(X); 
  ptn = 0; 
  
  
  #Si existen diferencias significativas
  #aplicar post test
  if ( tf$p.value < p ){ 
    
    #Post test de nemenyi
    ptn = posthoc.friedman.nemenyi.test(X);
  
  }
  
  res = list(tfriedman = tf, ptnemenyi = ptn)
  return (res);
  
  
}


