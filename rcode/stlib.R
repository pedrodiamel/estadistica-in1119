
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
  
  #Post test de nemenyi
  ptn = posthoc.friedman.nemenyi.test(X);
  
  
  res = list(tfriedman = tf, ptnemenyi = ptn)
  return (res);
  
  
}


