#Scoate x-ul din densitatea comuna a lui x si y
extract_x_marginal <- function(f, dx){
  Vectorize(function(y){
    integrate(function(x){f(x, y)}, dx[1], dx[2]) $ value
  })
}

#Scoate y-ul din densitatea comuna a lui x si y
extract_y_marginal <- function(f, dy){
  Vectorize(function(x){
    integrate(function(y){f(x, y)}, dy[1], dy[2]) $ value
  })
}

#Calculeaza covariata si corelatia coeficientului
covariance_and_correlation <- function(pdf, dx, dy){
  
  fx = extract_x_marginal(pdf, dx)
  fy = extract_y_marginal(pdf, dy)
  
  mx = medium(fx, dx)
  my = medium(fy, dy)
  
  cov = integrare_dubla(function(x,y){x*y*pdf(x,y)}, dx, dy) - (mx*my)
  
  cor = cov / (mx*my)
  
  list(cov = cov, cor = cor)
}


