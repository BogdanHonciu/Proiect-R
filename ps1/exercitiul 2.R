# Verifica daca o functie este pozitiva pe un interval dat
pozitiv <- function(f, d = c(-10000000, 10000000), step=10000000){
  # Set maximum lower and upper bounds
  if(d[2] == Inf)
    d[2] = 10000000;
  if(d[1] == -Inf)
    d[1] = -10000000;
  #Calculeaza valorile in intervale
  valori <- seq(d[1], d[2], len=step)
  #Verifica daca toate valorile sunt pozitive
  all(f(valori)>=0)
}

check_pdf <- function(f, d = c(-Inf, Inf)){
  #Verifica daca functia este pozitiva
  if(!pozitiv(f, d)){
    print("Functia este negativa")
    return(FALSE)
  }
  # In cazul in care functia data nu poate fi integrata
  i = integrare(f, d)
  # Nu se poate integra
  if(typeof(i)=='logical' && i==FALSE)
    return(FALSE)
  v = i $ value
  round(v) == 1
}

