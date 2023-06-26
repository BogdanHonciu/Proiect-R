# Integreaza o functie si returneaza "false" daca exista erori
integrare <- function(f, d = c(-Inf, Inf)){
  tryCatch(integrate(Vectorize(f), d[1], d[2]),
           error = function(e){
             print(e)
             FALSE
           })
}

# Integrare dubla x y
integrare_dubla <- function(f, dx, dy){
  integrate(
    Vectorize(function(y){
      integrate(function(x){f(x, y)}, dx[1], dx[2]) $ value
    })
    , dy[1], dy[2]) $ value
}
