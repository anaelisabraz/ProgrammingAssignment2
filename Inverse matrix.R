

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  aux <- function(y) {
    x <<- y
    i <<- NULL
  }
  obter <- function() x
  aux_inversa <- function(inversa) i <<- inversa
  obter_inversa <- function() i
  list(aux = aux,
       obter = obter,
       aux_inversa = aux_inversa,
       obter_inversa = obter_inversa)
  }  





cache_inverter <- function(x, ...) {
  i <- x$obter_inversa()
  if (!is.null(i)) {
    message("Obtendo dados de cache")
    return(i)
  }
  data <- x$obter()
  i <- solve(data, ...)
  x$aux_inversa(i)
  i
}
  
  
a <- matrix(c(1,2,3,4,5,6,7,9,8), 3)
b <- makeCacheMatrix(a)
cache_inverter(b)
