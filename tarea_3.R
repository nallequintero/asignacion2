makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inicializando
  w <- NULL
  
  ## Ver matriz
  set <- function( matrix ) {
    m <<- matrix
    w <<- NULL
  }
  
  
  get <- function() {
    m
  }
  
  ## inversa de la matrix
  setInversa <- function(inversa) {
    w <<- inversa
  }
  
  
  getInversa <- function() {
      w
  }
  
  
  list(set = set, get = get,
       setInversa = setInversa,
       getInversa = getInversa)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getInversa()
  
  
  if( !is.null(m) ) {
    message("obteniendo datos")
    return(m)
  }
  
    data <- x$get()
  
  ## inversa de una matriz con multiplicación
  m <- solve(data) %*% data
  
  x$setInversa(m)
  
  ## devolver la matriz
  m
}
