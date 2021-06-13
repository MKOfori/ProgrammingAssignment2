## The makeCacheMatrix creates special matrix object of which its inverse can be cached
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  get <- function() {x}  #function used in getting the matrix x
  setInverse <- function(inverse) {inv<<-inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

cachesolve <- function(x, ...){
  inv <- x$getInverse() # function used to get the inverse of the matrix
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}