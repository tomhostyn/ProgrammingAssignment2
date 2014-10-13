# Caching the Inverse of a Matrix
#
# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly
#
# This package offers a function to replace the solve function with one that caches its result 


## creates a cacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function (y) {
    inverse <<- NULL
    x <<- y
  }
  
  get <- function () {x}
  
  setInverse <- function (i) { inverse <<- i}
  
  getInverse <- function () {inverse}
  
  list (set = set,
        get = get,
        getInverse = getInverse,
        setInverse = setInverse)
}


## solves a matrix created by makeCacheMatrix.
## retrieves the result from local cache to avoid needless computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if (! is.null(i)){
    #message ("retrieving cached solved matrix")
    return (i)
  }
  
  i <- solve (x$get(),...)
  x$setInverse(i)
  i
}



testCachedMatrix <- function () {
 
  testID <- "TEST 1"
  r <- 4
  m <- matrix (runif(r*r, -1, 1), nrow = r)
  mi <- solve (m)  

  cm <- makeCacheMatrix(m)
  s <- cacheSolve(cm)
  if (!identical (s, mi)) {warning (cat (testID, "FAILED"))}
  
  
  testID <- "TEST 2"
  # test if we are really speeding up things...
  r <- 100
  m <- matrix (runif(r*r, -1, 1), nrow = r)
  solve_time <- system.time (replicate (1000,  solve (m)))  
  print ("solved matrix the hard way")
  print (solve_time)
  
  cm <- makeCacheMatrix(m)
  cache_time <- system.time (replicate (1000, cacheSolve(cm)))
  print ("solved matrix the fast way")
  print (cache_time)  
}