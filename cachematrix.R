
## These functions are a relatively simple extension of the examples given in the programming assignments.
## Instead of computing the mean of a vector, they compute the inverse of a matrix.
## The main innovation is that the function is able to cache an intermediate result, so that if the inverse of a function has
## already been computed, it does not need to re-compute, but fetches the result from cache.


## This function stores the inverse of a matrix in a variable outside of the scope of the function itself. 
## It can thus be retrieved by another function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function solves for the inverse of a matrix. 
## It first checks whether the inverse has already been computer (!is.null(m)). If yes, it fetches the precomputed version.
## Otherwise it computes the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getsolve()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
