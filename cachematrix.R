## The following functions calculate an inverse of a matrix and store the cache

## This function creates cache list which stores matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function calculates inverse of x, if it is not yet stored.
## If there is already a cache stored, it returns the cached data

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