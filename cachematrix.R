
## Functions for creating and using inverted matrices which caching ability
## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

  ### 1. set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
  ### 2. get the value of the matrix
        get <- function() x
	### 3. set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
	### 4. get the value of the inverse of the matrix
        getsolve <- function() m
	# return our list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() # retrieve from cache
    if(!is.null(m)) { # we got something...
          message("getting cached data")
          return(m)
        }
# cache isn't filled yet
    data <- x$get()       # get it
    m <- solve(data, ...) # solve it
    x$setsolve(m)	      # set it
    m		      # return it
}
