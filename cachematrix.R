
## This set of functions are useful to cache the inverse of a matrix in order
## to save time when computing it repeatedly

# This function creates a special "matrix" object that contains four functions 
# that operate over the object:
# 1.- allows to set the value of the matrix
# 2.- allows to get the value of the matrix
# 3.- allows to set the value of the inverse matrix
# 4.- allows to get the value of the inversematrix

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    # Code to set the value of the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    # Code to get the value of the matrix
    get <- function() x
    
    # Code to set the value of the inverse matrix
    setinv <- function(solve) m <<- solve
    
    # Code to get the value of the inverse matrix
    getinv <- function() m
    
    # List of functions in the object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}
  
# This function calculates the inverse of the "special" matrix created
# with "makeCacheMatrix". It only calculates de inverse matrix if it has not
# already been calculated. If so, it returns the inverse matrix from the cache
# and skips the computation.

cacheSolve <- function(x, ...) {

    # It looks for the already calculated inverse matrix
    m <- x$getinv()
    
    # If the result is not null, show the pre-calculated inverse matrix
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    # If not, do the calculation, store it, and show it
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m

}
