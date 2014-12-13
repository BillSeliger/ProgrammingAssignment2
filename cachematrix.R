## Bill Seliger Programming Assignment 2 for rprog-016
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL          ## sets m to null in the function environment
    set <- function(y) {        
      x <<- y         
      m <<- NULL      ## sets m to null in the global environment
    }
    get <- function() x                      ## returns value of original matrix
    setsolve <- function(solve) m <<- solve  ## stored at the global environment
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix()) {   
  m <- x$getsolve()
  if(!is.null(m)) {                           ## if there is data in the matrix m
    message("getting cached data")            ## print "getting cached data"
    return(m)                                 ## return matrix m 
  }                                           ## if m is.null 
  data <- x$get()                             ## matrix is stored at x$get()
  m <- solve(data)                            ## m <- the inverse of the original matrix
  x$setsolve(m)                               ## 
  m                                           ## return m 
}