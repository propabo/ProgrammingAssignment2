## makeCacheMatrix is a function that takes a numeric matrix and creates a special matrix object which contains 
## functions to cache the matrix's inverse


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  # set the matrix function
  set <- function(y) {
    
    x <<- y
    m <<- NULL
  
  }
  
  # get the matrix function
  get <- function() x
  
  # set the inverse function
  setinv <- function(inv) m <<- inv
  
  # get the inverse function
  getinv <- function() m
  
  # store the functions in a list object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve is a function that takes the special matrix from makeCacheMatrix and checks to see if the inverse
## has already been calculated, if it has already been calculated it returns the inverse, otherwise it solves
## the matrix and stores it's inverse in the list.


cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  # check to see if the inverse has already been calculated
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
  
  }
  
  # if the inverse has not already been calculated and stored in the cache then calculate the inverse
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m

}
