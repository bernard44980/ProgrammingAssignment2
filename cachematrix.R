## [Put comments here that describe what your functions do]

makeCacheMatrix <- function(x = matrix()) {
  
  #This creates a special matrix which should contain in fact
  #a list of 4 functions

  inverse <- NULL  #this initialize the object that will store the inverse matrix
  
  #The first function called "set" will set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #the second function called "get" will get the value of the matrix
  get <- function() x
  
  #the third function called "setinverse" will set the value of the inverse of the matrix
  # it uses the function solve
  setinverse <- function(solve) inverse <<- solve
  
  #the fourth function called "getinverse" will get the value of the inverse of the matrix
  getinverse <- function() inverse
  
  #below is the instruction that returns a list of the 4 functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse() #this does not work : "Error in x$getinverse : $ operator is invalid for atomic vectors"
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}