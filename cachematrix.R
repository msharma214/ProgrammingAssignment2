## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Function to create a cache matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#This function finds inverse of cache matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #If the inverse has not been calculated:
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
