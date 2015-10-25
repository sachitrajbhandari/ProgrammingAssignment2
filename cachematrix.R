## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special "matrix", which is a list of following functions 
# set - sets the value of the matrix
# get - gets the value of the matrix
# setinv - sets the cached value of the inverse matrix
# getinv - gets the cached value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize cached inv value to NULL
  inv <- NULL
  
  # set matrix and flush cache value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # returns matrix set
  get <- function() x
  
  # set cache matrix
  setinv <- function(inverse) inv <<- inverse
  
  # returns cached matrix
  getinv <- function() inv
  
  # return list of function
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

# calculates the inverse of a special "matrix" if no cached matrix exists
cacheSolve <- function(x, ...) {
  
  # Retrieve a cached matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # check if cached exists and if exists returns cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # retrieve matrix
  data <- x$get()
  
  # calculate inverse matrix
  inv <- solve(data, ...)
  
  #set inverse matrix
  x$setinv(inv)
 
   # return inverse matrix
  inv
}
