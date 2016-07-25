## Write a short comment describing this function

#This function creates a object that has the ability to store a matrix and its inverse. The object also has
#get/set functions to set the matrix. Also there is a getinverse method to obtain the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# cacheSolve computes the inverse of the matrix it references. In case the inverse has already been calculated
# since when the last time the matrix was set, then it returns the inverse from the cached value, else it
# calculates the inverse, stores it in the cache and also returns the inverse matrix value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
