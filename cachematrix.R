## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Part 1 makeCacheMatrix function to create an unique matrix
##create a new function in studio and set value of the matrix
makeCacheMatrix <- function(x=matrix()) {
  ##use i represent inverse; assign NULL to i
  i <- NULL
  ##function in a different environment; use <<- instead of <-
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the value of inverse
  seti <- function(inverse) i<<- inverse
  ##get the value of inverse
  geti <- function() i
  ##return a list composed of value being tagged
  list(set=set, get=get,
       seti = seti,
       geti = geti)
  
}


## Write a short comment describing this function

##Part 2 cacheSolve function to test and computate the inverse from cache
cacheSolve <- function(x,...) {
  ##get inverse value from cache if any
  i <- x$geti()
  ##check if the inverse has already been calculated
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  ##otherwise, calculate the inverse of the matrix and saved it in cache
  inverse <- x$get()
  i <- solve(inverse, ...)
  x$seti(i)
  i
}
