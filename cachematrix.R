## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Part 1
##create a new function in studio and set value of the matrix
makeCacheMatrix <- function(x=matrix()) {
  ##use i represent inverse
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the value of the inverse
  seti <- function(inverse) i<<- inverse
  ##get the value of the inverse
  geti <- function() i
  list(set=set, get=get,
       seti = seti,
       geti = geti)
  
}


## Write a short comment describing this function

##Part 2
cacheSolve <- function(x,...) {
  i <- x$geti()
  ##check if the inverse has already been calculated
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  ##otherwise, calculate the inverse of the matrix
  inverse <- x$get()
  i <- solve(inverse, ...)
  x$seti(i)
  i
}
