## Put comments here that give an overall description of what your
## functions do

## makecachematrix provides way for inverse matrix to be cached for future use 
## and the same can be accessed through lexical scoping.

makecachematrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function checks if the inverse of a matrix is already cached, 
## if not it calculates the inverse and caches it for future reference.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
