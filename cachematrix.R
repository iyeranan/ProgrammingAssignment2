## Functions below indicate caching mechanism implemented for matrices using the <<- operator and lexical scoping techniques

## This function creates a special "matrix" object that can cache its inverse. It is a list containing a function to
## set the Matrix
## get the Matrix
## set the inverse
## get the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

## The following function calculates the inverse of the special matri created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it fetche from cache and skips the computation. 
## Else, it calculates the inverse sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}