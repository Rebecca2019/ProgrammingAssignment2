## This two functions that are used to create a special object 
## that stores a matrix and cache's its inversed matrix.

## This function create an object that contains 4 functions:
## 1. set the matrix
## 2. get the matrix
## 3. set the inversed matrix
## 4. get the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function first checks to see if the inversed matrix 
## has already been calculated. If so, it gets the inversed matrix 
## from the cache and skips the computation. Then, it calculates the 
## inversed matrix and sets the value of the inversed matrix in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

