## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix will caches the value of inverse of a particular matrix
## Initially it will have value of inverse stored as Null.  
## Once inverse is calculated it will be cached(stored) in m by setinverse()function.   
## Next time inverse of matrix is to be calculated getinverse() function returns m(inverse of the matrix).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                     
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve checks if inverse of matrix is cached. If inverse is cached it prints inverse(using getinverse()
## function) else it calculates inverse and sends inverse to setinverse() function so that inverse is cached

cacheSolve <- function(x,...) {
## Return a matrix that is the inverse of 'x'
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
