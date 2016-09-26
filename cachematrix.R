## The below two functions help us understanding the concept of caching in R

## This function is the base which sets up all the necessary functions and variables required for 
## setting up caching and retrieving the data

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieves the cached value if it is already present else it calculates the 
## inverse of the matrix and stores the data in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("calculating inverse..")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
