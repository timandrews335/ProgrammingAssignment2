

## This function takes in a matrix as an argument and returns a list of matrices

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


## caches and displays a makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  i <- x$getinverse()
  if(!is.null(i)) {	#if it is not null, then we already have a cached value, so no need to call the solve function
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)		#get the inverse with the solve function
  x$setinverse(i)
  i
}


