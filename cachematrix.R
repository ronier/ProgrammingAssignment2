
## Generates a matrix object that will cache is reverse.
makeCacheMatrix <- function(x = matrix()) {
  ##Init the matrix cache as NULL
  m <- NULL
  ##Define the setter functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##Define the getter function
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ##Return the list with all the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the value of the current matrix
  m <- x$getinverse()
  ## if is not null, we already made this calculation so we 
  ## can return it without performing any additional calc and
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## we will continue with the calculation to generate the inverse 
  data <- x$get()
  ## we pass the data to "solve" to generate the inverse
  m <- solve(data, ...)
  ## save it on the cache matrix object
  x$setinverse(m)
  ## and then return it
  m
}

