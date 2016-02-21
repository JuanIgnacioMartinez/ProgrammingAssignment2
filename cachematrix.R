##  This function creates a special "matrix" object that can cache its inverse

## This function save the inverser of a matrix in the cache so it can be
## called again whitrout all the calculation so it saves time and 
## computer requirements.
## This function use the following functions: set, get, setmean and getmean.

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


## This functions call the inverse of the matrix used in the function 
## makeCacheMatrix that its saved in the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {

    return(m)
    
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
  
  ## Finally it returns a matrix that is the inverse of the original matrix 'x'
