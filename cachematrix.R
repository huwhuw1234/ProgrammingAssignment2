##This is the function for week 2
##Creating a matrix function that will cache a computation
##making it to save time by not computing each time

## makeCacheMatrix is the function that creates a matrix list

makeCacheMatrix <- function(x = matrix()) {
  ##Set value vector
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get value of matrix
  get <- function() x
  ##set value of inverse
  setinverse <- function(inv) m <<- inv
  ##get value of inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
##Check to see if function has been solved 
   m <- x$getinverse()
##if it is solved print getting cached data and return stored data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
##if matrix is not cached send to makeCacheMatrix, create inverse and save (list)
  data <- x$get()
  m <- inv(data, ...)
  x$setinverse(m)
  m
}
## Return a matrix that is the inverse of 'x'
