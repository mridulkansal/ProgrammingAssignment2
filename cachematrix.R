## A pair of functions that cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL                             # initializes inverse to null
   set <- function(y) {                    # sets a new matrix, initializes inverse to null
     x <<- y
     inv <<- NULL
   }
  get <- function() { x }                  # returns the cached matrix 
  setinverse <- function(i)  { inv <<- i } # sets the cached inverse
  getinverse <- function() { inv }         # gets the cached inverse         
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes matrix that is the inverse of the 
## special "matrix" (x) returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {                 # if cached inverse is present, return it
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)                  # else, compute inverse, cache and return it
  x$setinverse(inv)
  inv
}
