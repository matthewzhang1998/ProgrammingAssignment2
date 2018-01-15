## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix is a constructor function that allows for the caching of a matrix and its inverse
#cacheSolve returns the cached inverse if present, else it computes it and stores in cache

## Write a short comment describing this function

# returns a list of functions set, get, setInv and getInv (similar to example with vectors)
# that allows for caching of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Ensures inv has NULL value if unassigned
  inv <- NULL
  
  #set: caches value of matrix and resets inv;
  #args: y (matrix)
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  
  #get: returns cached matrix
  #args: NULL
  get <- function() x
  
  #setInv: caches inverse matrix
  #args: solve (inverted matrix)
  setInv <- function(solve) inv <<- solve

  #getInv: returns cached inverse
  #args: NULL
  getInv <- function() inv
  
  #returns list containing all functions set, get, setInv and getInv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

# cacheSolve returns the inverted matrix, fetching from cache if possible and computing/storing it otherwise

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
