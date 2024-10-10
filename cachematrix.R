# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will store the cached inverse
  
  # Method to set the matrix (and reset the cached inverse)
  set <- function(y) {
    x <<- y  # Update the matrix
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  # Method to get the matrix
  get <- function() x  # Return the matrix
  
  # Method to set the inverse (cache the inverse)
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the methods to interact with the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then it retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")  # If cached, return the cached inverse
    return(inv)
  }
  
  # If the inverse is not cached, calculate it
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse using solve()
  x$setInverse(inv)  # Cache the computed inverse
  
  return(inv)  # Return the computed inverse
}
