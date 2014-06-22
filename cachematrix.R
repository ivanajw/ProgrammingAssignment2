# Example:
# Create a matrix x
# x <- matrix(rnorm(9), nrow = 3)          
# # Create special matrix object
# sx <- makeCacheMatrix(x)      
# # Return the matrix
# sx$get()                                  
# # Return the inverse
# cacheSolve(sx)                            
# # Call the 2nd time, return the cached inverse
# cacheSolve(sx)                            


# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Set the value for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the value for the matrix
  get <- function() x
  
  # Set the value for the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Get the value for the inverse
  getinv <- function() inv
  
  # Return the matrix with defined functions above
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated, then the cachesolve 
# retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is already calculated returns it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Calculate the inverse if it is not calculated
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}

