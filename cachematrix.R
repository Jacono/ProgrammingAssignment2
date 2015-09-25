## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Make a special matrix, consist in a list of object that are function that perform simply operation on matrix


makeCacheMatrix <- function(x = matrix()) {
      
      # inv will store the cached inverse matrix
      inv <- NULL
      
      # Save matrice
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      # Retrive matrix
      get <- function() x      
      
      
      setinv <- function(inverse) inv <<- inverse
      
      getinv <- function() inv
      
      # Create function list
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# If the inverse has already been calculated (and the matrix has not changed)
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      
      # If already calculated
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }      
      
      data <- x$get()
      inv <- solve(data, ...)      
      # Cache the inverse
      x$setinv(inv)  
      inv
}
