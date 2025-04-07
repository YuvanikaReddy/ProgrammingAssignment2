# Combined R code for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# Example usage:
my_matrix <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
cachedMatrix <- makeCacheMatrix(my_matrix)

# First call (computes the inverse)
inverse1 <- cacheSolve(cachedMatrix)
print(inverse1)

# Second call (retrieves cached inverse)
inverse2 <- cacheSolve(cachedMatrix)
print(inverse2)

