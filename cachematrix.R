## 2 functions that cache the inverse of a matrix
## Usage: Pass the result of a makeCacheMatrix and call to cacheSolve 

# function that set the matrix and the inverse in an environment
# x an invertible matrix
# x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
# x$set(matrix(rnorm(16), 4, 4))
# Instruction: Pass the result of a makeCacheMatrix call to cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# parameter x the result of a previous makeCacheMatrix call
# parameter ... additional arguments to pass and solve the function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
