# These two functions makeCacheMatrix() and cacheSolve() takes advantage
# of the the scoping rules in R by making a cache and hereby saving computation time. 
# The first function creates a special matrix and enables cache.
# The second function computes inverse of matrix. If this matrix has already been inverted,
# the function returns the inverted matrix from cache defined in the function: makeCacheMatrix()

# Example:
# In this example we define a toepliz matrix and use our caching functions to first
# calculate and since retrieve the inversion.
# We then redefine the matrix to test the setting/resetting functionality followed by 
# calculation and caching of the new toeplitz matrix.

cm <- makeCacheMatrix(toeplitz(1:5))
print(round(cacheSolve(cm))) # Calculating inverse matrix
print(round(cacheSolve(cm))) #this print statement returns the first i (line 2)
cm$set(toeplitz(1:3))# Overwrite old data with new matrix
print(round(cacheSolve(cm))) #Calculate inverse of new matrix
print(round(cacheSolve(cm)))


## This function creates a special matrix object and this function can cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y # Our new data
    i <<- NULL # Resetting inverse cache.
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() return(i)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse of the special matrix, which is returned by makeCacheMatrix. 
##If the inverse of the matrix has already been calculated, then cacheSolve retrieves the inverse from the cache
## and hereby saves computation time.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i) #Retrieve inversion from cache
  }
  data <- x$get()
  i <- solve(data, ...) #Compute inverse of matrix
  x$setinv(i)
  return(i)
}

