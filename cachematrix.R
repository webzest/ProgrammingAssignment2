## Code two functions that This function creates a special "matrix"
## object that can cache its inverse and makes it available
## when needed, saving computation time and resources

## First cache matrix creation process

makeCacheMatrix <- function(x = matrix()) {

  ## initialize the inverse variable
  ma <- NULL

  ##  Setting the matrix
  set <- function(b) {
    x <<- b
    ma <<- NULL
  }

  ## Getting the matrix function
  get <- function() x

  ## function that sets the matrix cache
  setInverse  <- function(cache) ma <<- cache

  ## function that gets the matrix cache
  getInverse  <- function() ma

  ##  Return the results
  list(set = set, get = get,
       setInverse  = setInverse ,
       getInverse  = getInverse )
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ma <- x$getInverse ()

  ## simply return the cache if it is already set
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)
  }

  ## Getting the matrix's data
  data <- x$get()

  ## Computing the inverse of a square matrix
  ma <- solve(data, ...)

  ## Setting the inverse
  x$setInverse (ma)

  ## Return the matrix
  ma
}

# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# my_matrix$get()
# my_matrix$getInverse()
# cacheSolve(my_matrix)
# cacheSolve(my_matrix)
# my_matrix$getInverse()
#
# my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
# # my_matrix$get()
# my_matrix$getInverse()
# cacheSolve(my_matrix)
# cacheSolve(my_matrix)
# my_matrix$getInverse()
