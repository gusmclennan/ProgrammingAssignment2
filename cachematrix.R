## The first function creates an object that stores a matrix and its inverse.  The second
## function takes an argument from the first function in order to return the inversed matrix
## stored in the cache, or calculate the inverse using the Solve function if inverse has not 
## been cached.

## The makeCacheMatrix function builds a set of functions that either set or retrieve a
## matrix and return the functions as a list to parent environment
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setmatrix <- function(matrix) m <<- matrix
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
   
}


## The cacheSolve function either computes the inverse of the matrix returned by makeCacheMatrix
## function, or if the inverse of matrix has been cached, returns that value
cacheSolve <- function(x, ...) {
        
        m <- x$getmatrix()
        ## If m object contains a value (a cached value), return the cached matrix
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        ## Return a matrix that is the inverse of 'x'
        m <- solve(data)
        x$setmatrix(m)
        m
}


## Test functions by defining test matrices
# a <- matrix(c(1/2, -1/4, -1, 3/4),2,2)
# b <- makeCacheMatrix(a)
# b$get()
# b$getmatrix()
# b$set(matrix(1:4, 2, 2))
# cacheSolve(b)
# b$getmatrix() 

