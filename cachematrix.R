## Example Matrix
## > m <- makeCacheMatrix(matrix(c(3, 4, 1, 2), c(2, 2)))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]    1 -0.5
## [2,]   -2  1.5



## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mat <<- inverse
  getinv <- function() mat
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        mat <- x$getinv()
        if(!is.null(mat)){
          message("Retrieving cached data")
          return(mat)
        }
      dat <- x$get()
      mat <- solve(dat, ...)
      x$setinv(mat)
      mat
}
