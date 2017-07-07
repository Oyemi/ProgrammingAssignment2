## The first function creates a matrix that can cache its inverse, 
## while the second function retrieves the cached inverse of the matrix 

## 'makeCacheMatrix' returns a list that contains functions to:
##        1. set the matrix
##        2. get the matrix
##        3. set the inverse of the matrix
##        4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) m <<- solve
          getinverse <- function() m
          list(set = set, get = get, 
               setinverse = setinverse, 
               getinverse = getinverse)
}


## 'cacheSolve' computes and/or retrieves the inverse of the matrix 
##  in 'makeCacheMatrix'.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
          if(!is.null(m)) {
            message("getting cached data")
            return (m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}

## for example
a_matrix <- makeCacheMatrix(matrix(1:4,2,2))
a_matrix$get()
a_matrix$getinverse()
cacheSolve(a_matrix)
