## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
  
        get <- function() x
  
        setinverse <- function(inversex) {
                mat <<- inversex
        }
  
        getinverse <- function() mat
  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
        mat <- x$getinverse()
  
        if (!is.null(mat)) {
                print("getting cached inverse matrix")
        return(mat)
        }
  
        data_mat <- x$get()
  
        mat <- solve(data_mat)
  
        x$setinverse(mat)
  
        mat
}
