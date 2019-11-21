## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below creates a list that allows to solve a matrix and if already solved, cache it

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        
        #task 1 : allowing to set the data 
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
  
        #task 2 : getting the data already stored 
        get <- function() x
  
        #task 3 : setting the cache matrix 
        setinverse <- function(inversex) {
                mat <<- inversex
        }
  
        #task 4: getting the cached matrix
        getinverse <- function() mat
  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Function below checks if there is a cache inverse matrix. If not, it solves and returns. If yes, returns the cache matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
        mat <- x$getinverse()
    
        # Checking if already cached
        if (!is.null(mat)) {
                print("getting cached inverse matrix")
                return(mat)   ## returns from function if already computed
        }
  
        # solving if not cached 
        data_mat <- x$get()
  
        mat <- solve(data_mat)
  
        x$setinverse(mat)
  
        mat
}
