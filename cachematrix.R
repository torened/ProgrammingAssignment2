## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## The object consists of a list of functions

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse){
                inverse_matrix <<- inverse
        } 
        getinverse <- function() inverse_matrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mat <- x$getinverse()
        ## Check if the reult is cached
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        ## If the result isn´t cached, create the matrix and invert it
        data <- x$get()
        mat <- solve(data, ...)
        
        x$setinverse(mat)
        ## Display matrix
        mat
}
