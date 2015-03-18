## Assignment: Caching the Inverse of a Matrix
## This program is based in two functions in where you can
## create a matrix and store it in cache. Then solve
## the matrix to get the inverse. If the result is already
## stored in cache, solving the matrix would not be necessary. 

## This function creates a special "matrix" object that can save in cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## Define the inverse matrix variable
        inverse_matrix <- NULL
        
        ## A function that set the value of the matrix
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        ## A function that get the value of the matrix
        get <- function() x
        
        ## A function that set the value of the inverse of the matrix
        setinverse <- function(inverse) inverse_matrix <<- inverse
        
        ## A function that get the value of the inverse of the matrix
        getinverse <- function() inverse_matrix
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## open the library to solve non square matrix
        library(MASS)
        
        ## get the inverse matrix of "x" and save it in inverse_matrix
        inverse_matrix <- x$getinverse()
        
        ## serch in cache if the value already exists
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        
        ## if the value does not exist calculate the inverse value of the matrix
        inverse_matrix <- ginv(x$get())
        
        ## set the inverse value of the matrix in cache
        x$setinverse(inverse_matrix)
        
        ## show the inverse value of the matrix
        inverse_matrix
}
