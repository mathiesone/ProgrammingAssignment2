## Pass a matrix through the "makeCacheMatrix" and store the result as a variable.
## Run this variable through "cacheSolve" to find the inverse of the Matrix.
## If the value of the inverse has been previously calculated, this will be returned
## from cache.

## A "object" function, stores 4 functions that aid in the search and
## cache storage of the inverse of the matrix x.
makeCacheMatrix <- function(x = matrix()) {
        
        ## Set inverse_matrix to NULL.  Incase there is any history.
        inverse_matrix <- NULL
        
        ## Define "set" function.  Only called when a new matrix is defined.
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        ## Define "get" function.  Used to pull data stored out.
        get <- function() x
        ## Define "setinverse".  Inverse stored when it was not previously available.
        setinverse <- function(inv) inverse_matrix <<- inv
        ## Define "getinverse".  Value available if cache stored.
        getinverse <- function() inverse_matrix
        list(set = set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## Uses the "makeCacheMatrix" functions output to see if an inverse
## matrix has already been found. If so, this is returned (with a cache message).  
## If not the inverse is calculated and returned.
cacheSolve <- function(x, ...) {
        
        ## If the value is already stored, find this value,
        inverse_matrix <- x$getinverse()
        ## IF function to see if a value was stored.  If so
        ## Return this value and a comment.  Exit function.
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return (inverse_matrix)
        }
        ## No value stored previously.  Retreive the matrix.
        data <- x$get()
        ## Calculate the inverse of the matrix and set to a variable.
        inverse_matrix <- solve(data, ...)
        ## Store the inverse matrix in cache.
        x$setinverse(inverse_matrix)
        ## Return the inverse matrix.
        inverse_matrix
}
