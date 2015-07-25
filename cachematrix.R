## Creates a matrix object that can cache its inverse.
## 'X' is a matrix object and 'invMatrix' is its inverse computed and cached

makeCacheMatrix <- function(x = matrix()) {
        invMatrix  <- NULL
        set  <- function(y){
                x <<- y
                invMatrix  <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) invMatrix    <<- inverse
        getinverse  <- function() invMatrix
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Computes the inverse of a matrix. If the inverse has already been computed it gets the result 
## using the below function cachesolve which should retrieve the inverse of the matrix from the cache.
## If not, it computes the inverse, sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMatrix  <- x$getinverse()
        if (!is.null(invMatrix)){
                message("getting cached data")
                return(invMatrix)
        }
        data  <- x$get()
        invMatrix  <- solve(data, ...)
        x$setinverse(invMatrix)
        invMatrix
}
