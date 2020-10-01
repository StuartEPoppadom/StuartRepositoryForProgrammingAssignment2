## Caching the Inverse of a Matrix
## A pair of functions that cache the inverse of a matrix instead of
## computing it repeatedly.

## Used sample code in 'Example: Caching the Mean of the Vector". Replaced all 
## code using 'mean' with 'inverse', with one exception. The exception is 
## that the funtion mean() is replaced with solve() per the instructions 
## of the assignment.


## makeCacheMatrix: creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m 
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}

       
## cacheSolve: computes the inverse of the special matrix returned by
##      makeCacheMatrix above.        

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m))   {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}     

        
