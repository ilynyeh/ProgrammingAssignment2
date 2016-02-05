## These functions will cache the inverse of a matrix 

## makeCacheMatrix is a function that creates a special matrix and will then
## set/get contents of a matrix and then set/get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## cacheSolve first checks to see if the inverse has already been calculated. 
## If so, it will get the inverse from the cache and skip computation.
## If not, it calculates the inverse of the matrix with the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
