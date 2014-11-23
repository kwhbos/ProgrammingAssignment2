## makeCacheMatrix() - will set, get, setinverse, getinverse for a matrix using
## cacheing to ensure the inverse is not recalculated if the original matrix has
## not changed
## cacheSolve() - executes the functions of makeCacheMatrix() and indicates if
## cacheing has been used

## makeCacheMatrix() - call using a matrix and assign the result to a variable 
## e.g. mymatrix and then the get, set, getinverse and setinverse functions are
## available as mymatrix$..

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() - call using a matrix, and it will show the inverse and whether
## it has been supplied from a cached result or is a new calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
