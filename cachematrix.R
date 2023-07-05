## Caching the inverse of a matrix

## Function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
                }
        get <- function() {x}
        setInverse <- function(inversecalculation) {inverse <<- inversecalculation}
        getInverse <- function() {inverse}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
       }


## Function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
                }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
        }
