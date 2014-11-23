## The makeCacheMatrix function creates a special matrix 
## The matrix is a list which contains a function for setting the value of the matrix,
## getting the value of the matrix, setting the value of the inverse and getting the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The following function computes the inverse of the special matrix, created above
## It first checks to see if the inverse has already been calculated
## If so, it gets the matrix nverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function

cacheSolve <- function(x, ...) {
                                    ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

