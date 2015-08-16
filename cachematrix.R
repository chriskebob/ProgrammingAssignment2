## These functions use the "<<-" operator to assign a cached value
## to an object in an environment different from the current environment.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

# 1.  set the value of the matrix - use the function makeCacheMatrix$set(x)
#     to update the value of the matrix and clear the cache object m
# 2.  get the value of the matrix - just retrieves the matrix itself from 
#     the original function call to makeCacheMatrix
# 3.  set the value of the inverse in the cache object m
# 4.  get the value of the inverse - used if the inverse has been calculated
#     already

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the matrix has already been inverted. If so, it uses
## the getinverse function created above and skips the computation. If the 
## inverse has not been calculated, setinverse is used to solve the matrix 
## inversion and sets the value of the inverse to the solve object.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
}
