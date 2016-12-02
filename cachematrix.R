## TThis function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Initializing inverse property
    inverse <- NULL
    
    ## This function sets the value of the vector and sets cached inverse to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## This function returns the value of the vector
    get <- function() {
        x
    }
    
    
    ## This function sets the cached value of the inverse of x to i
    setInverse <- function(i) {
        inverse <<- i
    }
    
    ## This function returns the value of the cached inverse
    getInverse <- function() {
        inverse
    }
    
    ## Returns the four functions as elements of a list
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    ##If the item exists in the cache
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
        
    }
    
    ## Else, then it computes it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    ## Returns a matrix that is the inverse of 'x'
    inv
}
