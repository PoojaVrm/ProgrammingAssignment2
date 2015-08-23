## makeCacheMatrix() creates a special matrix object and then   
## cacheSolve()computes inverse of the special matrix returned by makeCacheMatrix()
## If the inverse has already been calculated (and the matrix has not changed),  
## then the cachesolve() retrieves the inverse from the cache.


## makeCacheMatrix() creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
    
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinv<- function(inverse) inverse_x <<-inverse
    getinv <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinv,
         getinverse = getinv)

}


##cacheSolve()calculates the inverse of special matrix returned by makeCacheMatrix()
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("Getting the cached inverse matrix")
        return(inverse_x)
    } else {
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
    }

}
