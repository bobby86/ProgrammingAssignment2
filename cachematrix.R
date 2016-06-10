## the function in this R file saves the inverse of a matrix in cache for later use.
## this eliminates the need for expensive computation of the same solution repeatedly.

## makeCacheMatrix function creates a list that stores the matrix and the inverse of the matrix
##  as various elements of the list.

makeCacheMatrix <- function(matrix = matrix()) {
    inv <- NULL
    set <- function(y) {
        matrix <<- y ## Storing the matrix as an element of the list
        inv <<- NULL
    }
    get <- function() matrix
    setinv <- function(sol) inv <<- sol
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function checks if the inverse of the matrix is already calculated. if so, the 
##  inverse from the cache is returned else the inverse is calculated and saved to the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) ## Returning a matrix that is the inverse of 'x', from cache
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv     ## Returning a matrix that is the inverse of 'x', calculated
}
