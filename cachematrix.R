## Function makeCacheMatrix takes a square matrix and creates a matrix object and 
## can cache its inverse at the parent level.   
## Function cacheinverse determines the inverse of the array.  If the inverse of
## the array has already been calculated then the cached inverse is used, no 
## need to recalculate it.n

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- inv
                inv <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set
                ,get = get
                ,setinverse = setinverse
                ,getinverse = getinverse
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
