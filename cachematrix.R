#Part 1
makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinverse <- function(inv) inverse <<- inv;
    getinverse <- function() return(inverse);
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

#Part 2
cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    inverse <- solve(data, ...)
    mtx$setinverse(inverse)
    return(inverse)
}
