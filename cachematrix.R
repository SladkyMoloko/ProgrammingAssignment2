## Used in conjunction, these functions will find the inverse of 
## a matrix and cache the result, so that it can be retrieved later

## This function produces a list that contains the matrix and, once
## it has been calculated by cacheSolve, the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function finds the inverse of the matrix stored by 
## makeCacheMatrix; if the inverse has already been calculated, it
## will return the cached result

cacheSolve <- function(x, ...){
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

