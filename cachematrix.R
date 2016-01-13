## The first functions allows to create a matrix object that can
## cache its inverse and the second function calculates the inverse
## of such an object by retrieving it from the cache or calculating
## it and caching it for future use.


## This function creates a matrix object which can cache its
## inverse, it has set and get methods and setInverse and
## getInverse methods.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(i){
        inverse <<- i
    }
    getInverse <- function(){
        inverse
    }
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function returns the inverse of a given matrix object,
## if the inverse has been calculated before, it is retrieved from
## the cache, otherwise it is calculated and cached for future use.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(is.null(inverse)){
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
    } else {
        message("getting cached data")
    }
    inverse
}
