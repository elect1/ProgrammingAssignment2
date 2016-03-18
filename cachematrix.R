## 1.  Function makeCacheMatrix() creates functions to store/retrieve
##     a matrix and its inverse.
## 2.  Function cacheSolve() returns the inverse of a matrix by 
##     retrieving it from the cache if possible.
## Usage example:
##   xfun <- makeCacheMatrix(x)
##   cacheSolve(xfun)

## Function makeCacheMatrix (callable w/ or w/o argument) returns 
## a list of 4 functions: set, get, setinv, getinv.  Usage:
## xfun <- makeVector(x)
## xfun$get() returns the x matrix
## xfun$setinv(inverse_of_x) sets the inverse of x
## xfun$getinv() returns the inverse of x previously set with setinv
## xfun$set(x) resets the x matrix used in the makeMatrix functions

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL                                ## init/default inverse
    set <- function(y) {                        ## func setting/resetting
        x <<- y                                 ## ... the x matrix and
        minv <<- NULL                           ## ... its inverse
    }
    get <- function() x                         ## func returning x
    setinv <- function(inverse) {               ## func returning inverse
        minv <<- inverse
    }
    getinv <- function() minv                   ## func returning minv 
    ## return list of functions defined above:
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The input argument to function cacheSolve is the list resulting
## from having called the makeCacheMatrix() function.  

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()      ## attempt to get cached inverse
    if ( !is.null(minv) ) {
        message("getting cached data")
        return(minv)        ## return the cached inverse
    }
    data <- x$get()         ## retrieve matrix
    minv <- solve(data,...) ## compute matrix inverse
    x$setinv( minv )        ## cache matrix inverse for next time
    minv                    ## return the inverse
}
