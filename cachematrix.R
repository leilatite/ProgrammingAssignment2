### Pair of functions that cache and compute the 
## inverse of a matrix.

## Function makeCacheMatrix creates a special matrix 'mtx' object
## and defines functions that can cache its inverse.

## input 'mtx' is a matrix. We get to assume that the input is always invertible.

## returns a list of functions that can perform operations on the cached inverse of the matrix

makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
        
}

## Function cacheSolve computes the inverse of the special
## matrix 'mtx' returned by function 'makeCacheMatrix' defined above. 
## If the inverse has already been calculated
## and the matrix has not changed, then
## 'cacheSolve' retrieves the inverse from the cache.

## mtx is a matrix, followed by optional additional parameters

## returns the inverse of the matrix 

cacheSolve <- function(mtx, ...) {
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                return(inverse)
        }
        data <- mtx$get()
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}}
