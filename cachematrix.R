## cachematrix.R
## This module implements matrix inversion with caching.

## makeCacheMatrix(x) creates a cached matrix object from a standard matrix x.
##
## Arguments:
## x        A square, nonsingular matrix.
##
## Details:
## The supplied matrix x should be invertible, although this is not validated. 
## If x is not supplied, it defaults to an empty matrix.
## The returned object is a special "cached matrix" object which wraps 
## the supplied matrix x, a cache for storing the inverse of x, and four access 
## methods.
## The matrix can be retrieved with the get() method and updated with
## the set() method. Setting the matrix has the secondary effect of clearing 
## (setting to NULL) the cache.
## The cache is initialized to NULL and can be accessed via the setInverse() 
## and getInverse() methods.

makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() { 
                return(x) 
        }
        setInverse <- function(i) {
                mInverse <<- i
        }
        getInverse <- function() {
                return(mInverse)
        }
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve(x) returns the inverse of the matrix stored in x.
##
## Arguments:
## x        A "cached matrix" object created with the makeCacheMatrix() 
##          function.
## ...      Not used in the current implementation.
##
## Details:
## Returns a standard matrix object which corresponds to the inverse of the 
## matrix stored in x.
## This function first tries to retrieve the inverse from x's cache. If the 
## cache is empty (equal to NULL), the inverse matrix is computed, stored in 
## x's cache, and returned. Otherwise, the cached valued is returned.
## The matrix is assumed to be invertible and no checks are performed before 
## invoking R's solver.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (is.null(i)) {
                i <- solve(x$get())
                x$setInverse(i)
        }
        return(i)
}
