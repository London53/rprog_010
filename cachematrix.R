########################################################################
##
## This script creates a special "matrix" object that can cache its inverse.
## The script contains two functions and relies on lexical scoping of R.
##
## Inverting a matrix is 'costly' in terms of CPU time. Using these
## functions can save time.  To have the functions work properly:
##
## The matrix must be invertable
## The matrix must remain constant between calls (otherwise there is no point
##     to caching
## The matrix should (generally speaking) be inverted multiple times (the
##     object/function will still work even if only required once, but
##     obviously there will be no time savings)
##
########################################################################



########################################################################
##
## This function creates a special Matrix which is a list.
##
## The list consists of 4 functions:
##     1) set .. sets the value of the matrix
##	 2) get .. returns the value of the matrix
##	 3) invert .. inverts the matrix and caches the inverted matrix
##     4) inverse .. returns the cached value of the inverted matrix
##
########################################################################

makeCacheMatrix <- function(x = matrix()) {
        m.inv <- NULL
        set <- function(y) {
                x <<- y
                m.inv <<- NULL
        }
        get <- function() x
        invert <- function(solve) m.inv <<- solve
        inverse <- function() m.inv
        list(set = set, get = get,
             invert = invert ,
             inverse = inverse )
}


########################################################################
##
## This function performs a solve(x) to find the inverse of the matrix
## x, but only if the value has not already been computed.  If the 
## inverse of the matrix x has already been cached, then the cached value
## is returned.
##
## you use cacheSolve(), just as you would solve()
## 
## x must have been 'set up' using the makeCacheMatrix
##
########################################################################
cacheSolve <- function(x, ...) {
        m.inv <- x$inverse()
        if(!is.null(m.inv)) {
                message("getting cached inverse of x")
                return(m.inv)
        }
        data <- x$get()
        m.inv <- solve(data)
        x$invert(m.inv)
        m.inv
}

########################################################################
##
## USAGE:
##
## > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## > A<-hilbert(4)
## > A
##           [,1]      [,2]      [,3]      [,4]
## [1,] 1.0000000 0.5000000 0.3333333 0.2500000
## [2,] 0.5000000 0.3333333 0.2500000 0.2000000
## [3,] 0.3333333 0.2500000 0.2000000 0.1666667
## [4,] 0.2500000 0.2000000 0.1666667 0.1428571
## > solve(A)
##      [,1]  [,2]  [,3]  [,4]
## [1,]   16  -120   240  -140
## [2,] -120  1200 -2700  1680
## [3,]  240 -2700  6480 -4200
## [4,] -140  1680 -4200  2800
## > X<-makeCacheMatrix(A)
## > X$set(A) #set the value of the special matrix X to the matrix A
## > X$get()  #check that X is now the same as A
##           [,1]      [,2]      [,3]      [,4]
## [1,] 1.0000000 0.5000000 0.3333333 0.2500000
## [2,] 0.5000000 0.3333333 0.2500000 0.2000000
## [3,] 0.3333333 0.2500000 0.2000000 0.1666667
## [4,] 0.2500000 0.2000000 0.1666667 0.1428571
## > cacheSolve(X) #'solve' the special matrix X (NB: you get the same answer
##                 #as solve(A) above
##      [,1]  [,2]  [,3]  [,4]
## [1,]   16  -120   240  -140
## [2,] -120  1200 -2700  1680
## [3,]  240 -2700  6480 -4200
## [4,] -140  1680 -4200  2800
## > # any further calls get cached value
## > cacheSolve(X)
## getting cached inverse of x
##     [,1]  [,2]  [,3]  [,4]
## [1,]   16  -120   240  -140
## [2,] -120  1200 -2700  1680
## [3,]  240 -2700  6480 -4200
## [4,] -140  1680 -4200  2800
## > cacheSolve(X)
## getting cached inverse of x
##      [,1]  [,2]  [,3]  [,4]
## [1,]   16  -120   240  -140
## [2,] -120  1200 -2700  1680
## [3,]  240 -2700  6480 -4200
## [4,] -140  1680 -4200  2800
##
########################################################################


