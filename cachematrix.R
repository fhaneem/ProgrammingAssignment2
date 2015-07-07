## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting inverse cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

## Sample run:
## > x = rbind(c(3,9), c(9,3))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  3 		9
## [2,]  9  	3

## No cache in the first run
## > cacheSolve(m)
            [,1]        [,2]
##	[1,] -0.04166667  0.12500000
##	[2,]  0.12500000 -0.04166667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
            [,1]        [,2]
##	[1,] -0.04166667  0.12500000
##	[2,]  0.12500000 -0.04166667
