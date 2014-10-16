## By using these two functions you can solve for the inverse of a square
## and invertible matrix and then cache the solution which will speed up
## performance when you would otherwise have to solve for the inverse
## multiple times

## for example:
## test <- makeCacheMatrix()
## test$set(matrix(1:4,2,2))
## cacheSolve(test)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## makeCacheMatrix creates a matrix which is really a list containing functions
## to
## 1.  set the matrix
## 2.  get the matrix
## 3.  solve and cache the value of the inverse of the matrix
## 4.  get the cached value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseX <- NULL #on initialization set the cached inverse to NULL
    set <- function(y) { #set the matrix
        x <<- y
        inverseX <<- NULL
    }
    get <- function() x # get the matrix

    setInverse <- function(solve) inverseX <<- solve #solve for the inverse of
        # 'x' and cache it
    getInverse <- function() inverseX #function to return the cached inverse

    list( #expose internal functions
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## cacheSolve checks to see if the inverse of the matrix 'x' is available in the
## cache and if it is, return it.  Otherwise solve for the inverse of 'x' and
## save it in the cache and return the solution

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse() #get the cached inverse of 'x' which could be null
    if(!is.null(inverse)) { #if null, then the inverse has not been cached
        message("getting cached data")
        return(inverse) #return the cached solution
    }
    data <- x$get() #the inverse has not been cached so solve(x) and cache it
    inverse <- solve(data, ...) #solve for the inverse
    x$setInverse(inverse) #set the cache to the inverse 
    inverse #return the inverse
}
