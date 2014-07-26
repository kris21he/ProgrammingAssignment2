## Creates a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) r <<- solve
    getsolve <- function() r
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## 1. Try to get the inverse matrix from the cache made by makeCacheMatrix above. 
## 2. If the inverse has already been calculated (and the matrix has not changed), return the cache value
## 3. Otherwise, get not calculatef matrix out and culculate the inverse matrix
## 4. Set the inverse matrix into cache for further use 
## 5. Then return the inverse matrix 
cacheSolve <- function(x, ...) {
    r <- x$getsolve()
    if(!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setsolve(r)
    r
}

## How to verify ths works, type following command in line start with "## >" as an example
## > a<-matrix(1:4,2,2)
## > t<-makeCacheMatrix(a)
## > cacheSolve(t)
#        [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
## > cacheSolve(t)
#   getting cached data
#        [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
