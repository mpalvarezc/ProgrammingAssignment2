##The function calculates and stores the inverse of a matrix


## Return a list that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Return the the inverse of a matrix by calculating it with solve or getting it with getinv (if it already exists)

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    matx <- x$get()
    i <- solve(matx, ...)
    x$setinv(i)
    i
}

