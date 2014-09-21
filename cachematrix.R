makeCacheMatrix <- function (x = matrix ()) {
    
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function (inverse) i <<- inverse
    getinverse <- function () i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

cacheSolve <- function (x, ...) {
    
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
    
}

x <- rbind(c(1, -1/4), c(-1/4, 1)) #create invertible matrix
cx <- makeCacheMatrix(x) #create cache matrix

cacheSolve(cx) #do it once to see if function can solve for inverse
cacheSolve(cx) #do it again to see that now inverse is stored so we see "getting cached data"