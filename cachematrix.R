#Put your matrix as an argument into this function to make a Cache Matrix.
makeCacheMatrix <- function (x = matrix ()) { 
    
    i <- NULL 
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x #prints matrix x
    setinverse <- function (inverse) i <<- inverse
    getinverse <- function () i #prints inverse of matrix x (if available)
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
    #final output of function is a list of functions
}

#Put your Cache Matrix through this function to solve for its inverse
cacheSolve <- function (x, ...) {
    
    i <- x$getinverse() #if inverse has been solved before, assign that value to i
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get() #if not use the inbuilt solve function on the cache matrix
    i <- solve(data,...) 
    x$setinverse(i) #assigns the value of that inverse to i
    i #prints i as final output which is the inverse of the Cache Matrix
    
}

#test the code to see if it works!

x <- rbind(c(1, -1/4), c(-1/4, 1)) #create invertible matrix
cx <- makeCacheMatrix(x) #create cache matrix

cacheSolve(cx) #do it once to see if function can solve for inverse
cacheSolve(cx) #do it again to see that now inverse is stored so we see "getting cached data"