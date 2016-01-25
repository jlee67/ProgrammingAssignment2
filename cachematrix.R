## Put comments here that give an overall description of what your
## functions do

## create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        m <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function () x
        
        ##set the value of the inverse
        setinverse <- function(inverse) m <<- inverse
        
        ##get the value of the inverse
        getinverse <- function () m
        list(set = set, get = get, 
                steinverse = setinverse, 
                getinvers = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
        data <-x$get()
        
        ## compute matrix inverse 
        m <- solve(data) %*% data
        x$setinverse(m)
        m
}