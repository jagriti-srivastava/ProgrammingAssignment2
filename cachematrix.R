## The two functions are used in sequence to find the inverse of a matrix x. 
## If the inverse has already been found, it is returned without recalculating.
## This function creates a special object that 
## 1. Initializes a variable 'm' in which inverse is stored
## 2. A function get() to get matrix whose inverse will be found out.
## 3. setImatrix() to assign inverse to m;
## 4. getImatrix() to obtain the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
	  ## Return a list of functions as an R object
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}


## First checking is done that whether inverse matrix has been found. If yes, 
## the result is returned there itself.
## If not, the inverse of x is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return the inverse of 'x'
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
