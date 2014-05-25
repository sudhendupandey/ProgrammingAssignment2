##This function creates a special "matrix" object that can cache its inverse

##Create a function that takes a matrix as an argument
makeCacheMatrix <- function(x = matrix()) { 
## set m as NULL to clear any previous values stored for the matrix inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
## apply the solve() function to calculate the inverse of the input matrix
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
## create a list of functions available within the makeCacheMatrix function for using in cacheSolve function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
## get matrix "m" from the cache as calculated by the "makeCacheMatrix" function above
        m <- x$getinverse()
## if m is not null, return the value retrieved from the cache        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## If "m" is found to be null in the previous step, get the original matrix
        newdata <- x$get()

## calculate the inverse of matrix obtained in the previous step with the solve() funtion
        m <- solve(data, ...)
        x$setinverse(m)
## return the calculated inverse as output
        m
}
