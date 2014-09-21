#makeCacheMatrix function is to get inverse of a matrix and save the data in 
#an object
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	  #setsol <- function(solve) m <<- solve
	  setsol <- function(x) m <<- solve(x)
        getsol <- function() m
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}

#cacheSolve function is used to get inverse of a matrix
#it will check first if the same matrix has its inverse 
#already.  If not, it will use solve function to get it.
cacheSolve <- function(x = matrix, ...) {
	  #how to test cached scenario? So far, everytime use solve again.
        d <- makeCacheMatrix(x)
	  m <- d$getsol()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- d$get()
        m <- solve(data, ...)
        d$setsol(m)
        m
}
