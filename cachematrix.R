#makeCacheMatrix function is to get inverse of a matrix and save the data in 
#an object.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	  setsol <- function(solve) m <<- solve
        getsol <- function() m
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}

#cacheSolve function is used to get inverse of a matrix
#it will check first if the same matrix has its inverse 
#already.  If not, it will use solve function to get it.
cacheSolve <- function(x = matrix, ...) {
	  #keep getting error $operator is invalid for atomic vector
	  #but use x[x[...]] and getting NA instead, so function
	  #that gets matrix is different?
        m <- x$getsol()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsol(m)
        m
}
