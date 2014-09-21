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

cacheSolve <- function(x, ...) {
        m <- x[x["getsol"]]
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x[x["get"]]
        m <- solve(data, ...)
        x[x["setsol(m)"]]
        m
}
