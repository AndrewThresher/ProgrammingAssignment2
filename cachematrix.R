## These functions act to store the inverted matrix so that when it is later called it can use the
## previously calculated value rather than calculating it fresh each time.

## Feed the function a matrix. Get returns the matrix you gave it. Setsolve creates the inverse of x
## and stores it as m in the parent environment. Getsolve returns the newly set m value. All arguments
## are then stored as a list.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## First m is updated with the value produced from the getsolve argument in makeCacheMatrix. If m is
## populated it returns the value rather than calculating it fresh. It not it uses the get argument
## to get the matrix to be inverted and stores it as 'data'. It then runs setsolve to invert 'data'
## and outputs the result.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setsolve(m)
      m
}
