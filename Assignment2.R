# makeCacheMatrix does the following:
# sets the matrix
# gets the matrix
# sets the inverse value
# gets the inverse value

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
           x <<- y
           i <<- NULL
       }
     get <- function() {x}
     setInv <- function(inverse) i <<- inverse
     getInve <- function() {i}
     list(set = set, get = get,setInv = setInv,getInv = getInv)
  }

# cacheSolve does the following
# checks to see if inverse value is already cached
# if the inverse value is cached, it returns that value
# otherwise, it calculates and returns the inverse value

cacheSolve <- function(x, ...) {
     i <- x$getInv()
     if(!is.null(i)) {
           message("getting cached data")
           return(i)
       }
     data <- x$get()
     i <- solve(data, ...)
     x$setInv(i)
     i
 }
