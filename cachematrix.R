## makeCache creates a stored value of your desired value to calculate, 
## if already found by cacheSolve, it will use the already cached value
## if not it will calculate and store the value

## This function has a get and set for x and for i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmean <- function(inverse) i <<- inverse
  getmean <- function() i
  list(set=set, get=get,
       getmean=getmean, setmean=setmean)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getmean()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
  data <- x$get()
  i <- solve(data)
  x$setmean(i)
  i 
  ## Return a matrix that is the inverse of 'x'
}
