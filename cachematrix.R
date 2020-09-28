## Computing the inverse of a special matrix by makeCacheMatrix and retieving the inverse from the cache


## Creating a function that calculate and stores the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinvs <- function(inverse) invs <<- inverse
  getinvs <- function() invs
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)

}


## Creating a function that first calls the inverse of the matrix 
##and retrieves the inverse of a matrix from cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getinvs()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setmean(invs)
  invs
}
