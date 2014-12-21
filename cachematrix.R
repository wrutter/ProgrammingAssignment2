## This first function makes a cache for the original matrix values at it's inverse
## 

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) I <<- Inv
  getInv <- function() I
  list(set = set, get = get,  
       setInv = setInv,
       getInv = getInv)

}

# this second function takes the output of the first and can output the cached inverse, or can calculate the inverse and cache it for next time


cacheSolve <- function(x, ...) {
  I <- x$getInv()
  if(!is.null(I)) { #do we have the inverse? If so return it
    message("getting cached data")
    return(I)
  }
  data <- x$get() #else grab the original matrix 
  I <- solve(data, ...) #and here is where we get the invers
  x$setInv(I) #cache it for later
  I   
}
