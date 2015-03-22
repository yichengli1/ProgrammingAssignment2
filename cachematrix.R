
## This function returns four methods: getVal, setVal, getInv, setInv, 

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  setVal <- function(inVal) {
    x <<- inVal
    Inv <<- NULL
  }
  getVal <- function() x
  setInv <- function(inverse) Inv <<- inverse
  getInv <- function() Inv
  list(setVal=setVal, getVal=getVal, setInv=setInv, getInv=getInv)
}

## This function will retunr the inverse of a square and inversable matrix
## It uses "solve" function internally to do the work

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("return cached value.")
    return(inv)
  }
  dt <- x$getVal()
  inv <- solve(dt)
  x$setInv(inv)
  inv
}
