

#This function calculates the inverse of a matrix and stores it for later use in order to save time and resources afterwards
#the matrix is stored in  x, and the inverse is stored into inv.mat
#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.mat <- NULL
  set <- function(y) {
    x <<- y
    inv.mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv.mat <<- inverse
  getinverse <- function() inv.mat
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
       #x
}





## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the inverse function.


cacheSolve <- function(x, ...) {
  inv.mat <- x$getinverse()
  if(!is.null(inv.mat)) {
    message("getting cached data")
    return(inv.mat)
  }
  data <- x$get()
  inv.mat <- solve(data, ...)
  x$setinverse(inv.mat)
  inv.mat
}  
#x<-rbind(c(1,4), c(7,-12))
#a<-makeCacheMatrix(x)
#x
#cacheSolve(a)



