## creates a special "matrix" object that can cache its inverse
## 

## This function returns something akin to a matrix object in the OO sense since
## it holds internally a copy of a matrix and if calculated, a
## copy of its inverse. It also supplies public methods used to 
## access either the matrix (set and get) or its inverse (setinverse,getinverse).
## It returns an object, rather than a class because the internal matrix is 
## instantiated upon first call to the function.
## In this sense makeCacheMatrix is akin to a factory method in the OO sense.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function takes as its argument our matrix "object" and 
## returns an inverse to the matrix held within. On the way
## it ensures that the inverse is cached within our matirx object
## in case furture requests for the inverse are made.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
# m<- matrix(c(1,0,0,1),nrow=2,ncol=2)
# mcm<-makeCacheMatrix(m)
# m1<-cacheSolve(mcm)
# print( m%*%m1)
# m2<-cacheSolve(mcm)
# print( m%*%m2)
