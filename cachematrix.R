## Cache inverse of matrix


##This function creates a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  z<-NULL
  set<-function(y){
    x<<-y
    z<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) z<<- solve
  getmatrix<-function() z
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  z<-x$getmatrix()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  matrix<-x$get()
  z<-solve(matrix, ...)
  x$setmatrix(z)
  z
}
