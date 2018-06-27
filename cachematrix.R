## makeCacheMatrix will create a matrix in the cache mamory
## and store the inverse of it


## It stores the inverse of the matrix in the cache ..along with the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-solve(x)
  }
  get<-function()x
  setinv<-function(solve)inv<-solve
  getinv<-function()inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## It calculates the inverse of the matrix if it is not previously present in the cache

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  
  if(!is.null(inv)){
    message("Getting Cached Data")
    return (inv)
  }
  mat<-x$get()
  
  inv<-solve(mat)
  x$setinv(inv)
  x$set(mat)
  inv
}
