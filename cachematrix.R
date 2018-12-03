#this function is used to create specicial object that stores 
#the matrix and caches its inverse

makeCachesMatrix<-function(x = matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set , get=get , getinverse=getinverse , setinverse=setinverse)
}

#This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
   i<-x$getinverse()
   if(!is.null(i)){
     message("getting cached data")
     return(i)
   }
   data<-x$get()
   i<-solve(data,...)
   x$setinverse(i)
   i
}
m<-matrix(c(1,2,3,4),2,2)
m1<-makeCachesMatrix(m)
cacheSolve(m1)