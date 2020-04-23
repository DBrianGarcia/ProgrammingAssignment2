#############################################################################################
# FECHA: 21/04/2020
# AUTHOR: BRIAN ARTURO GARCÍA CERRITEÑO
# OBJECTIVE: CREATES TWO FUNCTIONS THAT STORES AND SOLVER THE INVERSE OF A GIVEN MATRIX
#############################################################################################

## Function that creates an special matrix object that caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
   m<-NULL
   set<-function(y) {
     x<<-y
     m<<-NULL
   }
   get<-function() x
   setInverse<-function(matrixInverse) m<<-matrixInverse
   getInverse<-function() m
   list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## Computes the inverse of the special matrix defined with the makeCacheMatrix Function
## If the inverse has already been calculated (and the matrix has not changes), it just 
## retrieves de inverse Matrix stored

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)) {
          message("getting chached data")
          return(m)
        }
        matrix<-x$get()
        m<-solve(matrix,...)
        x$setInverse(m)
        m
}
