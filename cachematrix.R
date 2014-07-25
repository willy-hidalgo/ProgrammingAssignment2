## Assignment: Caching the Inverse of a Matrix 
## from Coursera R-programming

## If the Inverse is not cached, calculate it

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## Let some matrix, calculate this inverse

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                ## When inverse is in cache
                message("getting cached data")
                return(m)
        }
        ## Otherwise, calculate the inserve an set it in cache
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m        
}
