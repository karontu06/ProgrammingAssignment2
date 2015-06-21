##Karon C.L. Creating a function that works to creat an invertible
##matrix and makes the inverse of the matrix available in the cache
##environment

## The makeCacheMatrix creates a matrix and returns a list of
## functions that will set the value of the matrix and get the 
##value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	cached<-NULL
	set<-function() {
		x<<-y
		cached<<-NULL
	}
	get<-function() x
	setmatrix<-function(inverse) cached<<-inverse
	getinv<-function() cached
	list(set=set, get=get, 
		setmatrix=setmatrix,
		getinv=getinv)
}

##cacheSolve calculates the inverse of the matrix created in 
##makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cached<-x$getInv()
        if (!is.null(cached)){
        	message("getting cached data")
        	return(cached)
        }
        matrix<-x$get()
        cached<-solve(matrix,...)
        x$setmatrix(cached)
        return(cached)
}
