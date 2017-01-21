makeCacheMatrix <- function(x = matrix()) {

##makeCacheMatrix creates a list containing a function which
##sets the value of the matrix
##gets the value of the matrix
##sets the value of inverse of the matrix
##gets the value of inverse of the matrix

g<-NULL
	set<-function(y)
		{
		x<<-y
		g<<-NULL
		}
	get<-function()x
	setinverse<-function(solve)g<<-solve
	getinverse<-function()g
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


cacheSolve <- function(x, ...) {
##cacheSolve function returns the inverse of the matrix
##it first checks if the inverse is computed, if so it gets the result
##if not,it computes the inverse and sets the value in the cache via setinverse function
	

	g<-x$getinverse()
	if(!is.null(g))	
		{
		message("getting cached data")
		return(g)
		}
	data<-x$get()
	g<-solve(data, ...)
	x$setinverse(g)
	g
}
