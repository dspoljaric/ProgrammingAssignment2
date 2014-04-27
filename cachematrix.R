## Here we give two functions which can be useful if you need to compute
## the inverse of the same matrix many times. These two functions create
## special matrix and the compute inverse only once and store the inverse
## in a cache for future reference


## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the created matrix
## 4.  get the inverse of the created matrix

makeCacheMatrix <- function(x = matrix()) {

	## initialization of matrix inverse at the beginning
	matrix_inverse <- NULL

	## this function sets the data into matrix
	set <- function(y){
		x <<- y
		matrix_inverse <<- NULL
	}

	## this function retrives stored matrix
	get <- function(){
		return(x)
	}

	## this function sets the computed inverse
	setinverse <- function(inverse) {
		matrix_inverse <<- inverse
	}

	## this function retrives stored matrix inverse
	getinverse <- function() {
		return(matrix_inverse)
	}

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the function `makeCacheMatrix`. It first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the given matrix and writes the inverse matrix in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	##getting the inverse if it exists; if not it will assign NULL
	m_inverse <- x$getinverse()

	## if something is cached, return that with extra message
	if(!is.null(m_inverse)){
		message("getting cashed matrix inverse")
		return(m_inverse)
	}


	## if nothing were cached, get the original matrix, compute the inverse
	## and write it into cache invoking `setinverse` function, then return
	## computed inverse
	original_matrix <- x$get()
	m_inverse <- solve(original_matrix)
	x$setinverse(m_inverse)
	
	return(m_inverse)
}
