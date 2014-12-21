# This function will create a list which contains the given value
# "x" and methods to set and retrieve:
#
# - the original value of "x",
# - a cached value related to that matrix.
# 
# Methods the list provides are set(), get(), getCached() and setCached().
#
# The current implementation of getCached() will, if no cached value is
# set, calculate the inverse of the matrix and assign the calculated
# value to the cache variable.
#
# @param x The value for which the cached value will be stored.
#
# @return The list described above.
makeCacheMatrix <- function(x = matrix()) 
{
	# Original data.
	data <- x;
	# Variable holding the cached value.
	cache <- NULL
	
	# Sets the matrix to operate on, and clears a possible cached value.
	set <- function(y = matrix()) 
	{
		data <<- y
		cache <<- NULL
	}
	
	# Returns the original value.
	get <- function() 
	{
		data
	}
	
	# Explicit calculate method.
	calculate <- function(...)
	{
		if (is.null( cache )) {
			cache <<- solve( data, ... )
		}
	}
	
	# Sets the calculated inverse.
	setCached <- function(inverse)
	{
		cache <<- inverse
	}
	
	# Gets the cached value. This method calls calculate() and provides
	# it with any parameter provided in the function call.
	getCached <- function(...)
	{
		calculate(...)
		cache
	}
	
	# Construct the list to be returned.
	list(
		set = set,
		get = get,
		setCached = setCached,
		getCached = getCached,
		calculate = calculate
	)
}


# This function will calculate the inverse of a matrix if no cached
# value has been stored already for parameter x.
#
# @param x List produced by the method makeCacheMatrix().
#
# @return The inverse of the matrix submitted to makeCacheMatrix().
cacheSolve <- function(x, ...) 
{
	# If no value is set yet, calculate it an set it.
	if (is.null( x$getCached() )) {
		x$setCached( solve( x$get(), ... ))
	}
	
	# Return the cached value.
	x$getCached()
}
