## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function set and retrieve the original matrix and inverse of it.
## Arguments: the raw matrix
## return: a list of internal functions
makeCacheMatrix<-function(m){
    inverse<-NULL  ## set inversed matrix null initially. 
    set<-function(y)
    {
        m<<-y  ## In different environment, the assignment operator should be <<- instead of <-
        inverse<<-NULL
    }
    
    get<-function()
    {
        m
    }
    
    setInverse<-function(inv)
    {
        inverse<<-inv
    }
    
    getInverse<-function()
    {
        inverse
    }
    
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes and returns the inversed matrix, which comes from the original matrix from makeCacheMatrix object.
## If the inversed matrix has been calculated and the original matrix has not been changed, the cached result should be returned. This is accomplished by checking the inverse matrix is null and the 
## multiplication of cached inverse and original matrix is identity matrix. Otherwise, the inversed should be computed.
##Args: makeCacheMatrix object
##Return: inversed matrix
cacheSolve<-function(makeFunc)
{
    inverse<-makeFunc$getInverse()
    matrix<-makeFunc$get()
    
    if(!is.null(inverse)&&checkIdentity(matrix%*%inverse)==TRUE) ## to check if the inversed matrix is null or not and the matrix has been changed or not.
    {
        print("Getting the cached Matrix")
        return(inverse)
    }
    
    result<-solve(matrix)  ## to store the inversed matrix to the result variable
    makeFunc$setInverse(result) ## to update the inversed matrix in the makeCacheMatrix object
    result
}

##to check if a matrix is identity or not
checkIdentity<-function(matrix)
{
    cols<-ncol(matrix)
    rows<-nrow(matrix)
    
    if(cols!=rows){
        return(FALSE)
    }
    
    for(i in 1:rows)
    {
        
        for(j in 1:cols)
        {
            if(i==j) 
            {
                if(round(matrix[i,j],0)!=1)
                {
                    return(FALSE);
                }    
            }
            else
            {
                if(round(matrix[i,j],0)!=0)
                {
                    return(FALSE)
                }
            }
        }
    }
    TRUE
}