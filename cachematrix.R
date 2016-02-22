## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(m){
    inverse<-NULL
    set<-function(y)
    {
        m<<-y
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

cacheSolve<-function(makeFunc)
{
    inverse<-makeFunc$getInverse()
    matrix<-makeFunc$get()
    
    if(!is.null(inverse)&&checkIdentity(matrix%*%inverse)==TRUE)
    {
        print("Getting the cached Matrix")
        return(inverse)
    }
    
    result<-solve(matrix)
    makeFunc$setInverse(result)    
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