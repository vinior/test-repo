#This function creates a random vector of TRUE and FALSE in a fixed proportion.

booleanVector<-function(size=1, proportion=1/2, along.with=NULL){
  proportion<-as.numeric(proportion)

  if(proportion>1|proportion<0)
    stop("\'proportion\' must be a numeric value between 0 and 1")
  
  if(!is.null(along.with))
    size=length(along.with)
  
  Ntrue<-round(size*proportion)
  chosen<-sample(size, Ntrue)
  v<-logical(size)
  v[chosen]=TRUE
  v[-chosen]=FALSE
  v
}