permute_test<-function(df, c1, c2, val1, val2)
{
  D<- mean(df[df[,c1]==val1,c2]) - mean(df[df[,c1]==val2,c2])
  null_tunnel <- rep(val1,nrow(df))
  null_tunnel[sample(nrow(df),floor(nrow(df)/2))] <- val2
  null <- data.frame(null_tunnel,df[,c2])
  names(null) <- c("Attribute1","Attribute2")
  pop1 <- null[null[,"Attribute1"] == val1,2]
  pop2 <- null[null[,"Attribute1"] == val2,2]
  D_null <- mean(pop1) - mean(pop2)
  D_null
  return(D_null)
}
