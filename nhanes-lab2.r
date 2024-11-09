library(copent)
library(lattice)
library(copBasic)

csv2 = read.csv("~/Rworks/nhanes/lab2.csv")
n = dim(csv2)[2]
len = 150

mxcor1 = mxcor2 = mxcor3 = mxsigma1 = mxgamma1 = mxce1 = matrix(0,n,n)
for(i in 1:n){
  for(j in i:n){
    v12 = na.omit(csv2[,c(i,j)])
    if(dim(v12)[1] < 20){
      next
    }else if(dim(v12)[1]>len){
      v12 = v12[1:len,]
    }else{
      len = dim(v12)[1]
    }
    mxcor1[i,j] = mxcor1[j,i] = cor(v12)[1,2]
    mxcor2[i,j] = mxcor2[j,i] = cor(v12,method = "spearman")[1,2]
    mxcor3[i,j] = mxcor3[j,i] = cor(v12,method = "kendall")[1,2]
    
    cop1 = construct_empirical_copula(v12)
    mxsigma1[i,j] = mxsigma1[j,i] = wolfCOP(para = cop1, as.sample = TRUE)
    mxgamma1[i,j] = mxgamma1[j,i] = giniCOP(para = cop1, as.sample = TRUE)
    
    v12[,1] = v12[,1] + max(abs(v12[,1])) * 0.00005 * runif(len)
    v12[,2] = v12[,2] + max(abs(v12[,2])) * 0.00005 * runif(len)
    mxce1[i,j] = mxce1[j,i] = copent(v12)
    
    str1 = paste("(",i,",",j,")")
    print(str1)
  }
}

x11(); levelplot(mxce1,xlab = '', ylab = '')
x11(); levelplot(mxcor1, xlab = '', ylab = '')
x11(); levelplot(mxcor2, xlab = '', ylab = '')
x11(); levelplot(mxcor3, xlab = '', ylab = '')
x11(); levelplot(mxsigma1, xlab = '', ylab = '')
x11(); levelplot(mxgamma1, xlab = '', ylab = '')
