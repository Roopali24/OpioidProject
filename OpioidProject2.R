## Calculating the no. of drug users that were assigned intervention
rm(list=ls())
#setwd("~/Desktop")
library(openxlsx)

## we assigned intervention according to best, worst and random case. 
## Each time the assignment is different.
## We need to sort based on the assignment (increasing order)
dat=read.xlsx("this.xlsx") # read the base probabilities and counterfactual probabilities

A=as.matrix(dat[,13])
na=length(A)
B=as.matrix(dat[,3:12])

cap=5 #capacity per intervention. We need to change it accordingly

#counting
a=table(A)
a
## no. of people assigned a particular intervention (1-10) 1 meaning no intervention
n1=342
n2=12
n3=8
n4=11
n5=6
n6=5
n7=14
n8=8
n9=9
n10=5
nn=c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10)
nnn=c(n1,n1+n2,n1+n2+n3,n1+n2+n3+n4, n1+n2+n3+n4+n5,n1+n2+n3+n4+n5+n6,
      n1+n2+n3+n4+n5+n6+n7,n1+n2+n3+n4+n5+n6+n7+n8,n1+n2+n3+n4+n5+n6+n7+n8+n9)

#random selection of people if they exceed the capacity
s=30
sim=matrix(0,na,s)
sim[1:n1,]=matrix(1,n1,s) 


for (i in 1:30){
  for (j in 1:9){  
    if (nn[j+1]>cap) {
      rr=replicate(1, rnorm(nn[j+1])) 
      srr=sort(rr)
      cutoff=srr[cap]
      rr=rr<=cutoff
      m=rr*(j+1)+(1-rr)
      sim[(nnn[j]+1):(nnn[j]+nn[j+1]),i]=m
    }
    
    if (nn[j+1]>0 & nn[j+1]<=cap){
      
      sim[(nnn[j]+1):(nnn[j]+nn[j+1]),i]=matrix(j+1,nn[j+1],1)
      
    }
    
  }
  
}

write.csv(sim,"Results.csv")

# before reading the file below, arrange them in decreasing order of participant id
dat=read.csv("Results.csv", header=TRUE)
response=read.csv("NewData_Test.csv", header=TRUE) ## reading the testdata

dat=dat[,-1]
response=response[,214]

response1=cbind(response,response,response,response,response,response,response,response,
                response,response,response,response,response,response,response,response,response,
                response,response,response,response,response,response,response,response,response,
                response,response,response,response)

sub=response1-dat

non_zeros=vector(mode = "numeric", length = 30)
sub_i=vector(mode = "numeric", length = 420)

for (i in 1:30) {
  sub_i=sub[,i]
  non_zeros[i]=nnzero(sub_i)
}

x=rep(420,times=30)-non_zeros
y=rep(91,times=30)-x # no. of drug users that were assigned intervention
mean(y) #avg over 30 times.
