
# 
library(nloptr)
library(dplyr)
library(rpart)
library(openxlsx2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(71,"Mersenne-Twister",sample.kind="Rejection")


if(1==1){
data <- read.table("black-friday.csv",header=TRUE, quote="", sep=",", na.strings="?")
d<-data
d[is.na(d)] <- 0
summary(d)
data[is.na(data)]<-0
summary(data[,6])
a <- table(data[,6])
data$City_Category[data$City_Category == "C"] <- "A"
}

#my_data<-data[,c(7, 8, 11,12, 6)]
my_data<-data[,c(9, 3, 6,8, 11)]

#df1<-group_by(my_data,white,hsgrad,lesscol)
df1<-group_by(my_data,Gender,Marital_Status,City_Category)


mysplit<-group_split(df1)

myt<-mysplit[6][[1]]
#myt<-group_split(df1)[21]

# x=c()
# tmp=c(1,2)
# n=6
# for (i in seq(1, by=2, length=n)) x[i:(i+1)] =tmp;

x<-c()
n<-6
for (i in 1:6) x[i] =2;

#myt[myt$cdall>0,]
#summary(myt[,1])

my_var<- data.frame(p=0,v=0,d =0)
m<-length(mysplit)
totalrow<-nrow(df1)
m<-length(mysplit)

n<-200
B<-1000
for(i in 1:m){
  my_var[i,1]<-nrow(mysplit[i][[1]])/totalrow
  my_var[i,2]<-var(mysplit[i][[1]][,1])
  my_var[i,3]<-1
  
}


x0<-c(n-(m-1),1,1,1,1,1)
x0f <- function (){
  x0r<-c()
  for (i in 1:m) x0r[i]<-1;
  x0r[1]<-n-(m-1);
  return(x0r)
}

eval_fx <- function(x)
{
  r<-0
  for (i in 1:m){
    pi<-my_var[i,1]
    npi<-n*pi
    r<-r+(x[i]-npi)^2/npi
    
  }
  return (r)
}

eval_fw <- function(x)
{
  r<-0
  for (i in 1:m){
    wi<-my_var[i,2]
    r<-r+wi/x[i]
    
  }
  return (r)
}
#Inequality constraints can be written as
eval_g_ineq <- function (x) {

  d<-0
  for (i in 1:m){
    di<-my_var[i,3]
    d<-d+di*x[i]^2/2
    
  }  
  return (d-B)
}
eval_g_eq <- function (x) {
  
  sn<-0
  for (i in 1:m){
    sn<-sn+x[i]
    
  }  
  return (sn-n)
}
#Lower and upper bounds are defined as
lb<-c()
for (i in 1:m) lb[i]<-1;
ub<-c()
for (i in 1:m) ub[i]<-n;

lbf <- function (){
  lb<-c()
  for (i in 1:m) lb[i]<-1;
  return(lb)
}
ubf <- function (){
  ub<-c()
  for (i in 1:m) ub[i]<-n;
  return(ub)
}


#Initial values are

#Finally, define options for nloptr
opts <- list( "algorithm" = "NLOPT_GN_ISRES",
              "xtol_rel" = 1.0e-8,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 1 ))

opts <- list( "algorithm" = "NLOPT_LN_AUGLAG",
              "xtol_rel" = 1.0e-8,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 1 ))


local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 0.01,"ranseed"=5 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 0.01,
              "maxeval"= 200000,
              "local_opts" = local_opts,
              "print_level" = 0,
              "ranseed"=9)
#Optimize
#set.seed(71,"Mersenne-Twister",sample.kind="Rejection")
res_x <- nloptr(
  x0 = x0f(),
  eval_f = eval_fx,
  lb = lbf(),
  ub = ubf(),
  eval_g_ineq = eval_g_ineq,
  eval_g_eq = eval_g_eq,
  opts = opts )





res_w <- nloptr(
  x0 = x0f(),
  eval_f = eval_fw,
  lb = lbf(),
  ub = ubf(),
  eval_g_ineq = eval_g_ineq,
  eval_g_eq = eval_g_eq,
  opts = opts )


