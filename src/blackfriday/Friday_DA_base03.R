indeindex<-1
deindex<-5
mysplit1<-mysplit
ABM_table<-data.frame(b100=0,b200=0)
pr<-rep(1,m)
pr<-pr/sum(pr)
n0<-n/10
n1<-n0*pr
#length(mysplit1)
for(i in 1:m){
  sa<-nrow(mysplit[i][[1]]);
  sa_i<-sample(1:sa,n1[i],replace=TRUE);
  mysplit1[i][[1]]<-mysplit[i][[1]][sa_i,]
}
sx<-as.data.frame(mysplit1[i][[1]]);
sy<-as.data.frame(mysplit[i][[1]]);

ABM<-function(sx,sy){
  sx1<-sx[indeindex]
  sy1<-sy[indeindex]
  m1<-colMeans(sx1, na.rm = TRUE);
  m2<-colMeans(sy1, na.rm = TRUE);
  #m2<-mean(sy);
  return (abs((m1-m2)/m2))
}

#sy<-(my_data[,1]);
ABM(sx,sy)
ABM_table[1,1]<-ABM(sx,sy)

ABRF<-function(sx,sy){
  m1<-as.data.frame(quantile(sy[,indeindex], c(0,0.5,1)))[2,1];
  m00<-nrow(sx[sx[,indeindex] < m1,]);
  m0<-nrow(sx);
  #m2<-mean(sy);
  return (abs(m00/m0-1/2))
}

ABRF(sx,sy)


AEM<-function(sx,sy){
  sx1<-sx[deindex]
  sy1<-sy[deindex]
  m1<-colMeans(sx1, na.rm = TRUE);
  m2<-colMeans(sy1, na.rm = TRUE);
  #m2<-mean(sy);
  r1<-abs((m1-m2)/m2+1)/2;
  r2<-r1/4+0.1;
  return (r2)
}
AERF<-function(sx,sy){
  m1<-as.data.frame(quantile(sy[,deindex], c(0,0.9,1)))[2,1];
  m00<-nrow(sx[sx[,deindex] < m1,]);
  m0<-nrow(sx);
  #m2<-mean(sy);
  return (abs(m00/m0-9/10))
}
AEM(sx,sy)
AERF(sx,sy)

AEC<-function(sx,sy){
  min1<-min(sx[,c(deindex)]);
  max1<-max(sx[,c(deindex)]);
  
  if(sum(sx[,c(deindex)])==0 || (max1-min1)==0)
  {
    r0<-0
  }else{
    c1<-cor(sx[,c(indeindex,deindex)]);
    r0<-c1[1,2];
  
  }
  c2<-cor(sy[,c(indeindex,deindex)])
  r1<-r0-c2[1,2]
  r2<-c2[1,2]
  #return(abs(r1/r2))
  return(abs(r1)+0.1)
}
AEC(sx,sy)
RMSE_o<-function(sx,sy){
  sx1<-sx[deindex]
  sy1<-sy[deindex]
  m1<-colMeans(sx1, na.rm = TRUE);
  m2<-colMeans(sy1, na.rm = TRUE);
  #m2<-mean(sy);
  r1<-abs((m1-m2))/50+1.1;
  #r10<-r1/4+0.5;
  #r2<-r10/2+1
  return (r1)
}
RMSE<-function(sx,sy){
  sx1<-sx[deindex]
  sy1<-sy[deindex]
  m1<-colMeans(sx1, na.rm = TRUE);
  m2<-colMeans(sy1, na.rm = TRUE);
  #m2<-mean(sy);
  #r1<-abs((m1-m2))/50+1.1;
  r1<-abs((m1-m2));
  #r10<-r1/4+0.5;
  #r2<-r10/2+1
  return (r1)
}
RMSE(sx,sy)
totalMCE<-0
MCE0<-function(sx,sy){


  breaks <- c(0,Inf)
#  sx$dep <- as.factor(findInterval(sx[,deindex], breaks,left.open = TRUE))
#  sy$dep <-  as.factor(findInterval(sy[,deindex], breaks,left.open = TRUE))
  sx$dep <- (findInterval(sx[,deindex], breaks,left.open = TRUE))
  sy$dep <- (findInterval(sy[,deindex], breaks,left.open = TRUE))
  

  #fitTree<-rpart(dep ~age ,sx,control=rpart.control(minsplit=2,cp=0.0000001))  
  xfitTree<-rpart(dep ~age ,sx) 
  yfitTree<-rpart(dep ~age ,sy) 
  
#  xpred <- predict(xfitTree, sy, type="class")  
#  ypred <- predict(yfitTree, sy, type="class")  

  xpred <- predict(xfitTree, sy)  
  ypred <- predict(yfitTree, sy)  
  rt<-sum(abs(xpred-sy$dep)/nrow(sy))
  totalMCE<-sum(abs(ypred-sy$dep)/nrow(sy))  
  
  
 # xfitTree<-rpart(cdall ~age ,sx) 
  #yfitTree<-rpart(cdall ~age ,sy) 
  
  #xpred <- predict(xfitTree, sy)  
  #ypred <- predict(yfitTree, sy)    
    
  #sum(abs(xpred-sy$cdall)/nrow(sy))
  #sum(abs(ypred-sy$cdall)/nrow(sy))
  # Write result to result data.frame
  #xtab <- table(xpred, sy$dep)  
  return(rt/2)
  #ytab <- table(ypred, sy$dep)
  #(ytab[1,2] + ytab[2,1])/nrow(sy)
  
  
  
  #df$start <- breaks[df$interval]
  #df$end <- breaks[df$interval + 1]  
  
 
}

MCE<-function(sx,sy){
  
  
  breaks <- c(0,Inf)
 
  sx[deindex+1] <- (findInterval(sx[,deindex], breaks,left.open = TRUE))
  sy[deindex+1]<- (findInterval(sy[,deindex], breaks,left.open = TRUE))
  sx1<-sx[deindex+1]
  sy1<-sy[deindex+1]
  
  #sx1<-sx$dep
  #sy1<-sy$dep
  m1<-colMeans(sx1, na.rm = TRUE);
  m2<-colMeans(sy1, na.rm = TRUE);

  r1<-abs((m1-m2));
 
  return (r1)

  
}


m1 <- matrix(1:25, ncol=5, byrow=TRUE)
d1 <- as.data.frame(m1, stringsAsFactors=FALSE)
rsort<-c()
k<-1
for(sum in 2:10){
  for (i in 1:5){
    for (j in 1:5){
      if(i+j-sum==0){
        #print(d1[i,j]);
        rsort[k]<-d1[i,j];
        k<-k+1
      }
    }
  }
}

m2 <- as.data.frame(matrix(rsort, ncol=5, byrow=TRUE))

m3 <- apply(m2, 2, sort, decreasing=T)
m4 <- apply(m3, 1, sort, decreasing=T)





#dd<-apply(TableW, 2, sort, decreasing=T)
i<-1

my_out<-function(dd,fn="output.xlsx"){
dd<-apply(dd, 2, sort, decreasing=T)
  
lt<-data.frame()
for(l in 1:7){
m1 <- matrix(dd[,l], ncol=5, byrow=TRUE)
d1 <- as.data.frame(m1, stringsAsFactors=FALSE)
rs<-c()
k<-1
  for(sum in 2:14){
    for (i in 1:7){
      for (j in 1:5){
        if(i+j-sum==0){
          #print(d1[i,j]);
          rs[k]<-d1[i,j];
          k<-k+1
        }
      }
    }
  }
  
m2 <- as.data.frame(matrix(rs, ncol=5, byrow=TRUE))
m3 <- apply(m2, 2, sort, decreasing=T)
m4 <- t(apply(m3, 1, sort, decreasing=T))
#lt <- c(lt, as.data.frame(m4))
lt[((l-1)*8+1):((l-1)*8+1+6),1:5]<-(m4)
#lt[((l-1)*6+1+4+1),]<-c(" "," "," "," "," ")
}

write_xlsx(lt, fn)
}
show_out<-function(dd,fn="output.xlsx"){
  dd<-apply(dd, 2, sort, decreasing=T)
  
  lt<-data.frame()
  for(l in 1:7){
    m1 <- matrix(dd[,l], ncol=5, byrow=TRUE)
    d1 <- as.data.frame(m1, stringsAsFactors=FALSE)
    rs<-c()
    k<-1
    for(sum in 2:14){
      for (i in 1:7){
        for (j in 1:5){
          if(i+j-sum==0){
            #print(d1[i,j]);
            rs[k]<-d1[i,j];
            k<-k+1
          }
        }
      }
    }
    
    m2 <- as.data.frame(matrix(rs, ncol=5, byrow=TRUE))
    m3 <- apply(m2, 2, sort, decreasing=T)
    m4 <- t(apply(m3, 1, sort, decreasing=T))
    #lt <- c(lt, as.data.frame(m4))
    lt[((l-1)*8+1):((l-1)*8+1+6),1:5]<-m4
    #lt[((l-1)*6+1+4+1),]<-c(" "," "," "," "," ")
  }
  lt;
  return(lt);
}

#my_out(TableW,"output1.xlsx")





if(0==1){
sy<-my_data
breaks <- c(0,Inf)

sy$dep <- (findInterval(sy[,deindex], breaks,left.open = TRUE))
yfitTree<-rpart(dep ~age ,sy,control=rpart.control(minsplit=2,cp=0.0000001)) 
ypred <- predict(yfitTree, sy)

breaks <- c(0.5,Inf)

ypred <- (findInterval(ypred, breaks,left.open = TRUE))

totalMCE<-sum(abs(ypred-sy$dep)/nrow(sy))  
totalMCE/2
}














