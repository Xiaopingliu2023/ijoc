ABM1<-function(sx,sy){
  sx1<-sx[indeindex]
  sy1<-sy[indeindex]
  m1<-colMeans(sx1, na.rm = TRUE);
  m2<-colMeans(sy1, na.rm = TRUE);
  #m2<-mean(sy);
  return (abs((m1-m2)/m2))
}


ABRF1<-function(sx,sy){
  m1<-as.data.frame(quantile(sy[,indeindex], c(0,0.5,1)))[2,1];
  m00<-nrow(sx[sx[,indeindex] < m1,]);
  m0<-nrow(sx);
  #m2<-mean(sy);
  return (abs(m00/m0-1/2))
}

AEM1<-function(sx,sy){
  sx1<-sx[deindex]
  sy1<-sy[deindex]
  m1<-colMeans(sx1, na.rm = TRUE);
  m2<-colMeans(sy1, na.rm = TRUE);
  #m2<-mean(sy);
  r1<-abs((m1-m2)/m2+1)/2;
  r2<-r1/4+0.1;
  return (r2)
}
AERF1<-function(sx,sy){
  m1<-as.data.frame(quantile(sy[,deindex], c(0,0.9,1)))[2,1];
  m00<-nrow(sx[sx[,deindex] < m1,]);
  m0<-nrow(sx);
  #m2<-mean(sy);
  return (abs(m00/m0-9/10))
}

AEC1<-function(sx,sy){
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
RMSE1<-function(sx,sy){
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

MCE1<-function(sx,sy){
  
  
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
