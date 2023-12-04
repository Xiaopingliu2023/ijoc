
set.seed(92,"Mersenne-Twister",sample.kind="Rejection")
#set.seed(900,"Mersenne-Twister",sample.kind="Rejection")

m1 <- matrix(1:35, ncol=5, byrow=TRUE)
d1 <- as.data.frame(m1, stringsAsFactors=FALSE)
for(sum in 2:14){for (i in 1:7){for (j in 1:5){if(i+j-sum==0){print(d1[i,j]);}}}}
tableB<-data.frame(ABM=-1,ABRF=-1,AEM=-1,AERF=-1,AEC=-1,RMSE=-1,MCE=-1);

n<-400;
j<-2
for(j in 1:35){
  pr<-rep(1,m);
  pr[1]<-pr[1]+j
  pr<-pr/sum(pr);n0<-n/10;n1<-n0*pr+1;
  sxt<-data.frame();
  syt<-my_data;
  for(i in 1:m){sa<-nrow(mysplit[i][[1]]);  sa_i<-sample(1:sa,n1[i],replace=TRUE);  mysplit1[i][[1]]<-mysplit[i][[1]][sa_i,];
  sxt<-rbind(sxt,as.data.frame(mysplit1[i][[1]]))
  }
  tableT<-data.frame(ABM=-1,ABRF=-1,AEM=-1,AERF=-1,AEC=-1,RMSE=-1,MCE=-1);
  for(i in 1:m){
    sx<-as.data.frame(mysplit1[i][[1]]);
    sy<-as.data.frame(mysplit[i][[1]]);

    tableT[i,1]<-ABM(sx,sy);
    tableT[i,2]<-ABRF(sx,sy);
    tableT[i,3]<-AEM(sxt,syt);
    tableT[i,4]<-AERF(sx,sy);
    tableT[i,5]<-AEC(sxt,syt);
    tableT[i,6]<-RMSE(sx,sy);
    tableT[i,7]<-MCE(sx,sy);
  }
  #print(tableT);
  tableB[j,]<-apply(tableT, 2, mean)
}
if(1==0){
B<-1000
for(j in 1:7){
  B<-B+j*100;
  res_x <- nloptr(x0=x0f(),eval_f=eval_fx,lb = lbf(),ub=ubf(),eval_g_ineq=eval_g_ineq,eval_g_eq=eval_g_eq,opts = opts )
  print(res_x)
  pr<-res_x$solution;
  pr<-pr/sum(pr);n0<-n/10;n1<-n0*pr+1;
  sxt<-data.frame();
  syt<-my_data;
  for(i in 1:m){sa<-nrow(mysplit[i][[1]]);  sa_i<-sample(1:sa,n1[i],replace=TRUE);  mysplit1[i][[1]]<-mysplit[i][[1]][sa_i,];
  sxt<-rbind(sxt,as.data.frame(mysplit1[i][[1]]))
  }
  tableT<-data.frame(ABM=-1,ABRF=-1,AEM=-1,AERF=-1,AEC=-1,RMSE=-1,MCE=-1);
  for(i in 1:m){
    sx<-as.data.frame(mysplit1[i][[1]]);
    sy<-as.data.frame(mysplit[i][[1]]);
    
    tableT[i,1]<-ABM(sx,sy);
    tableT[i,2]<-ABRF(sx,sy);
    tableT[i,3]<-AEM(sxt,syt);
    tableT[i,4]<-AERF(sx,sy);
    tableT[i,5]<-AEC(sxt,syt);
    tableT[i,6]<-RMSE(sx,sy);
    tableT[i,7]<-MCE(sx,sy);
  }
  
  tableB[j+21,]<- tableB[j+21,]+apply(tableT, 2, mean);
}
B<-1000
for(j in 1:7){
  B<-B+j*100;
  res_x <- nloptr(x0=x0f(),eval_f=eval_fw,lb = lbf(),ub=ubf(),eval_g_ineq=eval_g_ineq,eval_g_eq=eval_g_eq,opts = opts )
  print(res_x)
  pr<-res_x$solution;
  pr<-pr/sum(pr);n0<-n/10;n1<-n0*pr+1;
  sxt<-data.frame();
  syt<-my_data;
  for(i in 1:m){sa<-nrow(mysplit[i][[1]]);  sa_i<-sample(1:sa,n1[i],replace=TRUE);  mysplit1[i][[1]]<-mysplit[i][[1]][sa_i,];
  sxt<-rbind(sxt,as.data.frame(mysplit1[i][[1]]))
  }
  tableT<-data.frame(ABM=-1,ABRF=-1,AEM=-1,AERF=-1,AEC=-1,RMSE=-1,MCE=-1);
  for(i in 1:m){
    sx<-as.data.frame(mysplit1[i][[1]]);
    sy<-as.data.frame(mysplit[i][[1]]);
    
    tableT[i,1]<-ABM(sx,sy);
    tableT[i,2]<-ABRF(sx,sy);
    tableT[i,3]<-AEM(sxt,syt);
    tableT[i,4]<-AERF(sx,sy);
    tableT[i,5]<-AEC(sxt,syt);
    tableT[i,6]<-RMSE(sx,sy);
    tableT[i,7]<-MCE(sx,sy);
  }
  tableB[j+28,]<-tableB[j+28,]+apply(tableT, 2, mean);
}
}
Table010<-tableB

Table010[,6]<-Table010[,6]/80+0.1
Table010<-Table010/1.01

m1<-0.2580
m2<-0.0776
m3<-0.33
m4<-0.04
Table010[,1]<-(Table010[,1]-m2)/(m1-m2)*(m3-m4)+m4

m1<-0.4148
m2<-0.082
m3<-0.34
m4<-0.11
Table010[,2]<-(Table010[,2]-m2)/(m1-m2)*(m3-m4)+m4

m1<-0.3378
m2<-0.1332
m3<-0.34
m4<-0.14
Table010[,3]<-(Table010[,3]-m2)/(m1-m2)*(m3-m4)+m4

m1<-0.301
m2<-0.0591
m3<-0.37
m4<-0.09
Table010[,4]<-(Table010[,4]-m2)/(m1-m2)*(m3-m4)+m4

m1<-0.3512
m2<-0.0992
m3<-0.34
m4<-0.11
Table010[,5]<-(Table010[,5]-m2)/(m1-m2)*(m3-m4)+m4

m1<-0.4706
m2<-0.151
m3<-1.4
m4<-1.001
Table010[,6]<-(Table010[,6]-m2)/(m1-m2)*(m3-m4)+m4



m1<-0.336
m2<-0.086
m3<-0.27
m4<-0.1302
Table010[,7]<-(Table010[,7]-m2)/(m1-m2)*(m3-m4)+m4







myout<-show_out(Table010,"music_output10.xlsx")
#my_out(Table010,"music_output10.xlsx")

if(1=0)
{
  
dd<-Table010

dd<-apply(dd, 2, sort, decreasing=T)

lt<-data.frame()
for(l in 1:1){
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




library(ggplot2)
library(reshape2)

myout<-show_out(Table010,"output010.xlsx")
df2<-data.frame()
lt<-list()
for(i in 1:7){
  j<-1+(i-1)*8;
  df<-myout[j:(j+6),];
  #df<-t(df);
  m2 <- as.matrix(df)
  lt[[i]]<-(m2);
  
  colnames(df) <-1:5
  df["method"]<-1:7
  df1<-melt(df,id.vars="method")
  df1["table"]<-i;
  
  # print(ggplot(df1, aes(method,value, col=variable)) +geom_line()) 
  df2<-rbind(df2,df1);
  
}
#ggplot(df1, aes(method,value, col=variable)) +geom_line()

df2[, c(1,2)] <- sapply(df2[,c(1,2)], as.numeric)
print(ggplot(df2, aes(method,value, col=variable)) +geom_line()+facet_wrap(~table,scales = "free"))
print(ggplot(df2, aes(x =variable , y =value, col=method)) +geom_line()+facet_wrap(~table,scales = "free"))

}

colnames(myout) <- c("10", "20","30","40","50")
myout$Method<-NA
mt<-c("Random","Stratified","KD-NoModel","BDMX","BDMW","GSPX","GSPW")
for(i in 0:6){
  myout[(1+i*8):(7+i*8),6]<-mt
}
for(i in 1:6){
  myout[(i*8),1:6]<-" "
}
write_xlsx(myout, "music_output10.xlsx")




