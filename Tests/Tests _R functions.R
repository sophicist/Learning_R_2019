
H =data.frame(names =c("james","Njuguna","juma","avicii"),
              age = c(-1,23,56,-1),
              Country =c("Alaska","Kenya","-","-"),
              jez = c("23","er","78,"))
df<-data.frame(text =c(-1,NA,23,78,231,90,4,22,34,-1,-1,-1,-1,NA,-2,45))
clean <- function(df){
  df[df==-1]<-NA
    return(df)
  
}
clean(df)

Map(toupper,H$names)
Map(substr(1,0,3),H$names)
Filter({function(x) x>40},df$text)
clean(H)

staker<-function(){
    Y<-rbind(dt[,c(1,2)],dt[,c(1,3)],use.names=FALSE)
    Y<-rbind(Y,dt[,c(1,4)],use.names=FALSE)
  Y<-rbind(Y,dt[,c(1,5)],use.names=FALSE)
  
}
X <-data.frame(Country = c("kenya","Uganda","Sychellers"),
               G1 =c(1,34,45),
               G2 =c(11,34,45),
               G3 =c(15,3,45),
               G4 =c(10,34,49))
Xstaker()

staker<-function(x =2){
  Y<-rbind(dt[,c(1,x)],dt[,c(1,x+1)],use.names=FALSE)
  Y<-rbind(Y,dt[,c(1,4)],use.names=FALSE)
  Y<-rbind(Y,dt[,c(1,5)],use.names=FALSE)
  
}

kam<-function(x){
  setDF(dt)
  G1<-dt[,c(1,1:x+1)]
  G2<-dt[,c(1,x+2:x+2+x)]
  G3<-dt[,c(1,x+x+3:x+x+x+3)]
  G4<-dt[,c(1,x+x+x+4:x+x+x+x+4)]
  Y<-rbind(G1,G2,use.names=FALSE)
  Y<-rbind(Y,G3,use.names=FALSE)
  Y<-rbind(Y,G4,use.names=FALSE)
}
J<-kam(7)

ken <-function(df){
  if(df=="Kenya"){return ("yah")}
  else{return(c(1,2,3))}
}
ken("Uganda")
ken("Kenya")
