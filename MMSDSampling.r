findNearest<-function(x,y,pop)
{
  N<-nrow(pop)
  
  nearDis<-99999999
  nearest<-0
  for(i in 1:N)
  {
    curDis<-abs(x-pop[i,"x"])+abs(y-pop[i,"y"])
    if(curDis<nearDis)
    {
      nearDis<-curDis
      nearest<-i
    }
  }
  
  nearest$ID<-nearest
  nearest$Dis<-nearDis
  
  return (nearest)
}

MMSD<-function(sampleIDs,pop)
{
  
  sample<-pop[sampleIDs,]
  pop<-pop[-1*sampleIDs,]
 
  N<-nrow(pop)
  MSD<-0
  for(i in 1:N)
  {
    nearest<-findNearest(pop[i,"x"],pop[i,"y"],sample)
    
    MSD<-MSD+nearest$Dis/N
  }
  return (MSD)
}



MMSDSampling<-function(pop,n,radius)
{
  
  c<-1
  step<-0.99
  
  N<-nrow(pop)
  print(N)
  print(n)
  #s1<-pop[sample((1:N),n),]
  s1<- sample((1:N),n)
  
  for(i in 1:2000)
  #while(TRUE)
  {
  
    dt1<-MMSD(s1,pop)
    
    s2<-s1
    
    searchPoint<-TRUE
    while(searchPoint)
    {
      i1 <- round(n*runif(1)+0.5)
      
      rdir<- 2*3.1415926*runif(1)
      rdist<- radius*runif(1)          
      x<- round(pop[s2[i1],"x"]+rdist*cos(rdir)+0.5)
      y <- round(pop[s2[i1],"y"]+rdist*sin(rdir)+0.5)
      newPoint<-findNearest(x,y,pop)$ID
      
      #print(searchPoint)
      
      #print(newPoint)
      searchPoint<-FALSE
      for(newpointID in 1:n)
      {
        if(s2[newpointID]==newPoint)
        {
          searchPoint<-TRUE
          break
        }
      }
    }
    

    s2[i1]<-newPoint
  
    dt2<-MMSD(s2,pop)
    
    if(dt2<dt1)
    {
      s1<-s2
    } 
    else
    {
      
      if(runif(1)<exp((dt1-dt2)/c))
      {
        s1<-s2
      }
    }
    
   # if(abs(dt1-dt2)<0.0001)
    # {
    #    break
    #  }
    
    print(dt1)
    c=c*step
    
  }
  
  return (pop[s1,])
}