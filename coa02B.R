rm(list = ls())
vector1<-seq(0,99,1) #make a sequence from 0 to 99 by 1. this is input
vector2<-seq(0,99,1)
i<-1
j<-2
k<-3
l<-4
null<-0
null_output<-0
counter <- 0
null_output_save<-vector("numeric",) 

for (x in 1:100){
  for (y in 1:100){
    input2<-c(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,9,23,1,5,23,27,1,27,9,31,1,6,31,35,2,35,9,39,1,39,6,43,2,9,43,47,1,47,6,51,2,51,9,55,1,5,55,59,2,59,6,63,1,9,63,67,1,67,10,71,1,71,13,75,2,13,75,79,1,6,79,83,2,9,83,87,1,87,6,91,2,10,91,95,2,13,95,99,1,9,99,103,1,5,103,107,2,9,107,111,1,111,5,115,1,115,5,119,1,10,119,123,1,13,123,127,1,2,127,131,1,131,13,0,99,2,14,0,0)
    input2[2]<-vector1[x] #addresses for adding/multipling are granted from a 0-99 vector
    input2[3]<-vector2[y]
    #print(10*x+y)
    #print(input2[1:10])
    i<-1
    j<-2
    k<-3
    l<-4
    null<-0
    
    while(null!=99){   #  | null_output!=19690720 | counter < 3 | null!=99
      #cat('elozo',input2[1:12])
      null<-input2[i]
      first<-input2[j]
      secnd<-input2[k]
      third<-input2[l]
      
      if (null==1) {input2[third+1]<-(input2[first+1]+input2[secnd+1])}
      if (null==2) {input2[third+1]<-(input2[first+1]*input2[secnd+1])}
      print(input2[1:10])
      null_output<-input2[1]
      null_output_save[10*x+y]<-null_output
      if(null_output==19690720){break}       #19690720 736708
      i<-i+4
      j<-j+4
      k<-k+4
      l<-l+4
 
      
      counter <- sum(counter, 1)
    }
    counter <-0
    if(null_output==19690720){break}
  }
  if(null_output==19690720){break}
}
null_output_save
null_output
