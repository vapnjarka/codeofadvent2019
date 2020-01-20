install.packages('gtools')
library(gtools)

rm(list = ls())

#input<-c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
#input<-c(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
#input<-c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
input<-c(3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99)

i<-1
j<-2
k<-3
l<-4
null<-0
counter <- 0
null_saver<-vector("numeric",)
output<-NA       #vector("numeric",) 
max_thruster<-0

perm<-c(0,1,2,3,4)
phase_set<-permutations(n=5,r=5,v=perm,repeats.allowed=F) #create 120 variations
p<-1 #input counter for inputing E=3


for (x in 1:120) { # phase setting counter
  
for (y in 1:5) { #amplifier counter A to E
 
  
if (is.na(output)==TRUE) {inp<-c(phase_set[x,y],0)}  #phase setting + input signal for amplifier A
if (is.na(output)==FALSE) 
        {inp<-c(phase_set[x,y],output)} #phase setting + input signal for amplifier B-E

  
while(  null!=99  ){
  
  null<-input[i] 
  first<-input[j]
  secnd<-input[k]
  third<-input[l]
  
  if (null==99) {break} #it wasn't enough to met the break condition (null!=99), it was needed to break directly
  
  if (nchar(null)==1 || nchar(null)==3 ) {         #fill up with leading zeros
    null<-sprintf("%04d",null)
  } 
  
 
  if (as.numeric(substr(null,4,4))==3){   #inputing      E=3
    
    input[first+1]<-inp[p]    ##################!!!!!!!#providing here the input!!!!!!!!!!!!
    p<-p+1
    
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2
  }
  
  if (as.numeric(substr(null,4,4))==1){   #addition     #E=1
    
  #elso tag:
  if (as.numeric(substr(null,2,2))==1) {a<-first}           # C=1
  if (as.numeric(substr(null,2,2))==0) {a<-input[first+1]}  # C=0        
    
  #masodik tag:
  if (as.numeric(substr(null,1,1))==1) {b<-secnd}           # B=1
  if (as.numeric(substr(null,1,1))==0) {b<-input[secnd+1]}  # B=0  
    
    input[third+1]<-a+b  
      i<-i+4 
      j<-j+4 
      k<-k+4 
      l<-l+4 
  }
  
  
  if(as.numeric(substr(null,4,4))==2){   #multiplication     #E=2
    
    #elso tag:
    if (as.numeric(substr(null,2,2))==1) {a<-first}           # C=1
    if (as.numeric(substr(null,2,2))==0) {a<-input[first+1]}  # C=0        
    
    #masodik tag:
    if (as.numeric(substr(null,1,1))==1) {b<-secnd}           # B=1
    if (as.numeric(substr(null,1,1))==0) {b<-input[secnd+1]}  # B=0  
    
    input[third+1]<-a*b
      i<-i+4 
      j<-j+4 
      k<-k+4 
      l<-l+4 
  }       
  
  
  if (as.numeric(substr(null,4,4))==4){   #outputing   E=4
    
    if (as.numeric(substr(null,2,2))==1)  {a<-first}           # C=1
    if (as.numeric(substr(null,2,2))==0)  {a<-input[first+1]}  # C=0   
    
    output<-a
    print(paste("output:",output))
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2
  }
  
  if (as.numeric(substr(null,4,4))==5){   #jump-if-true   E=5
    
    if (as.numeric(substr(null,2,2))==1)  {a<-first}           # C=1
    if (as.numeric(substr(null,2,2))==0)  {a<-input[first+1]}  # C=0
    
    if (as.numeric(substr(null,1,1))==1)  {b<-secnd+1}           # B=1
    if (as.numeric(substr(null,1,1))==0)  {b<-input[secnd+1]}   # B=0
        if (a!=0 && as.numeric(substr(null,1,1))==0) {
          i<-b+1
          j<-i+1
          k<-j+1
          l<-k+1
        }
        
        if (a!=0 && as.numeric(substr(null,1,1))==1) {
          i<-b
          j<-i+1
          k<-j+1
          l<-k+1
        }
    
        if (a==0) {
          i<-i+3
          j<-j+3
          k<-k+3
          l<-l+3}
    }
    
  
  
  if (as.numeric(substr(null,4,4))==6){   #jump-if-true   E=6
  
    #elso tag:
    if (as.numeric(substr(null,2,2))==1)  {a<-first}             # C=1
    if (as.numeric(substr(null,2,2))==0)  {a<-input[first+1]}    # C=0
    
    #masodik tag:
    if (as.numeric(substr(null,1,1))==1)  {b<-secnd+1}             # B=1
    if (as.numeric(substr(null,1,1))==0)  {b<-input[secnd+1]}    # B=0
  
    if (a==0 && as.numeric(substr(null,1,1))==0) {
      i<-b+1
      j<-i+1
      k<-j+1
      l<-k+1
    }
    if (a==0 && as.numeric(substr(null,1,1))==1) {
      i<-b
      j<-i+1
      k<-j+1
      l<-k+1
    }
    if (a!=0) {
      i<-i+3
      j<-j+3
      k<-k+3
      l<-l+3
    }
  }
  
  
  if(as.numeric(substr(null,4,4))==7){   #less than     #E=7
    
    #elso tag:
    if (as.numeric(substr(null,2,2))==1) {a<-first}           # C=1
    if (as.numeric(substr(null,2,2))==0) {a<-input[first+1]}    # C=0        
    
    #masodik tag:
    if (as.numeric(substr(null,1,1))==1) {b<-secnd}           # B=1
    if (as.numeric(substr(null,1,1))==0) {b<-input[secnd+1]}    # B=0  
    
    if (a<b)  {input[third+1]<-1}
    if (a>=b) {input[third+1]<-0}
    
    i<-i+4 
    j<-j+4 
    k<-k+4 
    l<-l+4 
  }       
  
  if(as.numeric(substr(null,4,4))==8){   #equals     #E=8
    
    #elso tag:
    if (as.numeric(substr(null,2,2))==1) {a<-first}           # C=1    value mode
    if (as.numeric(substr(null,2,2))==0) {a<-input[first+1]}  # C=0    pozition mode   
    
    #masodik tag:
    if (as.numeric(substr(null,1,1))==1) {b<-secnd}           # B=1   value mode
    if (as.numeric(substr(null,1,1))==0) {b<-input[secnd+1]}  # B=0   position mode
    
    if (a==b)  {input[third+1]<-1}
    if (a!=b)  {input[third+1]<-0}
    
    i<-i+4 
    j<-j+4 
    k<-k+4 
    l<-l+4 
  }       
  
  #counter <- sum(counter, 1)
  
  null_saver<-c(null_saver,null)
}


  
  #print(paste("output again:",output))

i<-1
j<-2
k<-3
l<-4
p<-1
null<-0

}

  if (max_thruster<output) max_thruster<-output #highest signal that can be sent to the thruster 
  output<-NA #needed to re-initialize output variable
  
}

max_thruster #result: 368584


null_saver
input
