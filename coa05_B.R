rm(list = ls())

input<-c(3,225,1,225,6,6,1100,1,238,225,104,0,2,136,183,224,101,-5304,224,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,1101,72,47,225,1101,59,55,225,1101,46,75,225,1101,49,15,224,101,-64,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,102,9,210,224,1001,224,-270,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,101,14,35,224,101,-86,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1102,40,74,224,1001,224,-2960,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1101,10,78,225,1001,39,90,224,1001,224,-149,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1002,217,50,224,1001,224,-1650,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,68,8,225,1,43,214,224,1001,224,-126,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,88,30,225,1102,18,80,225,1102,33,28,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,677,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1107,677,226,224,102,2,223,223,1006,224,344,1001,223,1,223,108,226,226,224,102,2,223,223,1005,224,359,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,374,101,1,223,223,108,677,226,224,102,2,223,223,1006,224,389,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,404,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,419,101,1,223,223,1107,677,677,224,102,2,223,223,1006,224,434,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,449,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,494,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,509,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,524,101,1,223,223,8,226,677,224,1002,223,2,223,1006,224,539,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,554,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,569,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,584,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,599,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,614,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,107,677,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226)
#input<-c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
#input<-c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
#input<-c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
#length(input)

i<-1
j<-2
k<-3
l<-4
null<-0
counter <- 0
null_saver<-vector("numeric",)
output<-vector("numeric",) 

#    counter < 2 
while(  null!=99  ){
  #print(input[1:12])
  
  #null<-1
  null<-input[i] 
  first<-input[j]
  secnd<-input[k]
  third<-input[l]
  
  if (nchar(null)==1 || nchar(null)==3 ) {         #fill up with leading zeros
    null<-sprintf("%04d",null)
  } 
  
  if (as.numeric(substr(null,4,4))==3){   #inputing      E=3
    input[first+1]<-5   ##################!!!!!!!#providing here the input!!!!!!!!!!!!
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
  
  #print(input[225])
  #print(input[226])
  #print(input[1:15])
  
  counter <- sum(counter, 1)
  
  null_saver<-c(null_saver,null)
}


null_saver
#13 978 427 - too high , 2: 11 189 491 OK:-)
print(input[16])
print(input)
