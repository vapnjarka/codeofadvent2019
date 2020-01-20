install.packages('gtools')
library(gtools)



rm(list = ls())
#input<-c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
#input<-c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
input<-c(3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99)
i<-1; j<-2; k<-3; l<-4; save6<-NA
null<-0
null_saver<-vector("numeric",)
output<-NA       #vector("numeric",) output in one amplifier, during running
output_touse<-NA       #vector("numeric",) output to use as input in next step
max_thruster<-0
perm<-c(5,6,7,8,9)
phase_set<-permutations(n=5,r=5,v=perm,repeats.allowed=F) #create 120 variations
#phase_set<-matrix(9:5,nrow = 1, ncol = 5) #sample 1
p<-1 #input counter for inputing E=3
y<-1 
save1<-NA; save2<-NA; save3<-NA; save4<-NA; save5<-NA
counterb<-0; counter<-0; counter_legb<-0
inp<-0; x<-0



while (counterb<130 ){ 
x<-x+1                # phase setting counter 
  counterb<-counterb+1
  
while (null!=99)  {  # amplifier counter A to E.  for (y in 1:5)

  if(counter==60) {break}
  counter<-counter+1

 
if (is.na(output_touse)==TRUE) {inp<-c(phase_set[x,y],0)}  # phase setting + 0 for amplifier A at the beginning
if (is.na(output_touse)==FALSE) 
        {inp<-c(phase_set[x,y],output_touse)} #phase setting + input signal for amplifier A-E
 
while(  null!=99 ){
  if(counter_legb==20) {break}
  counter_legb<-counter_legb+1
  print(paste(i,"inp:",inp[1:2],"y:",y, "counter_legb",counter_legb,x)) #inp: inputs for E=3
  
  
  if (is.na(get(paste("save", y, sep="")))==FALSE && counter_legb==1) { #if there is a saved instruction pointer, and it is the beginning of the loop
    input<-(get(paste0("saved_input", y, sep=""))) #using the relevant input
    i<-get(paste0("save", y, sep=""))              ##using the relevant pointers: i,j,k,l
    j<-i+1; k<-i+2; l<-i+3}
   
    null<-input[i] 
    first<-input[j]
    secnd<-input[k]
    third<-input[l]
  
  
  if (null==99) {break} # 
  #if (i>length(input)) {break} #  #is.na(output)==FALSE

  if (nchar(null)==1 || nchar(null)==3 ) {         #fill up with leading zeros
    null<-sprintf("%04d",null)
  } 
 
  if (as.numeric(substr(null,4,4))==3){   #inputing      E=3
    
    if (is.na(inp[p])==FALSE)     # if next input is available
    {input[first+1]<-inp[p]    
     p<-p+1 #next input
   
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2
  }
  }
  
   
  if (as.numeric(substr(null,4,4))==4){   #outputing   E=4
    
    if (as.numeric(substr(null,2,2))==1)  {a<-first}           # C=1
    if (as.numeric(substr(null,2,2))==0)  {a<-input[first+1]}  # C=0   
    
    if (is.na(a)==FALSE) output<-a  #not to overwrite output with NA
    output_touse<-output
    print(paste("output:",output))
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2
    
    if (is.na(inp[p])==TRUE) {  # if there is no more input and we have our output
      assign(paste("save", y, sep=""),i) #save the current instruction pointer, and 
      assign(paste("saved_input", y, sep=""),input) #save the current stage of input, and 
      counter_legb<-0 #re-initialize inner counter, and
        if (is.na(save2)==TRUE || is.na(save3)==TRUE || is.na(save4)==TRUE || is.na(save5)==TRUE) { #if the next amplifier has not run yet, i.e very first line when we need the phase settings
          i<-1; j<-2; k<-3; l<-4;                            #re-initialize inner counters, and
          p<-1                                               #re-initialize pointer of input/output
        }
         if (is.na(save2)==FALSE && is.na(save3)==FALSE && is.na(save4)==FALSE && is.na(save5)==FALSE) { # from the second round, let's p is always 2
        p<-2                                              
         }
    }
      {break} #break
      
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
      
      #inputchange<-rbind(inputchange,input)
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
      
      #inputchange<-rbind(inputchange,input)
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
  
  
  null_saver<-c(null_saver,null)
}
  
print(paste("output again:",output))
output<-NA
if (null==99) {break} # stop feedback loop

if (y<6) {y<-y+1} # go to next amplifier
if (y==6) {y<-1} # to repeat the feedback 
}

#if (y==6) {y<-1}  # to repeat the feedback 
if (max_thruster<output_touse) max_thruster<-output_touse #highest signal that can be sent to the thruster 

null<-0
output_touse<-NA  
save1<-NA; save2<-NA; save3<-NA; save4<-NA; save5<-NA
i<-1; j<-2; k<-3; l<-4; save6<-NA #ezt ne aktivald!
#input<-c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
#input<-c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
input<-c(3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99)
counter_legb<-0; counter<-0
p<-1; y<-1
  }
  


max_thruster #result: 35993240


null_saver
input[26] # 99 - quit
input[23] # jumps backwards

https://www.reddit.com/r/adventofcode/comments/e7aqcb/2019_day_7_part_2_confused_with_the_question/
  
 united<- rbind(saved_input1,saved_input2,saved_input3,saved_input4,saved_input5)
