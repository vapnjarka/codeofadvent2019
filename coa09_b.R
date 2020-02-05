library("xlsx")
rm(list = ls())

https://www.reddit.com/r/adventofcode/comments/e8aw9j/2019_day_9_part_1_how_to_fix_203_error/fac3294/
  

#input<-c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99,rep(0, 100))#memory filled up with zerosinput<-data.frame(as.list(input2))#input2<-data.frame(as.list(input3))
#input<-data.frame(as.list(input))
#input<-c(1102,34915192,34915192,7,4,7,99,0,rep(0, 100)); input<-data.frame(as.list(input))
#input<-c(104,1125899906842624,99,rep(0, 100))
#input2<-c(109, 1, 3, 3, 204, 2, 99, 0, 0); input<-data.frame(as.list(input2))

setwd("H:/_D MEGHAJTO/NEPSZAMLALAS2021/R/coa") #setwd("c:/Users/Asus/Rcof") 
input <- as.vector(read.table("day09.txt", sep =",")) #I wish to have it as a vector, but it is a df
zeros<-data.frame(matrix(ncol = 1000, nrow = 1))
zeros[,]<-0
input<-cbind(input,zeros)
#write.xlsx(input, file="input.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


i<-1
j<-2
k<-3
l<-4
null<-0
counter <- 0
null_saver<-vector("numeric",)
output<-vector("numeric",) 
base<-0

#      counter <70   
while(  null!=99  ){
  #print(input[1:12])
  
  null<-as.numeric(input[,i]) #integer is an exact type with limited range, and numeric is a floating-point type that can represent a much wider range of value but is inexact
  first<-as.numeric(input[,j])
  secnd<-as.numeric(input[,k])
  third<-as.numeric(input[,l])
  

  if (nchar(null)==1 || nchar(null)==3 || nchar(null)==4) {         #fill up with leading zeros
    null<-sprintf("%05d",null)
  } 
  
  if (as.numeric(substr(null,5,5))==3){   #inputing      E=3
    #if (as.numeric(substr(null,2,2))==1) {}           # C=1
    if (as.numeric(substr(null,3,3))==0) {input[,first+1]<-2}  # C=0 
    if (as.numeric(substr(null,3,3))==2) {input[,first+1+base]<-2 }  # C=2      
    #input[first+1]<-5
    ##################The BOOST program will ask for a single input; run it in test mode by providing it the value 1.
    
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2
  }
  
  
  if (as.numeric(substr(null,5,5))==1){   #addition     #E=1
    
  #elso tag:
  if (as.numeric(substr(null,3,3))==1) {a<-first}           # C=1
  if (as.numeric(substr(null,3,3))==0) {a<-input[,first+1]}  # C=0 
  if (as.numeric(substr(null,3,3))==2) {a<-input[,first+1+base]}  # C=2   
    
  #masodik tag:
  if (as.numeric(substr(null,2,2))==1) {b<-secnd}           # B=1
  if (as.numeric(substr(null,2,2))==0) {b<-input[,secnd+1]}  # B=0  
  if (as.numeric(substr(null,2,2))==2) {b<-input[,secnd+1+base]}  # B=2    
  
  #harmadik tag:
  if (as.numeric(substr(null,1,1))==0) {input[,third+1]<-a+b}      #A=0
  if (as.numeric(substr(null,1,1))==2) {input[,third+1+base]<-a+b}      #A=2    
      
      i<-i+4 
      j<-j+4 
      k<-k+4 
      l<-l+4 
  }
  
  
  if(as.numeric(substr(null,5,5))==2){   #multiplication     #E=2
    
    #elso tag:
    if (as.numeric(substr(null,3,3))==1) {a<-first}           # C=1
    if (as.numeric(substr(null,3,3))==0) {a<-input[,first+1]}  # C=0        
    if (as.numeric(substr(null,3,3))==2) {a<-input[,first+1+base]}  # C=2
    
    #masodik tag:
    if (as.numeric(substr(null,2,2))==1) {b<-secnd}           # B=1
    if (as.numeric(substr(null,2,2))==0) {b<-input[,secnd+1]}  # B=0  
    if (as.numeric(substr(null,2,2))==2) {b<-input[,secnd+1+base]}  # B=2 
    
    #harmadik tag:
    if (as.numeric(substr(null,1,1))==0) {input[,third+1]<-a*b} #A=0
    if (as.numeric(substr(null,1,1))==2) {input[,third+1+base]<-a*b} #A=2
    
    i<-i+4 
      j<-j+4 
      k<-k+4 
      l<-l+4 
  }       
  
  
  if (as.numeric(substr(null,5,5))==4){   #outputing   E=4
    
    if (as.numeric(substr(null,3,3))==1)  {a<-first}           # C=1
    if (as.numeric(substr(null,3,3))==0)  {a<-input[,first+1]}  # C=0  
    if (as.numeric(substr(null,3,3))==2)  {a<-input[,first+1+base]}  # C=2 
    
    output<-a
    print(paste("output:",output))
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2
  }
  
  if (as.numeric(substr(null,5,5))==5){   #jump-if-true   E=5
    
    if (as.numeric(substr(null,3,3))==1)  {a<-first}           # C=1
    if (as.numeric(substr(null,3,3))==0)  {a<-input[,first+1]}  # C=0
    if (as.numeric(substr(null,3,3))==2)  {a<-input[,first+1+base]}  # C=2
    
    if (as.numeric(substr(null,2,2))==1)  {b<-secnd+1}           # B=1
    if (as.numeric(substr(null,2,2))==0)  {b<-input[,secnd+1]}   # B=0
    if (as.numeric(substr(null,2,2))==2)  {b<-input[,secnd+1+base]}   # B=2
    
        if (a!=0 && (as.numeric(substr(null,2,2))==0 || as.numeric(substr(null,2,2))==2)) { #position/relative
          i<-b+1
          j<-i+1
          k<-j+1
          l<-k+1
        }
        
        if (a!=0 && as.numeric(substr(null,2,2))==1) {   #value
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
    
  
  
  if (as.numeric(substr(null,5,5))==6){   #jump-if-true   E=6
  
    #elso tag:
    if (as.numeric(substr(null,3,3))==1)  {a<-first}             # C=1
    if (as.numeric(substr(null,3,3))==0)  {a<-input[,first+1]}    # C=0
    if (as.numeric(substr(null,3,3))==2)  {a<-input[,first+1+base]}    # C=2
    
    #masodik tag:
    if (as.numeric(substr(null,2,2))==1)  {b<-secnd+1}             # B=1
    if (as.numeric(substr(null,2,2))==0)  {b<-input[,secnd+1]}    # B=0
    if (as.numeric(substr(null,2,2))==2)  {b<-input[,secnd+1+base]}    # B=2
  
    if (a==0 && (as.numeric(substr(null,2,2))==0 || as.numeric(substr(null,2,2))==2)) { #position/relative
      i<-b+1
      j<-i+1
      k<-j+1
      l<-k+1
    }
    if (a==0 && as.numeric(substr(null,2,2))==1 ) { #value
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
  
  
  if(as.numeric(substr(null,5,5))==7){   #less than     #E=7
    
    #elso tag:
    if (as.numeric(substr(null,3,3))==1) {a<-first}           # C=1
    if (as.numeric(substr(null,3,3))==0) {a<-input[,first+1]}    # C=0        
    if (as.numeric(substr(null,3,3))==2) {a<-input[,first+1+base]}    # C=2 
    
    #masodik tag:
    if (as.numeric(substr(null,2,2))==1) {b<-secnd}           # B=1
    if (as.numeric(substr(null,2,2))==0) {b<-input[,secnd+1]}    # B=0  
    if (as.numeric(substr(null,2,2))==2) {b<-input[,secnd+1+base]}    # B=2 
    
    #harmadik tag:
    if (as.numeric(substr(null,1,1))==0) {   #A=0
      if (a<b)  {input[,third+1]<-1}
      if (a>=b) {input[,third+1]<-0}}
    
    if (as.numeric(substr(null,1,1))==2) {   #A=2
      if (a<b)  {input[,third+1+base]<-1}
      if (a>=b) {input[,third+1+base]<-0}}
    
    i<-i+4 
    j<-j+4 
    k<-k+4 
    l<-l+4 
  }       
  
  if(as.numeric(substr(null,5,5))==8){   #equals     #E=8
    
    #elso tag:
    if (as.numeric(substr(null,3,3))==1) {a<-first}           # C=1    value mode
    if (as.numeric(substr(null,3,3))==0) {a<-input[,first+1]}  # C=0    pozition mode   
    if (as.numeric(substr(null,3,3))==2) {a<-input[,first+1+base]}  # C=2    relative mode 
    
    #masodik tag:
    if (as.numeric(substr(null,2,2))==1) {b<-secnd}           # B=1   value mode
    if (as.numeric(substr(null,2,2))==0) {b<-input[,secnd+1]}  # B=0   position mode
    if (as.numeric(substr(null,2,2))==2) {b<-input[,secnd+1+base]}  # B=2   relative mode
    
    #harmadik tag:
    if (as.numeric(substr(null,1,1))==0) {    #A=0
      if (a==b)  {input[,third+1]<-1}
      if (a!=b)  {input[,third+1]<-0}}
    
    if (as.numeric(substr(null,1,1))==2) {    #A=2
      if (a==b)  {input[,third+1+base]<-1}
      if (a!=b)  {input[,third+1+base]<-0}}
    
    i<-i+4 
    j<-j+4 
    k<-k+4 
    l<-l+4 
  }       
  
  if(as.numeric(substr(null,5,5))==9){   #adjusts the relative base     #E=9
    if (as.numeric(substr(null,3,3))==1) {base<-base+first}           # C=1    value mode
    if (as.numeric(substr(null,3,3))==0) {base<-base+input[,first+1]}  # C=0    pozition mode   
    if (as.numeric(substr(null,3,3))==2) {base<-base+input[,first+1+base]}  # C=2    relative mode 
    
    #print(base)
    
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2 
  }       
  
  #counter <- sum(counter, 1)
  
  #null_saver<-c(null_saver,null)
}


null_saver
input[,188]

input[,1001]
