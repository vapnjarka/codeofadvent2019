rm(list = ls())
library("xlsx")

#setwd("H:/_D MEGHAJTO/NEPSZAMLALAS2021/R/coa") 
setwd("c:/Users/Asus/Rcof") 
input <- as.vector(read.table("day11.txt", sep =",")) #I wish to have it as a vector, but it is a df
zeros<-data.frame(matrix(ncol = 1000, nrow = 1))
zeros[,]<-0
input<-cbind(input,zeros)

i<-1
j<-2
k<-3
l<-4
null<-0
counter <- 0
null_saver<-vector("numeric",)
output<-vector("numeric",)
output_all<-vector("numeric",)
base<-0

saving<-data.frame(matrix(ncol = 2,)); names(saving) = c("x", "y") # have to rename columns, because later when rbinding the positions, 
                                                                    # saving and cbind(x,y) should have the same header
data<-data.frame(matrix(nrow=200, ncol = 200,))
data<- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
data[,]<-"."
x<-100; y<-100; sd<-"^"; saving<-rbind(saving,cbind(x,y))

#         counter <20 
while( null!=99  ){
  #print(input[1:12])
  
  null<-as.numeric(input[,i]) #integer is an exact type with limited range, and numeric is a floating-point type that can represent a much wider range of value but is inexact
  first<-as.numeric(input[,j])
  secnd<-as.numeric(input[,k])
  third<-as.numeric(input[,l])
  

  if (nchar(null)==1 || nchar(null)==3 || nchar(null)==4) {         #fill up with leading zeros
    null<-sprintf("%05d",null)
  } 
  
  if (as.numeric(substr(null,5,5))==3){   #inputing      E=3
    if (as.numeric(substr(null,3,3))==1) {print("103")}           # C=1
    if ((as.numeric(substr(null,3,3))==0) && data[x,y]==".") {input[,first+1]<-0}  # C=0; provide 0 if the robot is over a black panel
    if ((as.numeric(substr(null,3,3))==2) && data[x,y]==".") {input[,first+1+base]<-0 } #C=2; provide 0 if the robot is over a black panel
    
    if ((as.numeric(substr(null,3,3))==0) && data[x,y]=="#") {input[,first+1]<-1}  # C=0; provide 1 if the robot is over a white panel
    if ((as.numeric(substr(null,3,3))==2) && data[x,y]=="#") {input[,first+1+base]<-1 } #C=2; provide 1 if the robot is over a white panel
    
    
    print(paste("input:",data[x,y]))
    
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
    #print(paste("output:",output))
    output_all<-c(output_all,output)
    
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2
  }
  
  #output_all<-c(0,1)
  while (length(output_all)==2) {
    c<-output_all[1]; d<-output_all[2] # c: color; d: direction
    
    if (c==1 && d==0  && sd=="^") { # white & turn left 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-"<"         #save current shape of direction
      y<-y-1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)  #empty output
    }
    if (length(output_all)!=2) {break}
    
    if (c==1 && d==0  && sd=="<") { # white & turn left 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-"V"         #save current shape of direction
      x<-x+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",) 
    }
    if (length(output_all)!=2) {break}
    
    if (c==1 && d==0  && sd=="V") { # white & turn left 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-">"         #save current shape of direction
      y<-y+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",) 
    }
    if (length(output_all)!=2) {break}
    
    if (c==1 && d==0  && sd==">") { # white & turn left 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-"^"         #save current shape of direction
      x<-x-1          #save the new the position of the roboT
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",) 
    }
    if (length(output_all)!=2) {break}
    
    ###BLACK
    if (c==0 && d==0  && sd=="^") { # black & turn left 90 degrees 
      data[x,y]<-"." #coloring
      sd<-"<"         #save current shape of direction
      y<-y-1          #move the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",) 
    }
    if (length(output_all)!=2) {break}
    
    if (c==0 && d==0  && sd=="<") { # black & turn left 90 degrees 
      data[x,y]<-"." #coloring
      sd<-"V"         #save current shape of direction
      x<-x+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",) 
    }
    if (length(output_all)!=2) {break}
    
    if (c==0 && d==0  && sd=="V") { # black & turn left 90 degrees 
      data[x,y]<-"." #coloring
      sd<-">"         #save current shape of direction
      y<-y+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",) 
    }
    if (length(output_all)!=2) {break}
    
    if (c==0 && d==0  && sd==">") { # black & turn left 90 degrees 
      data[x,y]<-"." #coloring
      sd<-"^"         #save current shape of direction
      x<-x-1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",) 
    }
    if (length(output_all)!=2) {break}
    
    #######
    
    if (c==1 && d==1  && sd=="^") { # white & turn right 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-">"         #save current shape of direction
      y<-y+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
    if (c==1 && d==1  && sd==">") { # white & turn right 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-"V"         #save current shape of direction
      x<-x+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
    if (c==1 && d==1  && sd=="V") { # white & turn right 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-"<"         #save current shape of direction
      y<-y-1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
    if (c==1 && d==1  && sd=="<") { # white & turn right 90 degrees 
      data[x,y]<-"#" #coloring
      sd<-"^"         #save current shape of direction
      x<-x-1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
    ###BLACK
    
    if (c==0 && d==1  && sd=="^") { # black & turn right 90 degrees 
      data[x,y]<-"." #coloring
      sd<-">"         #save current shape of direction
      y<-y+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
    if (c==0 && d==1  && sd==">") { # black & turn right 90 degrees 
      data[x,y]<-"." #coloring
      sd<-"V"         #save current shape of direction
      x<-x+1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
    if (c==0 && d==1  && sd=="V") { # black & turn right 90 degrees 
      data[x,y]<-"." #coloring
      sd<-"<"         #save current shape of direction
      y<-y-1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
    if (c==0 && d==1  && sd=="<") { # black & turn right 90 degrees 
      data[x,y]<-"." #coloring
      sd<-"^"         #save current shape of direction
      x<-x-1          #save the new the position of the robot
      saving<-rbind(saving,cbind(x,y)) #save the positions
      output_all<-vector("numeric",)
    }
    if (length(output_all)!=2) {break}
    
     
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
    
    print(base)
    
    i<-i+2 
    j<-j+2 
    k<-k+2 
    l<-l+2 
  }       
  
  counter <- sum(counter, 1)
  
  null_saver<-c(null_saver,null)
  
}

output_all
null_saver
  
#write.xlsx(data, file="data6.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
#write.xlsx(saving, file="saving.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

unique(saving[c("x","y")])
write.xlsx(unique(saving[c("x","y")]), file="saving2.xlsx")
#2140 to low
#2141 :-)



