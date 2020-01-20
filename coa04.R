rm(list = ls())
#359282-820401

number<-359282
number2<-vector("numeric",) 
#number!=820401) 

while (number!=820401) {
  
  number<-number+1
  
  if (number %/% 10^4 %% 10^1 >= number %/% 10^5 &&            #2>=1
      number %/% 10^3 %% 10^1 >= number %/% 10^4 %% 10^1 &&    #3>=2
      number %/% 10^2 %% 10^1 >= number %/% 10^3 %% 10^1 &&    #4>=3
      number %/% 10^1 %% 10^1 >= number %/% 10^2 %% 10^1 &&    #5>=4
      number %% 10^1 >= number %/% 10^1 %% 10^1 &&             #6>=5
      
      (number %/% 10^5==number %/% 10^4 %% 10^1 ||             #1=2
       number %/% 10^4 %% 10^1==number %/% 10^3 %% 10^1 ||     #2=3
       number %/% 10^3 %% 10^1==number %/% 10^2 %% 10^1 ||     #3=4
       number %/% 10^2 %% 10^1==number %/% 10^1 %% 10^1 ||     #4=5
       number %/% 10^1 %% 10^1==number %% 10^1                 #5=6
       )
  )
   number2<-c(number2,number)   
      
}

number2
length(number2)
min(number2)

###########PART II.


st<-number2 %/% 10^5
nd<-number2 %/% 10^4 %% 10^1
rd<-number2 %/% 10^3 %% 10^1
th<-number2 %/% 10^2 %% 10^1
fh<-number2 %/% 10^1 %% 10^1
sx<-number2 %% 10^1

number2_invalid<-vector("numeric",)

for (i in 1:length(number2)){
  
  if (st[i]==fh[i] ||    #1=5
      nd[i]==sx[i]  ||    #2=6
      
     (st[i]==th[i] && fh[i]!=sx[i]) ||  #1=4 & 5!=6
      nd[i]==fh[i] ||  #2=5
     (rd[i]==sx[i] && st[i]!=nd[i]) ||  #3=6 & 1!=2
     
     ((st[i]==rd[i] && th[i]!=fh[i]) && (st[i]==rd[i] && fh[i]!=sx[i]))  ||   #1=3 & 4!=5 és #1=3 & 5!=6
     (nd[i]==th[i] && fh[i]!=sx[i])  || #2=4 & 5!=6
     (rd[i]==fh[i] && st[i]!=nd[i])  || #3=5 & 1!=2
     ((th[i]==sx[i] && st[i]!=nd[i]) && (th[i]==sx[i] &&  nd[i]!=rd[i])) || #4=6 & 1!=2 és 4=6 & 2!=3
     (st[i]==rd[i] && th[i]==sx[i]) #1=3 és 4=6 
     
     
     )
  
    number2_invalid<-c(number2_invalid,number2[i])
  
}

number2_invalid #276 #162 #206 #355
length(number2)-length(number2_invalid)   #235 - nem jo, 349 too high!!! 59 nem jo 156 
# 160 nem jo
#316 JOOOO

number2_valid<-setdiff(number2,number2_invalid)
number2_valid
