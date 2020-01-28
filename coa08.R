rm(list = ls())


setwd("H:/R/coa")
input <- as.vector(read.table("day08.txt", sep =",")) #I wish to have it as a vector, but it is a df



splitdf <- function(df, n) {              #function for splitting
  indx <- matrix(seq_len(ncol(df)), ncol = n)
  lapply(seq_len(n), function(x) df[, indx[, x]])
}

new<-splitdf(input, 100) #splitting

a<-vector("numeric",)

for (i in 1:100){
  a[i]<-sum(new[[i]] == 0)   #count zeros in every layer
}
a
min(a)

sum(new[[6]] == 1)*sum(new[[6]] == 2)
#2318

###########################################################################♫
l<-1 #layer vector
#picture<-data.frame(matrix(ncol = 150, nrow = 1))
install.packages('data.table')
library(data.table)
library("xlsx")



for (p in 1:150) { 
  if (new[[1]][p]==2) {      #whether pixel from first layer is valid, if not go to next layer
      while (new[[l]][p]!=0 || new[[l]][p]!=1) {  
        print(paste("pixel:",new[[l]][p]))
        l<-l+1
        if (new[[l]][p]==0 || new[[l]][p]==1) {break} #it was not enough to give the exit condition in the begining
        }
      picture[,p]<-new[[l]][p]  #if pixel is valid, copy it to a new layer called picture
      l<-1
      
      }
  else {                    # if pixel from the first layer is valid, copy it to a new layer called picture
    picture[,p]<-new[[1]][p]
  }
}


new2<-splitdf(picture, 6) #splitting
picture2<-rbindlist(new2, use.names=FALSE, fill=FALSE) #bind it into 6*25 dataframe

write.xlsx(picture2, file="picture.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
