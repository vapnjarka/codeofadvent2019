library("xlsx")
install.packages("readxl")
library("readxl")
rm(list = ls())

setwd("c:/Users/Asus/Rcof") 
input <- as.vector(read.table("day10.txt", sep =",", stringsAsFactors =FALSE)) #I wish to have it as a vector, but it is a df
grid<-data.frame(matrix(input, ncol = 42, nrow = 42, byrow=TRUE),stringsAsFactors=FALSE)

steps<-read_excel("day10a.xlsx",col_names=FALSE)
steps<-data.frame(steps)

#position of the observatory is at:37,27
matrix<--1; 


f<-37; s=27;  x<-f; y<-s; n=0   

#up
while(x>0 || y<43){
  y<-y+0;x<-x-1
  if (x<1 || y>42) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(-1,0,x,y,(atan2(1,0)/pi*180)-90,n))
    n<-n+1}
} 
x<-f; y<-s;n<-0

#right
while(x>0 || y<43){
  y<-y+1;x<-x+0
  if (x<1 || y>42) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(0,1,x,y,(atan2(0,1)/pi*180)+90,n))
    n<-n+1}
} 
x<-f; y<-s;n<-0

#down
while(x<43 || y<43){
  y<-y+0;x<-x+1
  if (x>42|| y>42) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(1,0,x,y,(atan2(1,0)/pi*180)+90,n))
    n<-n+1}
} 
x<-f; y<-s;n<-0

#left
while(x>0 || y>0){
  y<-y-1;x<-x+0
  if (x<1 || y<1) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(0,1,x,y,(atan2(0,1)/pi*180)+270,n))
    n<-n+1}
} 
x<-f; y<-s;n<-0

# first quarter 45 deg
while(x>0 || y<43){
  y<-y+1;x<-x-1
  if (x<1 || y>42) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(-1,1,x,y,atan2(1,1)/pi*180,n)) # n is needed to differentiate among 45 degree positons 
    n<-n+1}
}  
x<-f; y<-s;n<-0
# second quarter 45 deg
while(x<43 || y<43){
  y<-y+1;x<-x+1
  if (x>42 || y>42) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(1,1,x,y,(atan2(1,1)/pi*180)+90,n))
    n<-n+1}
}
x<-f; y<-s; n<-0
# third quarter 45 deg
while(x<43 || y>0){
  y<-y-1;x<-x+1
  if (x>42 || y<1) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(1,-1,x,y,(atan2(1,1)/pi*180)+180,n))
    n<-n+1}
}
x<-f; y<-s; n<-0
# fourth quarter 45 deg
while(x>0|| y>0){
  y<-y-1;x<-x-1
  if (x<1 || y<1) {break}
  if (grid[x,y]=="A") {
    matrix<-rbind(matrix,cbind(-1,-1,x,y,(atan2(1,1)/pi*180)+270,n))
    n<-n+1}
}
x<-f; y<-s; n<-0
#1 (first quarter: x always negative, y always positive)
for (row in 1:nrow(steps))    {
  while(x>0 || y<43){
    y<-y+steps[row,1];x<-x-steps[row,2]
    if (x<1 || y>42) {break}
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind((steps[row,2]*-1),steps[row,1],x,y,90-(atan2(steps[row,2],steps[row,1])/pi*180),n))
      n<-n+1}
  }  
  print(row)
  x<-f; y<-s;n<-0
}
#2 first quarter, invert steps
for (row in 1:nrow(steps))    {    
  while(x>0 || y<43){
    y<-y+steps[row,2];x<-x-steps[row,1]
    if (x<1 || y>42) {break}
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind((steps[row,1]*-1),steps[row,2],x,y,90-(atan2(steps[row,1],steps[row,2])/pi*180),n))
      n<-n+1}
  }
  print(row)
  x<-f; y<-s;n<-0
}

#3 (second quarter: x, y always positive)
for (row in 1:nrow(steps))    {
  while(x<43 || y<43){
    y<-y+steps[row,1];x<-x+steps[row,2]
    if (x>42 || y>42) {break}  #<--------------modify based on where is the observatory
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind(steps[row,2],steps[row,1],x,y,(atan2(steps[row,2],steps[row,1])/pi*180)+90,n))
      n<-n+1}
  }  
  print(row)
  x<-f; y<-s;n<-0
}

#4 second quarter, invert steps
for (row in 1:nrow(steps))    {
  while(x<43 || y<43){
    y<-y+steps[row,2];x<-x+steps[row,1]
    if (x>42 || y>42) {break}  #<--------------modify based on where is the observatory
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind(steps[row,1],steps[row,2],x,y,(atan2(steps[row,1],steps[row,2])/pi*180)+90,n))
      n<-n+1}
  }  
  print(row)
  x<-f; y<-s;n<-0
}

#5 (third quarter: x positive, y negative)
for (row in 1:nrow(steps))    {
  while(x<43 || y>0){
    y<-y-steps[row,1];x<-x+steps[row,2]
    if (x>42 || y<1) {break}  #<--------------modify based on where is the observatory
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind(steps[row,2],steps[row,1],x,y,(atan2(steps[row,1],steps[row,2])/pi*180)+180,n))
      n<-n+1}
  }  
  print(row)
  x<-f; y<-s;n<-0
}

#6 third quarter, invert steps
for (row in 1:nrow(steps))    {
  while(x<43 || y>0){
    y<-y-steps[row,2];x<-x+steps[row,1]
    if (x>42 || y<1) {break}  #<--------------modify based on where is the observatory
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind(steps[row,1],steps[row,2],x,y,(atan2(steps[row,2],steps[row,1])/pi*180)+180,n))
      n<-n+1}
    # changed the order of the sides!
  }  
  print(row)
  x<-f; y<-s;n<-0
}

#7 (fourth quarter: x & y negative)
for (row in 1:nrow(steps))    {
  while(x>0 || y>0){
    y<-y-steps[row,1];x<-x-steps[row,2]
    if (x<1|| y<1) {break}  
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind(steps[row,2],steps[row,1],x,y,(atan2(steps[row,2],steps[row,1])/pi*180)+270,n))
      n<-n+1}
  }  
  print(row)
  x<-f; y<-s;n<-0
}

#8 fourth quarter, invert steps
for (row in 1:nrow(steps))    {
  while (x>0 || y>0){
    y<-y-steps[row,2];x<-x-steps[row,1]
    if (x<1|| y<1) {break}  
    if (grid[x,y]=="A") {
      matrix<-rbind(matrix,cbind(steps[row,1],steps[row,2],x,y,(atan2(steps[row,1],steps[row,2])/pi*180)+270,n))
      n<-n+1}
  }  
  print(row)
  x<-f; y<-s;n<-0
}



matrix<-cbind(matrix,NA) #cbind a new empty column

for (m in 1:nrow(matrix)) { #calculate distance from  observatory:abs distance from obs.
  matrix[m,7]<-abs(37-matrix[m,3])+abs(27-matrix[m,4])  #give the coordiantes of the observatory! e.g: 14,12
}
colnames(matrix) <- c("x step", "y step","x", "y", "degree", "nr of asteroids", "manhattan dist")

matrix<-matrix[order(matrix[,5], matrix[,7], matrix[,6], decreasing = FALSE),]
#order by increasing: angle, manhattan dist from observatory, n - nr of asteroid in that angle

matrix<-matrix[order(matrix[,5], decreasing = FALSE),] #by degree

matrix2<-matrix[order( matrix[,6], matrix[,5], decreasing = FALSE),]
#order by nr of asteroid (round) and degrees





