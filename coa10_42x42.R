library("xlsx")
install.packages("readxl")
library("readxl")
rm(list = ls())

#input<-c("E","A","E","E","A","E","E","E","E","E","A","A","A","A","A","E","E","E","E","A","E","E","E","A","A") #A: asteroid E:empty 
#grid<-data.frame(matrix(input, ncol = 5, nrow = 5, byrow=TRUE),stringsAsFactors=FALSE) # grid, filled up by rows

#input<-c("E","E","E","E","E","E","A","E","A","E","A","E","E","A","E","A","E","E","E","E","E","E","A","A","A","A","A","A","A","E","E","A","E","A","E","A","A","A","E","E","E","A","E","E","A","E","E","E","E","E","E","E","A","E","E","E","E","A","E","A","A","E","E","A","E","E","E","E","A","E","E","A","A","E","A","E","E","A","A","A","A","A","E","E","E","A","E","E","A","E","E","A","E","E","E","E","A","A","A","A")
#grid<-data.frame(matrix(input, ncol = 10, nrow = 10, byrow=TRUE),stringsAsFactors=FALSE) # grid, filled up by rows

#input<-c("A","E","A","E","E","E","A","E","A","E","E","A","A","A","E","E","E","E","A","E","E","A","E","E","E","E","A","E","E","E","A","A","E","A","E","A","E","A","E","A","E","E","E","E","A","E","A","E","A","E","E","A","A","E","E","A","A","A","E","A","E","E","A","E","E","E","A","A","E","E","E","E","A","A","E","E","E","E","A","A","E","E","E","E","E","E","A","E","E","E","E","A","A","A","A","E","A","A","A","E")
#grid<-data.frame(matrix(input, ncol = 10, nrow = 10, byrow=TRUE),stringsAsFactors=FALSE) # grid, filled up by rows

#input<-c("E","A","E","E","A","E","E","A","A","A","A","A","A","A","E","A","A","A","E","A","E","E","E","E","A","A","A","E","A","E","E","E","A","A","A","E","A","A","E","A","A","A","E","A","A","E","A","E","A","E","E","E","E","E","A","A","A","E","E","A","E","E","A","E","A","E","E","A","E","A","A","E","E","A","E","A","E","A","A","A","E","A","A","E","E","E","A","A","E","A","E","E","E","E","E","A","E","A","E","E")
#grid<-data.frame(matrix(input, ncol = 10, nrow = 10, byrow=TRUE),stringsAsFactors=FALSE)

#input<-c("E","A","E","E","A","A","E","A","A","A","E","E","E","A","A","A","A","A","A","A","A","A","E","A","A","A","A","A","A","A","A","A","A","A","A","E","E","A","A","E","E","A","E","A","A","A","A","A","A","E","A","A","A","A","A","A","A","A","E","A","E","A","A","A","E","A","A","A","A","A","A","A","E","A","A","A","A","E","A","E","A","A","A","A","A","E","A","A","E","A","E","A","A","E","A","A","A","E","A","A","E","E","A","A","A","A","A","E","E","A","E","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","E","A","A","A","A","E","E","E","E","A","A","A","E","A","E","A","E","A","A","A","A","E","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","E","A","A","E","A","A","A","E","E","A","A","A","A","E","E","E","E","A","A","A","A","A","A","E","E","A","A","E","A","A","A","A","A","A","A","A","A","A","A","E","A","A","E","A","A","A","A","E","E","E","A","A","E","E","A","E","A","A","A","A","A","E","E","A","E","A","A","A","A","A","A","E","A","A","A","A","A","E","E","E","A","E","A","A","A","A","A","A","A","A","A","A","E","E","E","A","E","A","A","A","A","A","A","A","A","A","A","E","A","A","A","A","A","A","A","E","A","A","A","A","E","A","E","A","A","A","E","A","A","A","E","A","E","A","A","E","E","E","E","A","A","E","A","A","E","A","A","A","E","E","A","A","A","A","A","E","A","E","A","E","A","A","A","A","A","A","A","A","A","A","A","E","A","A","A","A","E","A","E","A","E","A","A","A","A","A","E","A","A","A","A","E","A","A","A","A","A","A","E","A","A","E","A","A","A","A","E","A","A","E","A","E","E","A","A")
#grid<-data.frame(matrix(input, ncol = 20, nrow = 20, byrow=TRUE),stringsAsFactors=FALSE)
setwd("c:/Users/Asus/Rcof") 
input <- as.vector(read.table("day10.txt", sep =",", stringsAsFactors =FALSE)) #I wish to have it as a vector, but it is a df
grid<-data.frame(matrix(input, ncol = 42, nrow = 42, byrow=TRUE),stringsAsFactors=FALSE)

#step<-c(1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,1,10,1,11,1,12,1,13,2,3,2,5,2,7,2,9,2,11,2,13,3,4,3,5,3,7,3,8,3,10,3,11,3,13,4,5,4,7,4,9,4,11,4,13,5,6,5,7,5,8,5,9,5,11,5,12,5,13,6,7,6,9,6,11,6,13,7,8,7,9,7,10,7,11,7,12,7,13,8,9,8,11,8,13,9,10,9,11,9,13,10,13,
#        1,14,3,14,5,14,9,14,11,14,13,14,1,15,2,15,4,15,7,15,8,15,11,15,13,15,1,16,3,16,5,16,7,16,9,16,11,16,13,16,14,15,15,16,1,17,2,17,3,17,4,17,5,17,6,17,7,17,8,17,9,17,10,17,11,17,12,17,13,17,14,17,15,17,16,17,1,18,5,18,7,18,11,18,13,18,17,18,
#       1,19,2,19,3,19,4,19,5,19,6,19,7,19,8,19,9,19,10,19,11,19,12,19,13,19,14,19,15,19,16,19,17,19,18,19,1,20,3,20,7,20,9,20,11,20,13,20,15,20,17,20,19,20,1,21,2,21,4,21,5,21,8,21,10,21,11,21,13,21,16,21,17,21,19,21,20,21,
#        1,22,3,22,5,22,7,22,9,22,13,22,15,22,17,22,19,22,21,22)
#steps<-data.frame(matrix(step, ncol = 2, nrow = 225))

steps<-read_excel("day10a.xlsx",col_names=FALSE)
steps<-data.frame(steps)


grid[1,5]




row<-1
f<-3; s=2; x<-f; y<-s
sum1<-0;sum<-0
#observatorium can be only on an asteroid

for (f in 1:42) {
  for (s in 1:42) {

  x<-f;y<-s ;print(paste("x,y:",x,y,grid[f,s]))
  
  if (grid[f,s]=="A") {

#1 horizontal angle
  while(x<43 || y<43){
      y<-y+1 #increase column
      if (x>42 || y>42) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s
    #print(paste("x,y:",x,y,"sum1:",sum1))
  while(x>0 || y>0){
      y<-y-1 #decrease column
      if (x<1 || y<1) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s
    #print(paste("x,y:",x,y,"sum2:",sum1))
#2 vertical angle
  while(x<43 || y<43){
      x<-x+1 #increase row
      if (x>42 || y>42) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s
    #print(paste("x,y:",x,y,"sum3:",sum1))
  while(x>0 || y>0){
      x<-x-1 #decrease row
      if (x<1 || y<1) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s
    #print(paste("x,y:",x,y,"sum4:",sum1))
#3 45 angle  (down right & up left)
  while(x<43 || y<43){
      y<-y+1;x<-x+1 # increase col & row
      if (x>42 || y>42) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s
    #print(paste("x,y:",x,y,"sum5:",sum1))
  while(x>0 || y>0){
      y<-y-1;x<-x-1  #decrease col & row
      if (x<1 || y<1) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s
    #print(paste("x,y:",x,y,"sum6:",sum1))
#4 45 angle  (down left & up right)
  while(x<43 || y>0){
      y<-y-1;x<-x+1 # decrease col and increase row
      if (x>42 || y<1) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s
    #print(paste("x,y:",x,y,"sum7:",sum1))
  while(x>0 || y<43){
      y<-y+1;x<-x-1  #increase col and decrease row
      if (x<1 || y>42) {break}
      if (grid[x,y]=="A") {sum1<-sum1+1}
      if (grid[x,y]=="A") {break}
    }
    x<-f;y<-s 
    #print(paste("x,y:",x,y,"sum8:",sum1))
for (row in 1:nrow(steps))    {    #<---------------declare number of steps!
#5 (down right & up left)
while(x<43 || y<43){
  y<-y+steps[row,1];x<-x+steps[row,2]# increase col & row 
  if (x>42 || y>42) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,1],steps[row,2],"sum:",sum1))
x<-f;y<-s
while(x>0 || y>0){
  y<-y-steps[row,1];x<-x-steps[row,2]  #decrease col & row
  if (x<1 || y<1) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,1]*-1,steps[row,2]*-1,"sum:",sum1))
x<-f;y<-s
#6 (down left & up right)
while(x<43 || y>0){
  y<-y-steps[row,1];x<-x+steps[row,2] # decrease col and increase row
  if (x>42 || y<1) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,1]*-1,steps[row,2],"sum:",sum1))
x<-f;y<-s
while(x>0 || y<43){
  y<-y+steps[row,1];x<-x-steps[row,2]  #increase col and decrease row
  if (x<1|| y>42) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,1],steps[row,2]*-1,"sum:",sum1))
x<-f;y<-s
#7 (down right & up left)
while(x<43 || y<43){
  y<-y+steps[row,2];x<-x+steps[row,1]# increase col & row
  if (x>42 || y>42) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,2],steps[row,1],"sum:",sum1))
x<-f;y<-s
while(x>0 || y>0){
  y<-y-steps[row,2];x<-x-steps[row,1]  #decrease col & row
  if (x<1 || y<1) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,2]*-1,steps[row,1]*-1,"sum:",sum1))
x<-f;y<-s
#8 (down left & up right)
while(x<43 || y>0){
  y<-y-steps[row,2];x<-x+steps[row,1] # decrease col and increase row
  if (x>42 || y<1) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,2]*-1,steps[row,1],"sum:",sum1))
x<-f;y<-s
while(x>0 || y<43){
  y<-y+steps[row,2];x<-x-steps[row,1]  #increase col and decrease row
  if (x<1|| y>42) {break}
  if (grid[x,y]=="A") {sum1<-sum1+1}
  if (grid[x,y]=="A") {break}
}
#print(paste("steps:",steps[row,2],steps[row,1]*-1,"sum:",sum1))
x<-f;y<-s

if (sum1>sum) {sum<-sum1}
}
print(paste("sum:",sum1))
sum1<-0
  
}
}
}



