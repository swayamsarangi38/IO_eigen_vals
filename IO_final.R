####IO Term paper
#### Installing libraries
install.packages("readr")
library(readr)
require(tidyverse)
library("xlsx")
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(xtable)



rm(list=ls())



setwd("D:/UoU/Semester 5/Economic doctrines/Term paper")
####Data inut files: each subscript number denotes the number of sectors. All files are from the Total Requirements matrix USA 2012
io_15 <- read.csv("2012_15.csv",row.names=1, header=T)
io_72 <- read.csv("2012_72.csv",row.names=1, header=T)
io_405 <- read.csv("2012_405.csv",row.names=1, header=T)




io15<-io_15[-16,]

#####Evaluating the A Matrix from the L matrix
A_15<-diag(nrow(io_15))-solve(io_15)
A_72<-diag(nrow(io_72))-solve(io_72)
A_405 <-diag(nrow(io_405))-solve(io_405)
####### Computing eigen values of total requirements matrixes for 2012 for 3 aggregations- 15 72 and 405 subsectors

eigen_15 <- Re(eigen(A_15)$values)
eigen_72 <- Re(eigen(A_72)$values)
eigen_405<- Re(eigen(A_405)$values)
####Writing to excel files. Other graphs are made in excel file.
write.csv(eigen_15,"D:/UoU/Semester 5/Economic doctrines/Term paper\\eigen_15.csv", row.names = FALSE)
write.csv(eigen_72,"D:/UoU/Semester 5/Economic doctrines/Term paper\\eigen_72.csv", row.names = FALSE)
write.csv(eigen_405,"D:/UoU/Semester 5/Economic doctrines/Term paper\\eigen_405.csv", row.names = FALSE)

#####Plotting eigenvalues
plot(eigen_15,type="l")
plot(eigen_72,type="l")
plot(eigen_405,type="l")


###Plotting mean of eigen values according to Brody's conjecture
A_15_star <- 2*(A_15)/15
mean_A_15 <- mean(A_15_star)
mean_A_15
