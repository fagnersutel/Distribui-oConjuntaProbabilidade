a = c(0,1,2,1,0,1,2,1,0)
b = c(2,3,4,3,4,5,4,5,6)
a
b
matrix(a, nrow = 3)
matrix(b, nrow = 3)

#https://districtdatalabs.silvrback.com/conditional-probability-with-r
ab = cbind(a, b)
ab = as.data.frame(ab)
ab
dice <- function(no_of_rolls=1){
  x <- sample(1:6, size=no_of_rolls, replace=TRUE)
  y <- sample(1:6, size=no_of_rolls, replace=TRUE)
  return(cbind(x,y))
}

set.seed(20485)
rolls <- as.data.frame(dice(100000))

rolls1 <- ab
library(plyr)
freq_table <- ddply(rolls, ~x, summarize,
                    y1=sum(y==1), y2=sum(y==2), y3= sum(y==3),
                    y4 = sum(y==4), y5=sum(y==5), y6=sum(y==6))
row.names(freq_table) <- paste0('x',1:6)
prob_table <- freq_table[,-1]/100000
prob_table

options(scipen=999)
amin= min(ab$a)
amin
amax = max(ab$a)
amax
bmin = min(ab$b)
bmin
bmax = max(ab$b)
bmax

freq_table <- ddply(rolls1, ~a, summarize,
                    "2"=sum(b==2), "3"= sum(b==3),
                    "4" = sum(b==4), "5"=sum(b==5), "6"=sum(b==6))

ddply(rolls1, ~a, summarize,
      "2"=sum(b==2), "3"= sum(b==3),
      "4" = sum(b==4), "5"=sum(b==5), "6"=sum(b==6))
row.names(freq_table) <- c(amin: amax)
#row.names(freq_table) <- paste0(' ',0:2)

prob_table <- freq_table[,-1]/10

prob_table

View(prob_table)

