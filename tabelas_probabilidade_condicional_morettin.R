#Adaptado para o modelo da pagina 95 de Morettin, Luiz Gonzaga - Estatística Básica: probabilidade e 
#inferência| 2010 | página 65
smoke <- matrix(c(1, 2, 1, 0, 0, 1, 2, 1),ncol=4,byrow=TRUE)
smoke
colnames(smoke) <- c("X=0","X=1","X=2","X=3")
rownames(smoke) <- c("Y=0","Y=1")
smoke <- as.table(smoke)
smoke

barplot(smoke,legend=T,beside=T,main='Smoking Status by SES')
plot(smoke,main="Smoking Status By Socioeconomic Status", col=c("red", "blue", "green", "pink"))

margin.table(smoke)
margin.table(smoke,1)
margin.table(smoke,2)

smoke/margin.table(smoke)
margin.table(smoke,1)/margin.table(smoke)
margin.table(smoke,2)/margin.table(smoke)


prop.table(smoke)
prop.table(smoke,1)
prop.table(smoke,2)

summary(smoke)

expected <- as.array(margin.table(smoke,1)) %*% t(as.array(margin.table(smoke,2))) / margin.table(smoke)
expected


chi <- sum((expected - as.array(smoke))^2/expected)
chi


mosaicplot(smoke)
mosaicplot(smoke,main="Nascimentos",xlab="Primeiro filho Homem = 1",ylab="Num Meninos")
mosaicplot(smoke,sort=c(2,1))


mosaicplot(smoke,main="Nascimentos",xlab="Primeiro filho Homem = 1",ylab="Num Meninos")
mosaicplot(smoke,dir=c("v","h"))


# http://tinyheero.github.io/2016/03/20/basic-prob.html
library("ggplot2")
library("dplyr")
library("reshape2")
library("knitr")

x = c(0,1,1,1,2,2,2,3) 
y = c(0,0,0,1,0,1,1,1)
teste = cbind(x, y)
class(teste)
teste = as.data.frame(teste)
diamonds.color.cut.df <-
  teste %>%
  group_by(y, x) %>%
  summarize(n = n())

diamonds.color.cut.df %>%
  dcast(y ~ x, value.nar = "n") %>%
  kable(align = "l", format = "pandoc")

diamonds.color.cut.df %>%
  dcast(y ~ x, value.nar = "n") %>%
  kable(align = "l", format = "rst")

diamonds.color.cut.df %>%
  dcast(y ~ x, value.nar = "n") %>%
  kable(align = "l", format = "markdown")


diamonds.color.cut.prop.df <- 
  diamonds.color.cut.df %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

diamonds.color.cut.prop.df %>%
  dcast(y ~ x, value.var = "prop") %>%
  kable(align = "l", format = "rst")



color.marginal.df <- 
  diamonds.color.cut.prop.df %>%
  group_by(color) %>%
  summarize(marginal = sum(prop))

cut.marginal.df <- 
  diamonds.color.cut.prop.df %>%
  group_by(x) %>%
  summarize(marginal = sum(prop))

diamonds.color.cut.prop.df %>%
  dcast(y ~ x, value.var = "prop") %>%
  left_join(color.marginal.df, by = "x") %>%
  bind_rows(
    cut.marginal.df %>%
      mutate(color = "marginal") %>%
      dcast(y ~ x, value.var = "marginal")
  ) %>%
  kable(align = "l", format = "rst")




joint.prob <- 
  diamonds.color.cut.prop.df %>%
  filter(x == "1", y == "1") %>%
  .$prop

marg.prob <- 
  cut.marginal.df %>%
  filter(x == "1") %>%
  .$marginal

cond.prob <- joint.prob / marg.prob
cond.prob



#https://rstudio-pubs-static.s3.amazonaws.com/209289_9f9ba331cccc4e8f8aabdb9273cc76af.html

p <- matrix(c(.125,0,.250,0.125,0.125,.250,0,.125),ncol=4) ## this line creates matrix p
p  
colnames(p) <- c("X=0","X=1","X=2", "X=3")
rownames(p) <- c("Y=0","Y=1")
p

test1 <- c(1,2,3,4)  ## this line creates the object called "test1"
test1  
test2 <- matrix(c(1,2,3,4), ncol=1)  ## this line creates the object called "test2"
test2
test3 <- matrix(c(1,2,3,4),ncol=2)   ## this line creates the object called "test3"
test3
test4 <- matrix(c(1,2,3,4,5,6), ncol=2)  ## this line creates the object called "test4"
test4
test5 <- matrix(c(1,2,3,4,5,6), ncol=3)  ## this line creates the object called "test5"
test5
test6 <- matrix(c(1,2,3,4,5,6,7,8,9), ncol=3)  ## this line creates the object called "test5"
test6
test7 <- matrix(c(1,2,3,4,5,6,7,8), ncol=2)  ## this line creates the object called "test5"
test7
p
p[1,1]
p[2,3]
p[2,4]
sum(p)
py <- apply(p,1,sum) ## create marginal probabilities for y  
py                   ## display these marginal probabilities
px <- apply(p,2,sum) ## create marginal probabilities for X  
px                   ## display these marginal probabilities

p[2,2]
# p(y=0)
py[1]
# p(y=1)
py[2]
# p(x=1 | y=1)
p_x1_y1 <- p[2,2]/py[2]  ## computes conditional probability P(X=5|Y=5)
p_x1_y1  
# p(x=3 | y=1)
p_x3_y1 <- p[2,4]/py[2]  ## computes conditional probability P(X=5|Y=5)
p_x3_y1  # (1/8) / (1/2) = 0.25

# p(x=0 | y=1)
p_x0_y1 <- p[2,1]/px[2]
p_x0_y1
# p(x=1 | y=1)
p_x1_y1 <- p[2,2]/px[2]
p_x1_y1
# p(x=2 | y=1)
p_x2_y1 <- p[2,3]/px[2]
p_x2_y1
# p(x=3 | y=1)
p_x3_y1 <- p[2,4]/px[2]
p_x3_y1
# p(x|y=1)
p_x_y1 <- c(p_x0_y1,p_x1_y1,p_x2_y1,p_x3_y1)
p_x_y1
sum(p_x_y1)
py[1]
py[2]

# p(x=2 | y= 0)
p_x2_y0 = p[1,3]/py[2]
p_x2_y0
# p(x=2 | y= 1)
p_x2_y1 = p[2,3]/py[2]
p_x2_y1

x<- c(0,1,2,3)
y<- c(0,1)
EX  <- sum(px*x)   ## expectation of X
EX
EX2 <- sum(px*x^2) ## expectation of X^2
EX2
# E(X | Y = 1) 
EX_Y1 <- sum(p_x_y1*x)
EX_Y1
