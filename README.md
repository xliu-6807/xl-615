title: "HW1 SURV615"
author: "Xiaoqing Liu"
date: "2024-09-04"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
install.packages('tidyverse')
library(faraway)
library(tidyverse)
```


Q1
```{r find means and variance of homeprice}
hprice$homeprice<-exp(hprice$narsp)*1000
mean(hprice$homeprice)
var(hprice$homeprice)
```
# Q1.What are the mean and the variance of homeprice? What do they mean?
# Mean is 94411.42, variance is 1583110349.


Q2
```{r confidence interval, echo=FALSE}
# 95% confidence interval of the average homeprice
t.test(hprice$homeprice)
tscore <- qt(p=0.05/2, df=n-1, lower.tail = F)

lcl <- mean(hprice$homeprice) - tscore*(sqrt(var(hprice$homeprice))/sqrt(n))
ucl <- mean(hprice$homeprice) + tscore*(sqrt(var(hprice$homeprice))/sqrt(n))
```
Q3
```{r Estimate the average homeprice}
hprice%>%
  group_by(ajwtr)%>% 
  summarize(m=mean(homeprice),
            se=std.error(homeprice))
```
Q4
```{r Test the difference in homeprice}
coast <- hprice %>% filter(ajwtr == 1) # Coastline MSAs (ajwtr = 1)
noncoast <- hprice %>% filter(ajwtr == 0) # Non-coastline MSAs (ajwtr = 0)
t.test(coast$homeprice, noncoast$homeprice, var.equal = FALSE)
```
Q5
```{r Estimate the Pearson correlation}
cor(hprice$homeprice, hprice$ypc)
```
Q6
```{r Test correlation coefficient}
cor.test(hprice$homeprice, hprice$ypc)
```
Q7
# No, although the correlation is 0.7437474.It does not mean that one variable causes the other to change. It merely shows that they move together in a systematic way.


Q8
```{r Test the normality of homeprice}
qqnorm(hprice$homeprice)
qqline(hprice$homeprice, col = "red")
shapiro.test(hprice$homeprice)
```
