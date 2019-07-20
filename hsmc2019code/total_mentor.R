library(readr)
AllData <- read_delim("AllData.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
View(AllData)

library(tidyverse)

## compare public vs. private, 7 vs K rates over time

## create my own mean function that works even when there is missing data

my.mean<- function(x){mean(x, na.rm = TRUE)}

## %>% create a new variable Public which True vs. False %>% group the data by Grade, Year and Public so we
## average, %>%  average all variables which are numeric%>% plot how the mean MMR vaccination rate changes over the three years +
## make separate line plot by grade and public vs private,+ add points to graph to make it easier to see

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Year, Public)%>% summarize_if(is.numeric, my.mean)%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=MMR, col=Grade, linetype = Public))+
  geom_point(aes(y= MMR, color=Grade, shape=Public))

## exclude year

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Public)%>% summarize(int=coef(lm(MMR~Year))[1], slope=coef(lm(MMR~Year))[2])


ggplot(aes(x=Year)) +
  geom_line(aes(y=MMR, col=Grade, linetype = Public))+
  geom_point(aes(y= MMR, color=Grade, shape=Public))

##  Use all the data to plot least squares line with confidence bands

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = MMR)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

## repeat this for other vaccines

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = HepA)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Public)%>%
  summarize(int=coef(lm(MMR~Year))[1], slope=coef(lm(MMR~Year))[2])

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = HepB)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = Polio)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = Varicella)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Public)%>%
  summarize(int=coef(lm(Varicella~Year))[1], slope=coef(lm(Varicella~Year))[2])

##########################

library(readr)

AllData <- read_delim("C:/Users/zhang/Desktop/Research/AllData2016_2019.txt",
                      "\t", escape_double = FALSE, col_types = cols(Meningococcal = col_double(),
                                                                    Tdap.Td = col_double()), trim_ws = TRUE)

View(AllData)

my.mean<- function(x){mean(x, na.rm = TRUE)}

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Year, Public)%>% summarize_if(is.numeric, my.mean)%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=MMR, col=Grade, linetype = Public))+
  geom_point(aes(y= MMR, color=Grade, shape=Public))

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = MMR)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  filter(Grade =="K")%>%
  group_by(Public)%>%
  summarise(int = coef(lm(MMR~Year))[1],
            slope  = coef(lm(MMR~Year))[2],
            MMR.2012.hat = predict(lm(MMR~Year), newdata = data.frame(Year = 2012))
  )

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  filter(Grade =="K", Year == 2019)%>%
  group_by(Public)%>%
  summarize(MMR7.obs.2019 = my.mean(MMR))

###  look at medians
my.median<- function(x){median(x, na.rm = TRUE)}

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Year, Public)%>% summarize_if(is.numeric, my.median)%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=MMR, col=Grade, linetype = Public))+
  geom_point(aes(y= MMR, color=Grade, shape=Public))

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Year, Public)%>% summarize_if(is.numeric, my.median)%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=HepA, col=Grade, linetype = Public))+
  geom_point(aes(y=HepA, color=Grade, shape=Public))

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Year, Public)%>% summarize_if(is.numeric, my.median)%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=HepB, col=Grade, linetype = Public))+
  geom_point(aes(y=HepB, color=Grade, shape=Public))

library(randnet)
## work: determine what the subvalues mean
dt <- BlockModel.Gen(30,300,K=3,beta=0.2,rho=0.2,simple=FALSE,power=TRUE)
A <- dt$A
deg = rep(0, 300)
for(i in 1:300)
{deg[i] = sum(dt$A[i,])}
deg
mean(deg)
dt$g
dt$A[dt$g==1, dt$g==1]
## creates a submatrix with inter-communities
dt$g == 1
## calculates the average number of interconnected edges to other members of the same community
sum(dt$A[dt$g==1, dt$g==1])/(2*sum(dt$g==1)) ## 11.07
sum(dt$A[dt$g==1, dt$g==2])/(sum(dt$g==1)) ## 3.51


dt <- BlockModel.Gen(30,300,K=3,beta=0.2,rho=0.9,simple=FALSE,power=TRUE)

### estimate vaccination rate by grade (transformation of data)
### getting more data about schools (accountability) and counties into the data set
### - determine what factors are related to vaccination rate and change
### build a stochastic blockmodel and simulate to determine herd immunity