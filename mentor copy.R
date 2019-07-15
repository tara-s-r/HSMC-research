library(readr)
AllData <- read_delim("~/Desktop/research/AllData.txt",
                      "\t", escape_double = FALSE, trim_ws = TRUE)
View(AllData)

library(tidyverse)

## compare public vs. private, 7 vs K rates over time

## create my own mean function that works even when there is missing data

my.mean<- function(x){mean(x, na.rm = TRUE)}


##  %>% create a new variable Public which True vs. False %>% group the data by Grade, Year and Public so we
## average, %>%  average all variables which are numeric%>% plot how the mean MMR vaccination rate changes over the three years +
## make separate line plot by grade and public vs private,+ add points to graph to make it easier to see

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  group_by(Grade, Year, Public)%>% summarize_if(is.numeric, my.mean)%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=MMR, col=Grade, linetype = Public))+
  geom_point(aes(y= MMR, color=Grade, shape=Public))

##  Use all the data to plot least squares line with confidence bands

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
    group_by(Grade, Public)%>%
  summarize(int=coef(lm(MMR~Year))[1], slope=coef(lm(MMR~Year))[2])

#for plotting graph. above code is for slope,etc.
# ggplot(aes(x=Year, y = MMR)) +
 # geom_smooth(aes( col=Grade, linetype = Public), method = lm)

x=1:10
y=x+rnorm(10,0,.5)
plot(x,y)
lm(y~x)->fit
summary(fit)

## repeat this for other vaccines
AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = HepA)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = HepB)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = Polio)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)

AllData%>%mutate(Public = (substring(`Facility Number`, 1,1) == "9" ))%>%
  ggplot(aes(x=Year, y = Varicella)) +
  geom_smooth(aes( col=Grade, linetype = Public), method = lm)