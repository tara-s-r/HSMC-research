my.mean = function(x){mean(x, na.rm = T)}
library(readr)
AllData <- read_delim("C:/Users/aw22/OneDrive - Texas State University/HSMCProject/AllData2016_2019.txt","\t", escape_double = FALSE, col_types = cols(Meningococcal = col_double(),Tdap.Td = col_double()), trim_ws = TRUE)
View(AllData)

library(readxl)
district <- read_excel("district.xlsx",col_types = c("text","numeric", "numeric","numeric", "text","text", "numeric", "text",rep("numeric", 98), rep("text",4)))
View(district)

library(tidyverse)

###  Making AllData wide with MMR rates for Kindergarden side by side for 2017-2019
MMR.wide  <- AllData%>%mutate(Public = (`Facility Type` == "Public"))%>%select(Grade:County,Public,  MMR)%>%
  filter(Grade == "K", Year == 2017)%>%arrange(FacilityNum)

year = 2018
temp = AllData%>%filter(Grade == "K", Year == 2018)%>%select(FacilityNum, MMR)%>%arrange(FacilityNum)
MMR.wider <- inner_join(MMR.wide, temp, by = "FacilityNum", suffix = c(paste0(".", 2017), paste0(".", 2018)))
temp = AllData%>%filter(Grade == "K", Year == 2019)%>%select(FacilityNum, MMR)%>%
  arrange(FacilityNum)%>%rename(MMR.2019 = MMR)

MMR.widest <- inner_join(MMR.wider, temp, by = "FacilityNum")
### joining distrct data to MMR data, mainly to get district size and kinder/7th grade enrollments

colnames(district)[2] <- "Fac.Id"
inner_join(arrange(MMR.widest, Fac.Id), district, by = "Fac.Id") ->  MMR.factors

MMR.factors%>%mutate(Kinder.num.fill = ifelse(is.na(KinderNum)| KinderNum ==0,  .073*totStuD, KinderNum),
                     Seventh.num.fill = ifelse(is.na(SeventhNum),
                      .075*totStuD, SeventhNum),
                     success = round(Kinder.num.fill*MMR.2017),
                     failure = round(Kinder.num.fill*(1-MMR.2017)))-> MMR.factors

###  fitting the logistic regression model going back in time
### using 2019 and 2018 to predict 2017
glm(as.matrix(MMR.factors[,125:126])~MMR.2018+MMR.2019, family = "binomial", data = MMR.factors)-> fit
summary(fit)
coef(fit)

### selecting out and renaming key variables, rK vaccince rate for Kinder in 2019
### r1 vaccine rate for 1st in 2019 = Kinder rate in 2018,
### r2 = second grade rate in 2019 = Kinder rate in 2017
rates.by.grade = MMR.factors%>%
  select(Fac.Id:totStuD,Kinder.num.fill, Seventh.num.fill, MMR.2017:MMR.2019)%>%
  rename(rK = MMR.2019, r1 = MMR.2018, r2 = MMR.2017)

### Using model to predict Kinder rate in 2016 using kinder rates from 2017 (r2) and 2018 (r1)
r3 = predict(fit, type = "response", newdata = data.frame(MMR.2018 = rates.by.grade$r2, MMR.2019 = rates.by.grade$r1))
r4 = predict(fit, type = "response", newdata = data.frame(MMR.2018 = r3, MMR.2019 = rates.by.grade$r2))
rates.by.grade.K = cbind(rates.by.grade,r3, r4)[, c(1:8, 12:22,11, 10, 9, 23, 24) ]

##############  7th grade  #######
MMR.wide  <- AllData%>%select(Grade:County, MMR)%>%
  filter(Grade == "7", Year == 2017)%>%arrange(FacilityNum)

temp = AllData%>%filter(Grade == "7", Year == 2018)%>%select(FacilityNum, MMR)%>%arrange(FacilityNum)
MMR.wider <- inner_join(MMR.wide, temp, by = "FacilityNum", suffix = c(paste0(".", 2017), paste0(".", 2018)))

temp = AllData%>%filter(Grade == "7", Year == 2019)%>%select(FacilityNum, MMR)%>%
  arrange(FacilityNum)%>%rename(MMR.2019 = MMR)
MMR.widest <- inner_join(MMR.wider, temp, by = "FacilityNum")

colnames(district)[2] <- "Fac.Id"
inner_join(arrange(MMR.widest, Fac.Id), district, by = "Fac.Id") ->  MMR.factors

MMR.factors%>%mutate(Kinder.num.fill = ifelse(is.na(KinderNum)| KinderNum ==0,  .073*totStuD, KinderNum),
                     Seventh.num.fill = ifelse(is.na(SeventhNum)|SeventhNum == 0 ,  .075*totStuD, SeventhNum),
                     success = round(Seventh.num.fill*MMR.2019),
                     failure = round(Seventh.num.fill*(1-MMR.2019)))-> MMR.factors
###  fitting the logistic regression model forward in time
### using 7th grade rates 2017 and 2018 to predict 2019,

glm(as.matrix(MMR.factors[,124:125])~MMR.2018+MMR.2017, family = "binomial", data = MMR.factors)-> fit
summary(fit)
coef(fit)

rates.by.grade = MMR.factors%>%
  select(Fac.Id:totStuD,Kinder.num.fill, Seventh.num.fill, MMR.2017:MMR.2019)%>%
  rename(r7 = MMR.2019, r8 = MMR.2018, r9 = MMR.2017)

### Using model to predict 7th grade rate in 2020 using 7th grade rates from 2018 (r8) and 2019 (r)
r6 = predict(fit, type = "response", newdata = data.frame(MMR.2018 = rates.by.grade$r7, MMR.2017 = rates.by.grade$r8))
r5 = predict(fit, type = "response", newdata = data.frame(MMR.2018 = r6, MMR.2017 = rates.by.grade$r7))
rates.by.grade.7 = cbind(rates.by.grade,r5, r6)[, c(1:7, 11:21,22, 23, 10, 9, 8) ]

### put all together  ####
rates.by.grades = inner_join(rates.by.grade.K, rates.by.grade.7%>%select(Fac.Id, r5:r9), by="Fac.Id")

### average rates over "all" schools 
rates.by.grades%>%select(rK:r9)%>%summarise_all(my.mean)
##          rK        r1        r2        r3       r4        r5        r6        r7        r8       r9
##    0.9631823 0.9669694 0.9673432 0.9663204 0.965177 0.9860419 0.9867036 0.9864736 0.9872318 0.988505
## rates don't quite meet at grades 4 and 5