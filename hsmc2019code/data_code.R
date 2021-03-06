#set directory and read csv file
setwd("~/Desktop/research")
grade_K <- read.csv("VCL_K - Vaccine Coverage Kindergarten.csv")

#set shared facility variable, view schools with same fac.id in all table
grade_7 <- read.csv("VCL_7 - Vaccine Coverage Seventh Grade.csv")
colnames(Grade_7)[1]<-"fac.id"
colnames(grade_K)[1]<-"fac.id"
inner_join(grade_K,Grade_7, by="fac.id")-> all


#create middle_6 column and find unique occurrances -> unique.all
all%>%mutate(middle_4 = str_sub(fac.id, 4, 7),middle_6 = as.integer(str_sub(fac.id, 2, 7)))->all
code.freq<-table(all$middle_6)
names(code.freq)[code.freq==1]->code1.freq
all%>%filter(middle_4 %in% as.numeric(code1.freq))->unique.all

#read TEA csv file and join with unique.all by middle_6 values
district <- read.csv("district.csv")
district%>%rename(c("middle_4"="middle_6"))->district
inner_join(unique.all,district,by="middle_6")->match

#renaming column and making its values have class of character
match <- rename(match, c('Facility Name' = 'Facility_Name'))
Facility.Name=as.character(Facility.Name)
View(match)

#trying to test that the names match between district and unique.all in match
match%>%merge(as.character(Facility.Name),as.character(Facility_Name))
code.freq<-table(all$middle_6)
names(code.freq)[code.freq==1]->code1.freq

#to do list:
#check middle_4 and school names match
#look at correlation between CVE and the schools, look for data more specialized than just county
#other set has how manny people took test, which can be used for population
#add a(cnt,  wealth data, etc.)
#family size varies by demographics, change spread basde on family spread
