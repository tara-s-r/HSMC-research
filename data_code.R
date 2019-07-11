#set directory and read csv file
setwd("~/Desktop/research")
grade_K <- read.csv("VCL_K - Vaccine Coverage Kindergarten.csv")

#set shared facility variable, view schools with same fac.id in all table
setwd("~/Desktop/research")
grade_7 <- read.csv("VCL_7 - Vaccine Coverage Seventh Grade.csv")
colnames(Grade_7)[1]<-"fac.id"
colnames(grade_K)[1]<-"fac.id"
inner_join(grade_K,Grade_7, by="fac.id")-> all


#look for how often middle four #s appear and see unique middle_4 combos
all%>%mutate(middle_4 = str_sub(fac.id, 4, 7),middle_6 = as.integer(str_sub(fac.id, 2, 7)))->all
code.freq<-table(all$middle_6)
names(code.freq)[code.freq==1]->code1.freq

all%>%filter(middle_4 %in% as.numeric(code1.freq))->unique.all

#set directory and read TEA csv file. all has middle_4 because of grade_K
setwd("~/Desktop/research")
district <- read.csv("district.csv")
inner_join(unique.all,district,by="middle_6")


#check middle_4 and school names match
#look at correlation between CVE and the schools, look for data more specialized than just county

#other set has how manny people took test, which can be used for population
#add other data set and its codebook that it came from and find important variables  a(cnt,  wealth data, etc.)
#family size varies by demographics, change spread basde on family spread
