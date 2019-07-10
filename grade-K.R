#set directory and read csv file
setwd("~/Desktop/research")
grade_K <- read.csv("VCL_K - Vaccine Coverage Kindergarten.csv")

#look for how often middle four #s appear and see unique middle_4 combos
mutate(grade_K,middle_4 = str_sub(Facility.Number, 4, 7))%>%
  arrange(middle_4)
code.freq<-table(grade_K$middle_4)
code.freq[code.freq==1]
View(grade_K)
#set shared facility variable, view schools with same fac.id in tmp table
setwd("~/Desktop/research")
grade_7 <- read.csv("VCL_7 - Vaccine Coverage Seventh Grade.csv")
colnames(Grade_7)[1]<-"fac.id"
colnames(grade_K)[1]<-"fac.id"
inner_join(grade_K,Grade_7, by="fac.id")-> tmp
View(tmp)

#set directory and read TEA csv file. tmp has middle_4 because of grade_K
setwd("~/Desktop/research")
district <- read.csv("district.csv")
table(table(district$middle_4))
#inner_join(tmp,district, by="middle_4")-> data
#View(data)

#check middle_4 and school names match


#look at correlation between CVE and the schools, look for data more specialized than just county

#other set has how manny people took test, which can be used for population
#add other data set and its codebook that it came from and find important variables  a(cnt,  wealth data, etc.)
#family size varies by demographics, change spread basde on family spread
