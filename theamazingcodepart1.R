#set directory and read csv file
setwd("C:/Users/zhang/Desktop/Research")
grade_K <- read.csv("VCL_K - Vaccine Coverage Kindergarten.csv")

#look for how often middle four #s appear and see unique middle_4 combos
mutate(grade_K,middle_4 = str_sub(Facility.Number, 4, 7))%>%
  arrange(middle_4)
code.freq<-table(grade_K$middle_4)
code.freq[code.freq==1]
View(grade_K)
#set shared facility variable, view schools with same fac.id in tmp table
setwd("C:/Users/zhang/Desktop/Research")
grade_7 <- read.csv("VCL_7 - Vaccine Coverage Seventh Grade.csv")
View(grade_7)
colnames(grade_7)[1]<-"fac.id"
colnames(grade_K)[1]<-"fac.id"
inner_join(grade_K,grade_7, by="fac.id")-> grades_7_and_Kf
View(grades_7_and_K)

grades_7_and_K%>%mutate(middle_4= str_sub(fac.id, 4, 7), middle_6 = as.integer(str_sub(fac.id, 2, 7)))-> grades_7_and_K
View(grades_7_and_K)
#work on this tonight!!!
code.freq <- table(grades_7_and_K$middle_6)
names(code.freq)[code.freq == 1] -> code1.freq
grades_7_and_K%>%filter(middle_4 %in% as.numeric(code1.freq)) -> unique_grades_7_and_K
View(unique_grades_7_and_K)

tmp <- table(grades_7_and_K$middle_4)
tmp2 <- names(tmp)[tmp == 1]
grades_7_and_K%>%filter(middle_4%in%tmp2) -> grades_7_and_K.unique
View(grades_7_and_K.unique)
inner_join(grades_7_and_K.unique, district, by="middle_4") -> all_districts
View(all_districts)

#set directory and read TEA csv file. tmp has middle_4 because of grade_K
setwd("C:/Users/zhang/Desktop/Research")
district <- read.csv("district.csv")
table(table(district$middle_4))
#inner_join(grades_7_and_K,district, by="middle_4")-> data
#View(data)

#check middle_4 and school names match
table(district_district_dat_1_ $ middle_4) #checks frequency of district ID in data set
VCL_K_Vaccine_Coverage_Kindergarten %>%
  mutate(middle_4 = fac.id) %>%
  arrange(middle_4) -> VCL_K_Vaccine_Coverage_Kindergarten

grades_7_and_K %>%
  mutate(middle_4 = fac.id) %>%
  arrange(middle_4) -> grades_7_and_K


#look at correlation between CVE and the schools, look for data more specialized than just county

#other set has how manny people took test, which can be used for population
#add other data set and its codebook that it came from and find important variables  a(cnt,  wealth data, etc.)
#family size varies by demographics, change spread basde on family spread