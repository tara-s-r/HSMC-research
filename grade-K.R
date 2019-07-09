setwd("~/Desktop/research")
grade_K <- read.csv("VCL_K - Vaccine Coverage Kindergarten.csv")
grade_K <- grade_K %>% arrange(Facility.Number)
View(grade_K)