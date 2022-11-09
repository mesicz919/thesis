setwd("~/Documents/NEW MAC/Office/Brock/MBE/ECON 5P04/RESEARCH PROJECT/data")

install.packages("dynlm")
install.packages("car")
install.packages("psych")
install.packages("ggplot2")
install.packages("forecast")
install.packages("fpp2")
install.packages("stargazer")
install.packages("MASS")
install.packages("foreign")
install.packages("plm")
library(dynlm)
library(psych)
library(plm)
library(dynlm)
library(car)
library(psych)
library("ggplot2")
library("forecast")
library("fpp2")
library("stargazer")
library("foreign")
library(plm)

df<-read.csv("DATA.csv", header = TRUE, check.names=FALSE)
View(df)

EMPLOYMENT<-cbind(df$`EMP_Construction_[23]`,	df$`EMP_Manufacturing_[31-33]`,	df$`EMP_Trade_[41-45N]`,	df$`EMP_Transportation_and_warehousing_[48-49]`,	df$`EMP_Information_and_cultural_industries_[51]`,	df$`EMP_Finance_and_insurance_[52]`,	df$`EMP_Professional_scientific_and_technical_services_[54,541]`,
                  df$`EMP_Administrative_and_support_waste_management_and_remediation_services_[56]`)
  
WAGES<-cbind(df$`Wages_Construction_[23]`,	df$`Wages_Manufacturing_[31-33]`,	df$`Wages_Wholesale_and_retail_trade_[41_44-45]`,	df$`Wages_Transportation_and_warehousing_[48-49]`,	df$`Wages_Information_culture_and_recreation_[51_71]`,	df$`Wages_Finance_insurance_real_estate_rental_and_leasing_[52-53]`,
             df$`Wages_Professional_scientific_and_technical_services_[54]`,	df$`Wages_Business_building_and_other_support_services_[55-56]`)

AUTOMATION<-cbind(df$`Construction_[23]`,	df$`Manufacturing_[31-33]`,	df$`Trade_[41-45N]`,	df$`Transportation_and_warehousing_[48-49]`,	df$`Information_and_cultural_industries_[51]`,	df$`Finance_and_insurance_[52]`,	df$`Professional_scientific_and_technical_services_[54]`,
                  df$`Administrative_and_support_waste_management_and_remediation_services_[56]`)

OFFSHORE_SC<-cbind(df$`SC_Construction_(23)`,	df$`SC_Manufacturing_(31)`,	df$`SC_Retail_Trade_(44)`,	df$`SC_Transport_and_Warehousing_(48)`,	df$`SC_Information_(51)`,	df$`SC_Finance_and_Insurance_(52)`,	df$`SC_Professional_Scientific_and_Technical_Services_(54)`,
                   df$`SC_Administrative_and_Support_and_Waste_Management_and_Remedial_Services_(56)`)
OFFSHORE_In<-cbind(df$`In_Construction_(23)`,	df$`In_Manufacturing_(31)`,	df$`In_Retail_Trade_(44)`,	df$`In_Transport_and_Warehousing_(48)`,	df$`In_Information_(51)`,	df$`In_Finance_and_Insurance_(52)`,	df$`In_Professional_Scientific_and_Technical_Services_(54)`,
                   df$`In_Administrative_and_Support_and_Waste_Management_and_Remedial_Services_(56)`)
OFFSHORE_Ex<-cbind(df$`Ex_Construction_(23)`,	df$`Ex_Manufacturing_(31)`,	df$`Ex_Retail_Trade_(44)`,	df$`Ex_Transport_and_Warehousing_(48)`,	df$`Ex_Information_(51)`,	df$`Ex_Finance_and_Insurance_(52)`,	df$`Ex_Professional_Scientific_and_Technical_Services_(54)`,
                   df$`Ex_Administrative_and_Support_and_Waste_Management_and_Remedial_Services_(56)`)

BUSINESS_SIZE<-df$Business_Size


model <-lm(EMPLOYMENT ~ AUTOMATION + WAGES + OFFSHORE_SC + OFFSHORE_In + OFFSHORE_Ex + BUSINESS_SIZE) 
summary(model)
