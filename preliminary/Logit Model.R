setwd("~/Documents/NEW MAC/Office/Brock/MBE/ECON 5P04/RESEARCH PROJECT/data")

install.packages("dynlm")
install.packages("car")
install.packages("psych")
install.packages("ggplot2")
install.packages("forecast")
install.packages("fpp2")
install.packages("stargazer")
install.packages("MASS")
library(dynlm)
library(car)
library(psych)
library("ggplot2")
library("forecast")
library("fpp2")
library("stargazer")

df<-read.csv("DATA.csv", header = TRUE, check.names=FALSE)
View(df)

EMP_Construction<- df$`EMP_Construction_[23]`
EMP_Manufacturing<- df$`EMP_Manufacturing_[31-33]`
EMP_Trade<- df$`EMP_Trade_[41-45N]`
EMP_Transportation_and_warehousing<- df$`EMP_Transportation_and_warehousing_[48-49]`
EMP_Information_and_cultural_industries<- df$`EMP_Information_and_cultural_industries_[51]`
EMP_Finance_and_insurance<- df$`EMP_Finance_and_insurance_[52]`
EMP_Professional_scientific_and_technical_services<- df$`EMP_Professional_scientific_and_technical_services_[54,541]`
EMP_Administrative_and_support_waste_management_and_remediation_services<- df$`EMP_Administrative_and_support_waste_management_and_remediation_services_[56]`

EMP_Construction<- as.factor(df$`EMP_Construction_[23]`)
EMP_Manufacturing<- as.factor(df$`EMP_Manufacturing_[31-33]`)
EMP_Trade<- as.factor(df$`EMP_Trade_[41-45N]`) 
EMP_Transportation_and_warehousing<- as.factor(df$`EMP_Transportation_and_warehousing_[48-49]`)
EMP_Information_and_cultural_industries<- as.factor(df$`EMP_Information_and_cultural_industries_[51]`)
EMP_Finance_and_insurance<- as.factor(df$`EMP_Finance_and_insurance_[52]`)
EMP_Professional_scientific_and_technical_services<- as.factor(df$`EMP_Professional_scientific_and_technical_services_[54,541]`)
EMP_Administrative_and_support_waste_management_and_remediation_services<- as.factor(df$`EMP_Administrative_and_support_waste_management_and_remediation_services_[56]`)

Wages_Construction<- df$`Wages_Construction_[23]`
Wages_Manufacturing<- df$`Wages_Manufacturing_[31-33]`
Wages_Wholesale_and_retail_trade<- df$`Wages_Wholesale_and_retail_trade_[41_44-45]`
Wages_Transportation_and_warehousing<- df$`Wages_Transportation_and_warehousing_[48-49]`
Wages_Information_culture_and_recreation<- df$`Wages_Information_culture_and_recreation_[51_71]`
Wages_Finance_insurance_real_estate_rental_and_leasing<- df$`Wages_Finance_insurance_real_estate_rental_and_leasing_[52-53]`
Wages_Professional_scientific_and_technical_services<- df$`Wages_Professional_scientific_and_technical_services_[54]`
Wages_Business_building_and_other_support_services<- df$`Wages_Business_building_and_other_support_services_[55-56]`

Wages_Construction<- as.factor(df$`Wages_Construction_[23]`)
Wages_Manufacturing<- as.factor(df$`Wages_Manufacturing_[31-33]`)
Wages_Wholesale_and_retail_trade<- as.factor(df$`Wages_Wholesale_and_retail_trade_[41_44-45]`)
Wages_Transportation_and_warehousing<- as.factor(df$`Wages_Transportation_and_warehousing_[48-49]`)
Wages_Information_culture_and_recreation<- as.factor(df$`Wages_Information_culture_and_recreation_[51_71]`)
Wages_Finance_insurance_real_estate_rental_and_leasing<- as.factor(df$`Wages_Finance_insurance_real_estate_rental_and_leasing_[52-53]`)
Wages_Professional_scientific_and_technical_services<- as.factor(df$`Wages_Professional_scientific_and_technical_services_[54]`)
Wages_Business_building_and_other_support_services<- as.factor(df$`Wages_Business_building_and_other_support_services_[55-56]`)

Construction<- df$`Construction_[23]`
Manufacturing<- df$`Manufacturing_[31-33]`
Wholesale_and_retail_trade<- df$`Trade_[41-45N]`
Transportation_and_warehousing<-	df$`Transportation_and_warehousing_[48-49]`	
Information_and_cultural_industries<- df$`Information_and_cultural_industries_[51]`
Finance_and_insurance<- df$`Finance_and_insurance_[52]`
Professional_scientific_and_technical_services<- df$`Professional_scientific_and_technical_services_[54]`
Administrative_and_support_waste_management_and_remediation_services<- df$`Administrative_and_support_waste_management_and_remediation_services_[56]`

SC_Construction<- df$`SC_Construction_(23)`
SC_Manufacturing<- df$`SC_Manufacturing_(31)`
SC_Retail_Trade<- df$`SC_Retail_Trade_(44)`
SC_Transport_and_Warehousing<- df$`SC_Transport_and_Warehousing_(48)`
SC_Information<- df$`SC_Information_(51)`
SC_Finance_and_Insurance<- df$`SC_Finance_and_Insurance_(52)`
SC_Professional_Scientific_and_Technical_Services<- df$`SC_Professional_Scientific_and_Technical_Services_(54)`
SC_Administrative_and_Support_and_Waste_Management_and_Remedial_Services<- df$`SC_Administrative_and_Support_and_Waste_Management_and_Remedial_Services_(56)`

In_Construction<- df$`In_Construction_(23)`
In_Manufacturing<- df$`In_Manufacturing_(31)`
In_Retail_Trade<- df$`In_Retail_Trade_(44)`
In_Transport_and_Warehousing<- df$`In_Transport_and_Warehousing_(48)`
In_Information<- df$`In_Information_(51)`
In_Finance_and_Insurance<- df$`In_Finance_and_Insurance_(52)`
In_Professional_Scientific_and_Technical_Services<- df$`In_Professional_Scientific_and_Technical_Services_(54)`
In_Administrative_and_Support_and_Waste_Management_and_Remedial_Services<- df$`In_Administrative_and_Support_and_Waste_Management_and_Remedial_Services_(56)`

Ex_Construction<- df$`Ex_Construction_(23)`
Ex_Manufacturing<- df$`Ex_Manufacturing_(31)`
Ex_Retail_Trade<- df$`Ex_Retail_Trade_(44)`
Ex_Transport_and_Warehousing<- df$`Ex_Transport_and_Warehousing_(48)`
Ex_Information<- df$`Ex_Information_(51)`
Ex_Finance_and_Insurance<- df$`Ex_Finance_and_Insurance_(52)`
Ex_Professional_Scientific_and_Technical_Services<- df$`Ex_Professional_Scientific_and_Technical_Services_(54)`
Ex_Administrative_and_Support_and_Waste_Management_and_Remedial_Services<- df$`Ex_Administrative_and_Support_and_Waste_Management_and_Remedial_Services_(56)`

BUSINESS_SIZE<-df$Business_Size

ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.8,0.2))
train <- df[ind==1,]
test <- df[ind==2,]

