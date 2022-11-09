setwd("~/Documents/NEW MAC/Office/Brock/MBE/MRP/Data")

install.packages("dynlm")
install.packages("plm")
install.packages("psych")
install.packages("car")
install.packages("forecast")
install.packages("fpp2")
install.packages("lmtest")
install.packages("stargazer")
install.packages("strucchange")
library(dynlm)
library(plm)
library(psych)
library(car)
library(forecast)
library(fpp2)
library(lmtest)
library(stargazer)
library(strucchange)

### REGRESSIONS AT OCCUPATIONAL LEVEL ###

df<-read.csv("occ_dataset_2.csv", header = TRUE)
data_frame_1<-pdata.frame(df, index=c("occ_fixed","geo_year"))
View(data_frame_1)

#reg w/OLS model
pooled_1<-plm(df$hours_worked ~ df$wage_rate_2010, data = df, model = "pooling")
summary(pooled_1)
stargazer(pooled_1)
pooled_2<-plm(df$hours_worked ~ df$rti, data = df, model = "pooling")
summary(pooled_2)
stargazer(pooled_2)
pooled_3<-plm(df$hours_worked ~ df$off, data = df, model = "pooling")
summary(pooled_3)
stargazer(pooled_3)
pooled_4<-plm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off, data = df, model = "pooling")
summary(pooled_4)
stargazer(pooled_4)

#reg w/ LSDV model
lsdv_1<-lm(df$hours_worked ~ df$wage_rate_2010 + factor(year) + factor(geo), data = df)
summary(lsdv_1)
stargazer(lsdv_1)
lsdv_2<-lm(df$hours_worked ~ df$rti + factor(year) + factor(geo), data = df)
summary(lsdv_2)
stargazer(lsdv_2)
lsdv_3<-lm(df$hours_worked ~ df$off + factor(year) + factor(geo), data = df)
summary(lsdv_3)
stargazer(lsdv_3)
lsdv_4<-lm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off + factor(year) + factor(geo), data = df)
summary(lsdv_4)
stargazer(lsdv_4)
lsdv_5<-lm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off + factor(year) + factor(geo) + factor(geo_year), data = df)
summary(lsdv_5)
stargazer(lsdv_5)

#reg w/ Within model
within_1<-plm(df$hours_worked ~ df$wage_rate_2010 + df$time_trend, index = c("geo_year"), model = "within", data = df)
summary(within_1)
stargazer(within_1)
within_2<-plm(df$hours_worked ~ df$rti + df$time_trend, index = c("geo_year"), model = "within", data = df)
summary(within_2)
stargazer(within_2)
within_3<-plm(df$hours_worked ~ df$off + df$off + df$time_trend, index = c("geo_year"), model = "within", data = df)
summary(within_3)
stargazer(within_3)
within_4<-plm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off + df$time_trend, index = c("geo_year"), model = "within", data = df)
summary(within_4)
stargazer(within_4)

fixef(within_1)

sctest(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off +df$time_trend, type = "Chow")
phtest(within_2,random_2)

### REGRESSIONS AT INDUSTRIAL LEVEL ###

df<-read.csv("ind_dataset.csv", header = TRUE)
data_frame_3<-pdata.frame(df, index=c("ind_fixed","geo_year"))
View(data_frame_3)

#POLS model
pooled_3<-plm(df$hours_worked ~ df$rti + df$off, data = df, model = "pooling")
summary(pooled_3)

#reg w/ LSDV model
lsdv_3<-lm(df$hours_worked ~ df$rti + df$off + factor(year) + factor(geo), data = df)
summary(lsdv_3)

#reg w/ Within model
within_3<-plm(df$hours_worked ~ df$rti + df$off, index = c("geo_year"), model = "within", data = df)
summary(within_3)
fixef(within_3)

# w/ WAGES

df<-read.csv("ind_dataset_2.csv", header = TRUE)
data_frame_4<- pdata.frame(df, index=c("ind_fixed","geo_year"))
View(data_frame_4)

#POLS model
pooled_4<-plm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off, data = df, model = "pooling")
summary(pooled_4)
stargazer(pooled_4)

#reg w/ LSDV model
lsdv_4<-lm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off + factor(year) + factor(geo), data = df)
summary(lsdv_4)
stargazer(lsdv_4)

#reg w/ Within model
within_4<-plm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off + df$time_trend, index = c("geo_year"), model = "within", effect = "twoways", data = df)
summary(within_4)
stargazer(within_4)
random_4<-plm(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off + df$time_trend, index = c("geo_year"), model = "random", effect = "twoways", data = df)
summary(random_4)
sctest(df$hours_worked ~ df$wage_rate_2010 + df$rti + df$off +df$time_trend, type = "Chow")
phtest(within_4,random_4)

### TEST STATISTICAL SIGNIFICANCE IN FIGURES ###

df<-read.csv("std_error.csv", header = TRUE)
View(df)

reg1<-lm(df$Hours_worked ~ df$RTI, data = df)
summary(reg1)
reg2<-lm(df$Hours_worked ~ df$Offshore, data = df)
summary(reg2)
reg3<-lm(df$Wage_rate ~ df$RTI, data = df)
summary(reg3)
reg4<-lm(df$Wage_rate ~ df$Offshore, data = df)
summary(reg4)

reg5<-lm(df$Employment_share ~ df$RTI, data = df)
summary(reg5)
reg6<-lm(df$Employment_share ~ df$Offshore, data = df)
summary(reg6)
reg7<-lm(df$Relative_wage ~ df$RTI, data = df)
summary(reg7)
reg8<-lm(df$Relative_wage ~ df$Offshore, data = df)
summary(reg8)

### EMPIRICALLY VERIFY FIGURES ###

emp_reg1<-lm(df$Hours_worked ~ df$RTI + df$Occupation_type, data = df)
summary(emp_reg1)
stargazer(emp_reg1)
emp_reg2<-lm(df$Hours_worked ~ df$Offshore + df$Occupation_type, data = df)
summary(emp_reg2)
stargazer(emp_reg2)
emp_reg3<-lm(df$Hours_worked ~ df$RTI + df$Offshore + df$Occupation_type, data = df)
summary(emp_reg3)
stargazer(emp_reg3)

emp_reg4<-lm(df$Employment_share ~ df$RTI + df$Occupation_type, data = df)
summary(emp_reg4)
stargazer(emp_reg4)
emp_reg5<-lm(df$Employment_share ~ df$Offshore + df$Occupation_type, data = df)
summary(emp_reg5)
stargazer(emp_reg5)
emp_reg6<-lm(df$Employment_share ~ df$RTI + df$Offshore + df$Occupation_type, data = df)
summary(emp_reg6)
stargazer(emp_reg6)
