setwd("~/Documents/NEW MAC/Office/Brock/MBE/ECON 5P04/RESEARCH PROJECT/data")

install.packages("plm")
install.packages("dynlm")
install.packages("psych")
library(dynlm)
library(psych)
library(plm)

df<-read.csv("panel data.csv", header = TRUE)
panel_data<-df
df<-pdata.frame(panel_data)

plm<-plm(log(df$Employment) ~ log(df$Degree_of_automation) + log(df$Wages) + log(df$OFF_Self_Classified) + log(df$OFF_Inferred) + log(df$OFF_Externally_Coded), data = df, model = "pooling")
summary(plm)
plm_within<-plm(log(df$Employment) ~ log(df$Degree_of_automation) + log(df$Wages) + log(df$OFF_Self_Classified) + log(df$OFF_Inferred) + log(df$OFF_Externally_Coded), data = df, model = "within")
summary(plm_within)
plm_random<-plm(log(df$Employment) ~ log(df$Degree_of_automation) + log(df$Wages) + log(df$OFF_Self_Classified) + log(df$OFF_Inferred) + log(df$OFF_Externally_Coded), data = df, model = "random")
summary(plm_random)
