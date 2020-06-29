rm(list=ls())

#libraries
library(plm)
library(ggplot2)
library(lmtest)
library(stargazer)
library(dplyr)
library(haven)
library(expss)
library(sandwich)
library(lmtest)

library(clubSandwich)

data_permit_FN <- read.csv("C:/Users/kuffe/OneDrive/Formation/UniFR/Thesis/Data/FNpermit.csv", header = TRUE)
data_permit_FN [c(4,9,11,13,16,18,20,21,22,23,24,25)]<- list(NULL)
library(readxl)
data_canton <- read_excel("C:/Users/kuffe/OneDrive/Formation/UniFR/Thesis/Data/Control.xlsx")

##Data selection + organisation
#creating a new variable to help in descriptive statistics
data_permit_FN <- mutate(data_permit_FN, treatmentgroup = ifelse(romandy==1&fr_sp==1, 1, ifelse(romandy==1&fr_sp==0, 2, ifelse(romandy==0&fr_sp==1, 3, 4))))
data_permit_FN <- mutate(data_permit_FN, stay_1 = ifelse(entry==year, 1,0), stay_2= ifelse((entry+1)==year, 1,0), stay_5= ifelse((entry+4)==year, 1,0), stay_7= ifelse((entry+6)==year, 1,0))
data_permit_FN%>% tab_cells(age==18) %>%  tab_cols(entry==2011, entry==2012, entry==2013, entry==2014, entry==2015, entry==2016, entry==2017, entry==2018) %>%  tab_stat_cases() %>%  tab_pivot()
data_subFN <- subset(data_permit_FN, !(canton  %in% c("BE", "FR", "VS")) & entry>=2000)
data_sub <- subset(data_permit_FN, age>=18 & age<=65 & !(canton  %in% c("BE", "FR", "VS")) & entry>=2000 & statut=="F")

#create panel data
data <- pdata.frame(data_sub, index=c("id", "year"))
data_canton<- subset(data_canton,!(canton  %in% c("BE", "FR", "VS")))
data_sub <- inner_join(data_sub, data_canton, index=c(year))
data2 <- pdata.frame(data_sub, index=c("id", "year"))
pdim(data)
attach(data)
summary(data)

##Empirical strategy
#Table 1
data %>%
  tab_cells(fr_used==0, fr_used==1, fr_used==2, fr_used ==3) %>%
  tab_cols(romandy) %>%
  tab_stat_sum() %>% tab_stat_rpct() %>% tab_pivot()

#Table 2 https://github.com/dcomtois/summarytools/blob/master/README.md  #takes time but worth watching : view(dfSummary(data))
library(summarytools)
view(stby(data, FUN = descr, INDICES = data$treatmentgroup, stats = c("mean", "sd", "min", "max"), transpose = TRUE))
view(dfSummary(data))
count(data, canton_diff, romandy)
#Table 3
#data_canton <- subset(data_canton, year==2011)
view(stby(data_canton, FUN = descr, INDICES = data_canton$romandy, stats = c("mean", "sd", "min", "max"), transpose = TRUE))
summary(data_canton)
#figure ???
ggplot(data_canton, aes(year, unempl,  color=factor(romandy))) +
  stat_summary(geom ="line") +  ylab("Gainful Activity Rate") + 
  geom_point(aes(year, unempl))+
  theme_minimal()

#figure 4
ggplot(data_subf, aes(year, gain_act, color=factor(treatmentgroup))) +
  stat_summary(geom = 'line') +  ylab("Gainful Activity Rate") + 
  scale_color_manual(name="Treatment Group", labels = c("Fr in Romandy", "Non Fr in Romandy", "Fr in other cantons", "Non Fr in other cantons"),
                     values = c("1"="blue", "2"="red", "3"="green", "4"="orange"))+ theme_minimal()
## emp anexe
#table a.2
data%>%
  tab_cells(year) %>%
  tab_cols(entry==2011, entry==2012, entry==2013, entry==2014, entry==2015, entry==2016, entry==2017, entry==2018) %>%
  tab_stat_cases() %>%  tab_pivot()
count(data_sub, year)
count(subset(data_subf, !(canton  %in% c("BE", "FR", "VS"))), age==18, age==66)
#table a.4
data %>%
  tab_cells(year) %>% 
  tab_cols(treatmentgroup) %>%
  tab_stat_rpct() %>% tab_stat_cases() %>% tab_pivot()
#table ?

data %>%
  tab_subgroup(gain_act==1) %>%
  tab_cells(sector==0, sector==1, sector==2, sector==3, stay_1, stay_5, stay_7, gain_act) %>%
  tab_cols(treatmentgroup) %>%
  tab_stat_mean() %>%  tab_stat_sum() %>%  tab_pivot()

##5.5 Random assignment ##
############################
data_subFN <- mutate(data_subFN, statfr = ifelse(statut=="N" &fr_sp==1, "N and Fr", ifelse(statut=="F"&fr_sp==1, "F and Fr", ifelse(statut=="N"&fr_sp==0, "N and non Fr", "F and non Fr"))))
data_subFN <- mutate(data_subFN, dummy_p = ifelse(statut=="F", 1, 0))
data_subFN <- mutate(data_subFN, permit=ifelse(statut=="F", 1, 0))
data_table<-subset(data_subFN, year==entry)
dataFN <- pdata.frame(data_subFN, index=c("id", "year"))
#figure 6
ggplot(data_table, aes(entry, romandy,  color=statfr)) +
  stat_summary(geom = 'line') +  ylab("Ratio in Romandy") +
  scale_color_manual(name="Permit & language Group", labels = c("Permit F and Fr", "Permit F and non Fr", "Permit N and FR", "Permit N and non FR"),
                     values = c("N and Fr"="blue", "F and Fr"="red", "N and non Fr"="green", "F and non Fr"="orange"))+theme_minimal()

ggplot(data_table, aes(entry, romandy,  color=xxx))+
  stat_summary(geom = 'line') +  ylab("Ratio in Romandy")
#figure 5
ggplot(data_subFN, aes(year, romandy,  color=statfr)) +
  stat_summary(geom = 'line') +  ylab("Ratio in Romandy") +
  scale_color_manual(name="Permit & language Group", labels = c("Permit F and Fr", "Permit F and non Fr", "Permit N and FR", "Permit N and non FR"),
                     values = c("N and Fr"="blue", "F and Fr"="red", "N and non Fr"="green", "F and non Fr"="orange"))+theme_minimal()
#table a.5
data_table %>%
  tab_cells(year, romandy) %>% 
  tab_cols(statfr) %>%
  tab_stat_rpct() %>% tab_stat_cases() %>% tab_pivot()
data_sub %>%
  tab_cells(year, romandy) %>% 
  tab_cols(treatmentgroup) %>%
  tab_stat_rpct() %>% tab_stat_cases() %>% tab_pivot()

#table a.x
data_table <- subset(data_subFN, fr_sp==1)
data_table%>%
  tab_cells(country) %>%
  tab_cols(romandy==1 & statut=="N", romandy==1 & statut=="F", romandy==0 & statut=="N", romandy==0 & statut=="F") %>%
  tab_stat_cases() %>%  tab_pivot() %>%  drop_rc()  
#balance test
t.test(romandy ~ fr_sp, data = data)
t.test(romandy ~ fr_sp, data = dataFN)
pooltest(romandy~fr_sp, data=dataFN, model="within")
t.test(fr_sp ~ romandy, data = data)
  #table 4
permit0<-lm(romandy~fr_sp+year, data=dataFN[dataFN$stay_1==1,])
permit1<-lm(romandy~fr_sp+year, data=dataFN[dataFN$stay_2==1,])
permit01<-lm(romandy~fr_sp+gain_act+gender+year+age+statut, data=dataFN[dataFN$stay_1==1,])
permit11<-lm(romandy~fr_sp+gain_act+gender+year+age+statut, data=dataFN[dataFN$stay_2==1,])
stargazer(permit0, permit1, permit01, permit11,  type="text", out="models.htm")

  #did - permit F 
permit0<-plm(permit~fr_sp*romandy+year,effect = "time",index=c("canton", "year"), data=dataFN[dataFN$stay_1==1,])
permit1<-plm(permit~fr_sp*romandy+year,effect = "time",index=c("canton", "year"), data=dataFN[dataFN$stay_2==1,])
permit2<-plm(permit~fr_sp*romandy+year,effect = "time",index=c("canton", "year"), data=dataFN[dataFN$stay_5==1,])
permit3<-plm(permit~fr_sp*romandy+year,effect = "time",index=c("canton", "year"), data=dataFN[dataFN$stay_7==1,])
permit01<-plm(permit~fr_sp*romandy+gain_act+gender+year+age,effect = "time", index=c("canton", "year"),data=dataFN[dataFN$stay_1==1,])
permit11<-plm(permit~fr_sp*romandy+gain_act+gender+year+age,effect = "time",index=c("canton", "year"), data=dataFN[dataFN$stay_2==1,])
permit21<-plm(permit~fr_sp*romandy+gain_act+gender+year+age,effect = "time", index=c("canton", "year"),data=dataFN[dataFN$stay_5==1,])
permit31<-plm(permit~fr_sp*romandy+gain_act+gender+year+age, effect = "time",index=c("canton", "year"),data=dataFN[dataFN$stay_7==1,])
stargazer(permit0, permit1, permit2, permit3, permit01, permit11, permit21, permit31,  type="text", out="models.htm")
permit0<-plm(permit~fr_sp*romandy+year,effect = "time",index=c("canton", "year"), data=dataFN)
permit01<-plm(permit~fr_sp*romandy+gender+year+age,effect = "time", index=c("canton", "year"),data=dataFN)
stargazer(permit0, permit01, type="text", out="models.htm")

############
##results ##
############
znp <- pvcm(gain_act~fr_sp*romandy, data=data, model="within")
zplm <- plm(gain_act~fr_sp*romandy, data=data, model="within")
pooltest(zplm, znp)

#table 5 OLS
did1 = plm(gain_act~fr_sp*romandy, effect = "time",index=c("id", "year"), data=data2)
did2 = plm(gain_act~fr_sp*romandy+gender + age, effect = "time",index=c("id", "year"), data=data2)
did3 = plm(gain_act~fr_sp*romandy+gender + age + share_for+unempl,effect = "time",index=c("id", "year"), data=data2)
did4 = plm(gain_act~fr_sp*romandy+gender + age + share_for+unempl+gdp_ln,effect = "time",index=c("id", "year"), data=data2)
stargazer(did1, did2, did3, did4, type="text", out="models.htm")

#table 6
didfr1 = plm(gain_act~factor(fr_used)*romandy, effect = "time",index=c("id", "year"), data=data2)
didfr2 = plm(gain_act~factor(fr_used)*romandy+gender + age, effect = "time",index=c("id", "year"), data=data2)
didfr3 = plm(gain_act~factor(fr_used)*romandy+gender + age + share_for+unempl,effect = "time",index=c("id", "year"), data=data2)
didfr4 = plm(gain_act~factor(fr_used)*romandy+gender + age + share_for+unempl+gdp_ln,effect = "time",index=c("id", "year"), data=data2)
stargazer(didfr1, didfr2, didfr3, type="text", out="models.htm")

#table a
datatest = subset(data_sub,year<2017)
datatest <- pdata.frame(datatest, index=c("id", "year"))
did21= plm(gain_act~fr_sp*romandy+gender + age ,effect = "time",index=c("id", "year"), data=datatest)
did31 = plm(gain_act~fr_sp*romandy+gender + age + share_for+unempl,effect = "time",index=c("id", "year"), data=datatest)
did41 = plm(gain_act~fr_sp*romandy+gender + age + share_for+unempl+gdp_ln,effect = "time",index=c("id", "year"), data=datatest)
stargazer(did21, did31, did41, type="text", out="models.htm")

#table a
didfr21 = plm(gain_act~factor(fr_used)*romandy+gender + age ,effect = "time",index=c("id", "year"), data=datatest)
didfr31 = plm(gain_act~factor(fr_used)*romandy+gender + age + share_for+unempl,effect = "time",index=c("id", "year"), data=datatest)
didfr41 = plm(gain_act~factor(fr_used)*romandy+gender + age + share_for+unempl+gdp_ln,effect = "time",index=c("id", "year"), data=datatest)
stargazer(didfr21, didfr31, didfr41, type="text", out="models.htm")

#figure 6 did
b1<- coef(did1)[[1]]
b2<- coef(did1)[["fr_sp"]]
b3<- coef(did1)[["romandy"]]
delta <- coef(did1)[["fr_sp:romandy"]]
C <- b1+b2+b3+delta
E <- b1+b3
B <- b1+b2
A <- b1
D <- E+(B-A)
plot(1, type="n", xlab="Romandy", ylab="Gainful activity rate", xaxt="n", xlim=c(-0.01, 1.01), ylim=c(-0.01, 0.51))
segments(x0=0, y0=A, x1=1, y1=E, lty=1, col=2)#control
segments(x0=0, y0=B, x1=1, y1=C, lty=3, col=3)#treated
segments(x0=0, y0=B, x1=1, y1=D, lty=4, col=4)#counterfactual
legend("topright", legend=c("Control", "Treated", "Counterfactual"), lty=c(1,3,4), col=c(2,3,4))
axis(side=1, at=c(0,1), labels=NULL)




#Table 6 stay 
didstay2 = plm(gain_act ~  (fr_sp)*(romandy)+factor(year),effect = "time",index=c("id", "year"),  data=subset(data2, stay_2==1))
didstay5 = plm(gain_act ~  (fr_sp)*(romandy)+factor(year),effect = "time",index=c("id", "year"),  data=subset(data2, stay_5==1))
didstay7 = plm(gain_act ~  (fr_sp)*(romandy)+factor(year), effect = "time",index=c("id", "year"), data=subset(data2, stay_7==1))
didstay20 = plm(gain_act ~  (fr_sp)*(romandy)+factor(year)+gender+age+share_for+unempl, effect = "time",index=c("id", "year"), data=subset(data2, stay_2==1))
didstay50 = plm(gain_act ~  (fr_sp)*(romandy)+factor(year)+gender+age+share_for+unempl,effect = "time",index=c("id", "year"),  data=subset(data2, stay_5==1))
didstay70 = plm(gain_act ~  (fr_sp)*(romandy)+factor(year)+gender+age+share_for+unempl,effect = "time",index=c("id", "year"),  data=subset(data2, stay_7==1))
stargazer(didstay2, didstay5, didstay7, didstay20, didstay50, didstay70, type="text", out="models.htm")
#table a.8 stay
didstay21 = plm(gain_act ~  (fr_sp)*(romandy)+gender+age+share_for+unempl+gdp_ln,effect = "time",index=c("id", "year"),  data=subset(data2, stay_2==1))
didstay51 = plm(gain_act ~  (fr_sp)*(romandy)+gender+age+share_for+unempl+gdp_ln,effect = "time",index=c("id", "year"),  data=subset(data2, stay_5==1))
didstay71 = plm(gain_act ~  (fr_sp)*(romandy)+gender+age+share_for+unempl+gdp_ln,effect = "time",index=c("id", "year"),  data=subset(data2, stay_7==1))
stargazer(didstay21, didstay51, didstay71, type="text", out="models.htm")
