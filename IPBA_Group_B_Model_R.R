getwd()
install.packages('car')
library('car')
setwd("C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP")
getwd()
df = read.csv("aggregated_v4.csv")
df1 = df
sum(is.na(df1))
df1[is.na(df1)] = 0
sum(is.na(df1))

df1$order_payment_type = as.factor(df1$order_payment_type)
df1$product_analytic_sub_category = as.factor(df1$product_analytic_sub_category)
df1$Premium_Product = as.factor(df1$Premium_Product)

df1$Holiday = as.factor(df1$Holiday)
df1$Salary_day = as.factor(df1$Salary_day)

4

#MinMax Scalar Normalization


normalize <- function(x, na.rm = TRUE) {
  return((x- mean(x)) /sd(x))
}

#Correlation Plot

install.packages("corrplot")
library(corrplot)

df2 = df1[,  c('gmv' , 'units', 'product_mrp', 'sla', 'Total_Investment',
               'TV_Investment', 'Digital_Investment', 'Sponsorship_Investment',
               'Content_Marketing_Investment', 'Online_Marketing_Investment',
               'Affiliates_Investment', 'SEM_Investment', 'Radio_Investment',
               'Other_Investment', 'product_procurement_sla','gmv_per_unit','Discount',
               'Value')]

df2.cor = cor(df2)

corrplot(df2.cor)


corrplot(df2.cor)


library(dplyr)


#Camera Model

df_camera = filter(df1, df1$product_analytic_sub_category ==  'Camera')


options("scipen"=999, "digits"=4)



n = nrow(df_camera)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train_camera = df_camera[trainIndex ,]

hist(train_camera$gmv)
boxplot(train_camera$gmv)


Q <- quantile(train_camera$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train_camera$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
train_camera <- subset(train_camera, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
hist(train_camera$gmv)
boxplot(train_camera$gmv)

train_camera$units = normalize(train_camera$units)
train_camera$gmv = normalize(train_camera$gmv)
train_camera$product_mrp = normalize(train_camera$product_mrp)
train_camera$sla = normalize(train_camera$sla)
train_camera$Total_Investment = normalize(train_camera$Total_Investment)
train_camera$TV_Investment = normalize(train_camera$TV_Investment)
train_camera$Digital_Investment = normalize(train_camera$Digital_Investment)
train_camera$Sponsorship_Investment = normalize(train_camera$Sponsorship_Investment)
train_camera$Content_Marketing_Investment = normalize(train_camera$Content_Marketing_Investment)
train_camera$Online_Marketing_Investment = normalize(train_camera$Online_Marketing_Investment)
train_camera$Affiliates_Investment = normalize(train_camera$Affiliates_Investment)
train_camera$SEM_Investment = normalize(train_camera$SEM_Investment)
train_camera$Radio_Investment = normalize(train_camera$Radio_Investment)
train_camera$Other_Investment = normalize(train_camera$Other_Investment)
train_camera$product_procurement_sla = normalize(train_camera$product_procurement_sla)
train_camera$gmv_per_unit = normalize(train_camera$gmv_per_unit)
train_camera$Discount = normalize(train_camera$Discount)
train_camera$Value = normalize(train_camera$Value)

hist(train_camera$gmv)

test_camera = df_camera[-trainIndex ,]


test_camera$units = normalize(test_camera$units)
test_camera$gmv = normalize(test_camera$gmv)
test_camera$product_mrp = normalize(test_camera$product_mrp)
test_camera$sla = normalize(test_camera$sla)
test_camera$Total_Investment = normalize(test_camera$Total_Investment)
test_camera$TV_Investment = normalize(test_camera$TV_Investment)
test_camera$Digital_Investment = normalize(test_camera$Digital_Investment)
test_camera$Sponsorship_Investment = normalize(test_camera$Sponsorship_Investment)
test_camera$Content_Marketing_Investment = normalize(test_camera$Content_Marketing_Investment)
test_camera$Online_Marketing_Investment = normalize(test_camera$Online_Marketing_Investment)
test_camera$Affiliates_Investment = normalize(test_camera$Affiliates_Investment)
test_camera$SEM_Investment = normalize(test_camera$SEM_Investment)
test_camera$Radio_Investment = normalize(test_camera$Radio_Investment)
test_camera$Other_Investment = normalize(test_camera$Other_Investment)
test_camera$product_procurement_sla = normalize(test_camera$product_procurement_sla)
test_camera$gmv_per_unit = normalize(test_camera$gmv_per_unit)
test_camera$Discount = normalize(test_camera$Discount)
test_camera$Value = normalize(test_camera$Value)




fit_camera = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                  sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                  Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                  Other_Investment + gmv_per_unit + Discount + Premium_Product  + Holiday + Salary_day, data = train_camera)

summary(fit_camera)


null = lm(gmv~1, data = train_camera)
full =fit_camera

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = train_camera)
summary(finalaic)



fit_camera_1 = lm(gmv ~ units + gmv_per_unit + product_mrp + Other_Investment + 
                    sla + Content_Marketing_Investment + Online_Marketing_Investment + 
                    Discount + Affiliates_Investment + Radio_Investment + TV_Investment + 
                    Total_Investment + Premium_Product + order_payment_type, data = train_camera)
summary(fit_camera_1)

vif(fit_camera_1)


#ff=boxCox(fit_camera_1)
# best_lambda=ff$x[which.max(ff$y)]
# best_lambda
# train_camera$gmv =train_camera$gmv^best_lambda






fit_camera_2 = lm(gmv ~ units  + product_mrp + Other_Investment + 
                    sla + Content_Marketing_Investment + Online_Marketing_Investment + 
                    Discount + Affiliates_Investment + Premium_Product + Radio_Investment + TV_Investment + 
                    order_payment_type, data = train_camera)
summary(fit_camera_2)

vif(fit_camera_2)

fit_camera_3 = lm(gmv ~ units  + product_mrp + Other_Investment + 
                    sla + Content_Marketing_Investment + Premium_Product + Online_Marketing_Investment + 
                    Discount, data = train_camera)
summary(fit_camera_3)




vif(fit_camera_3)
#Check Model Assumptions
# 
# fit_camera_4 = lm(gmv ~ units + product_mrp + Affiliates_Investment + Discount + 
#                     order_payment_type  + Online_Marketing_Investment + 
#                     Content_Marketing_Investment + Digital_Investment  + 
#                     Holiday, data = train_camera)
# summary(fit_camera_4)
# 
# 
# vif(fit_camera_4)
# 
# 
# 
# 
# fit_camera_5 = lm(gmv ~ units + product_mrp  + Discount + 
#                     order_payment_type  + Online_Marketing_Investment + 
#                     Digital_Investment  , data = train_camera)
# summary(fit_camera_5)
# 
# 
# vif(fit_camera_5)
# 




#Camera - Digital and Online Investments are significant for Camera





train_camera$yhat = predict(fit_camera_3)

plot(train_camera$gmv, train_camera$yhat)


library(ggplot2)
library(reshape2)

df_c1 = train_camera[c("Month","gmv","yhat")]
df_c1$Month = toString(df_c1$Month)


write.csv(df_c1,"C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP\\camera_yhat.csv", row.names = TRUE)

df_c1 = df_c1 %>% 
  group_by(Month) %>%
  summarise(total_gmv = sum(gmv), total_yhat = sum(yhat))


df_c1 <- melt(df_c1 ,  id.vars = 'Month', variable.name = 'series')

ggplot(df_c1, aes(Month, value)) +
  geom_line(aes(colour = series))



train_camera$error = train_camera$yhat - train_camera$gmv
train_camera$sqerror = train_camera$error^2
train_camera$mse = mean(train_camera$sqerror)
train_camera$sqrt_mse = sqrt(train_camera$mse)

boxplot(test_camera$gmv)


Q <- quantile(test_camera$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(test_camera$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
test_camera <- subset(test_camera, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
hist(test_camera$gmv)
boxplot(test_camera$gmv)

test_camera$yhat = predict(fit_camera_3, newdata = test_camera)


plot(test_camera$gmv, test_camera$yhat)

test_camera$error = test_camera$yhat - test_camera$gmv
test_camera$sqerror = test_camera$error^2
test_camera$mse = mean(test_camera$sqerror)
test_camera$sqrt_mse = sqrt(test_camera$mse)

#model assumption checks


res = rstandard(fit_camera_3)   # standardized residual
yhat = predict(fit_camera_3) # predicted y

# uncorrelated error
plot(res); abline(h=0); 

plot(yhat,res); abline(h=0); 

# Normality check
hist(res)
qqnorm(res);qqline(res);
#---------------------
boxplot(res)
#---------------------







#Speaker Model

df_speaker = filter(df1, df1$product_analytic_sub_category ==  'Speaker')



options("scipen"=999, "digits"=4)



n = nrow(df_speaker)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train_speaker = df_speaker[trainIndex ,]

hist(train_speaker$gmv)
boxplot(train_speaker$gmv)


Q <- quantile(train_speaker$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train_speaker$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
train_speaker <- subset(train_speaker, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
boxplot(train_speaker$gmv)

train_speaker$units = normalize(train_speaker$units)
train_speaker$gmv = normalize(train_speaker$gmv)
train_speaker$product_mrp = normalize(train_speaker$product_mrp)
train_speaker$sla = normalize(train_speaker$sla)
train_speaker$Total_Investment = normalize(train_speaker$Total_Investment)
train_speaker$TV_Investment = normalize(train_speaker$TV_Investment)
train_speaker$Digital_Investment = normalize(train_speaker$Digital_Investment)
train_speaker$Sponsorship_Investment = normalize(train_speaker$Sponsorship_Investment)
train_speaker$Content_Marketing_Investment = normalize(train_speaker$Content_Marketing_Investment)
train_speaker$Online_Marketing_Investment = normalize(train_speaker$Online_Marketing_Investment)
train_speaker$Affiliates_Investment = normalize(train_speaker$Affiliates_Investment)
train_speaker$SEM_Investment = normalize(train_speaker$SEM_Investment)
train_speaker$Radio_Investment = normalize(train_speaker$Radio_Investment)
train_speaker$Other_Investment = normalize(train_speaker$Other_Investment)
train_speaker$product_procurement_sla = normalize(train_speaker$product_procurement_sla)
train_speaker$gmv_per_unit = normalize(train_speaker$gmv_per_unit)
train_speaker$Discount = normalize(train_speaker$Discount)
train_speaker$Value = normalize(train_speaker$Value)


test_speaker = df_speaker[-trainIndex ,]



test_speaker$units = normalize(test_speaker$units)
test_speaker$gmv = normalize(test_speaker$gmv)
test_speaker$product_mrp = normalize(test_speaker$product_mrp)
test_speaker$sla = normalize(test_speaker$sla)
test_speaker$Total_Investment = normalize(test_speaker$Total_Investment)
test_speaker$TV_Investment = normalize(test_speaker$TV_Investment)
test_speaker$Digital_Investment = normalize(test_speaker$Digital_Investment)
test_speaker$Sponsorship_Investment = normalize(test_speaker$Sponsorship_Investment)
test_speaker$Content_Marketing_Investment = normalize(test_speaker$Content_Marketing_Investment)
test_speaker$Online_Marketing_Investment = normalize(test_speaker$Online_Marketing_Investment)
test_speaker$Affiliates_Investment = normalize(test_speaker$Affiliates_Investment)
test_speaker$SEM_Investment = normalize(test_speaker$SEM_Investment)
test_speaker$Radio_Investment = normalize(test_speaker$Radio_Investment)
test_speaker$Other_Investment = normalize(test_speaker$Other_Investment)
test_speaker$product_procurement_sla = normalize(test_speaker$product_procurement_sla)
test_speaker$gmv_per_unit = normalize(test_speaker$gmv_per_unit)
test_speaker$Discount = normalize(test_speaker$Discount)
test_speaker$Value = normalize(test_speaker$Value)




fit_speaker = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                   sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                   Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                   Other_Investment + gmv_per_unit + Discount  + Holiday + Salary_day, data = train_speaker)

summary(fit_speaker)


null = lm(gmv~1, data = train_speaker)
full =fit_speaker

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = train_speaker)
summary(finalaic)



fit_speaker_1 = lm(gmv ~ units + gmv_per_unit + sla + product_mrp + Discount + product_procurement_sla + 
                     Affiliates_Investment + Total_Investment + Radio_Investment + 
                     Sponsorship_Investment + Holiday + Other_Investment + TV_Investment + 
                     Digital_Investment + SEM_Investment + Online_Marketing_Investment + 
                     Content_Marketing_Investment + Salary_day, data = train_speaker)
summary(fit_speaker_1)


# 
# ff=boxCox(fit_speaker_1,family="yjPower")
# best_lambda=ff$x[which.max(ff$y)]
# best_lambda
# train_speaker$gmv =train_speaker$gmv^best_lambda


fit_speaker_0 = lm(gmv ~ units + gmv_per_unit + sla + product_mrp + Discount + product_procurement_sla + 
                     Affiliates_Investment + Total_Investment + Radio_Investment + 
                     Sponsorship_Investment + Holiday + Other_Investment + TV_Investment + 
                     Digital_Investment + SEM_Investment + Online_Marketing_Investment + 
                     Content_Marketing_Investment + Salary_day, data = train_speaker)
summary(fit_speaker_0)
vif(fit_speaker_0)

fit_speaker_2 = lm(gmv ~ units  + sla + product_mrp + Discount + product_procurement_sla + 
                     Affiliates_Investment  + Radio_Investment + 
                     Sponsorship_Investment + Holiday + Other_Investment + TV_Investment + 
                     Digital_Investment + SEM_Investment + Online_Marketing_Investment + 
                     Content_Marketing_Investment + Salary_day, data = train_speaker)
summary(fit_speaker_2)




vif(fit_speaker_2)


fit_speaker_3 = lm(gmv ~ units  + sla + product_mrp + Discount + product_procurement_sla + 
                     Radio_Investment + 
                     Sponsorship_Investment + Holiday + Other_Investment + TV_Investment + 
                     Digital_Investment + SEM_Investment  + 
                     Content_Marketing_Investment , data = train_speaker)
summary(fit_speaker_3)




vif(fit_speaker_3)


fit_speaker_4 = lm(gmv ~ units  + sla + product_mrp + Discount + product_procurement_sla + 
                     Radio_Investment + 
                     Sponsorship_Investment + Holiday + Other_Investment  + 
                     Digital_Investment + SEM_Investment  + 
                     Content_Marketing_Investment , data = train_speaker)
summary(fit_speaker_4)




vif(fit_speaker_4)


fit_speaker_5 = lm(gmv ~ units  + sla + product_mrp + Discount + product_procurement_sla + 
                     Radio_Investment + 
                     Sponsorship_Investment + Holiday   + 
                     SEM_Investment  + 
                     Content_Marketing_Investment , data = train_speaker)
summary(fit_speaker_5)




vif(fit_speaker_5)



fit_speaker_6 = lm(gmv ~ units  + sla + product_mrp + Discount + product_procurement_sla + Premium_Product + 
                     Sponsorship_Investment + Holiday   + 
                     SEM_Investment,
                   data = train_speaker)
summary(fit_speaker_6)




vif(fit_speaker_6)
#Speaker - SEM and Online marketing investments are significant for speaker




train_speaker$yhat = predict(fit_speaker_6)



df_c1 = train_speaker[c("Month","gmv","yhat")]
df_c1$Month = toString(df_c1$Month)


write.csv(df_c1,"C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP\\speaker_yhat.csv", row.names = TRUE)


plot(train_speaker$gmv, train_speaker$yhat)

train_speaker$error = train_speaker$yhat - train_speaker$gmv
train_speaker$sqerror = train_speaker$error^2
train_speaker$mse = mean(train_speaker$sqerror)
train_speaker$sqrt_mse = sqrt(train_speaker$mse)


boxplot(test_speaker$gmv)


Q <- quantile(test_speaker$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(test_speaker$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
test_speaker <- subset(test_speaker, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
hist(test_speaker$gmv)
boxplot(test_speaker$gmv)
hist(test_speaker$gmv)

test_speaker$yhat = predict(fit_speaker_6, newdata = test_speaker)

plot(test_speaker$gmv, test_speaker$yhat)

test_speaker$error = test_speaker$yhat - test_speaker$gmv
test_speaker$sqerror = test_speaker$error^2
test_speaker$mse = mean(test_speaker$sqerror)
test_speaker$sqrt_mse = sqrt(test_speaker$mse)




#Check Model Assumptions


res = rstandard(fit_speaker_6)   # standardized residual
yhat = predict(fit_speaker_6) # predicted y

# uncorrelated error
plot(res); abline(h=0); 

plot(yhat,res); abline(h=0); 

# Normality check
hist(res)
qqnorm(res);qqline(res);
#---------------------
boxplot(res)
#---------------------




#Home Audio Model

df_homeaudio = filter(df1, df1$product_analytic_sub_category ==  'HomeAudio')


options("scipen"=999, "digits"=4)



n = nrow(df_homeaudio)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train_homeaudio = df_homeaudio[trainIndex ,]

hist(train_homeaudio$gmv)
boxplot(train_homeaudio$gmv)


Q <- quantile(train_homeaudio$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train_homeaudio$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
train_homeaudio <- subset(train_homeaudio, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
boxplot(train_homeaudio$gmv)

train_homeaudio$units = normalize(train_homeaudio$units)
train_homeaudio$gmv = normalize(train_homeaudio$gmv)
train_homeaudio$product_mrp = normalize(train_homeaudio$product_mrp)
train_homeaudio$sla = normalize(train_homeaudio$sla)
train_homeaudio$Total_Investment = normalize(train_homeaudio$Total_Investment)
train_homeaudio$TV_Investment = normalize(train_homeaudio$TV_Investment)
train_homeaudio$Digital_Investment = normalize(train_homeaudio$Digital_Investment)
train_homeaudio$Sponsorship_Investment = normalize(train_homeaudio$Sponsorship_Investment)
train_homeaudio$Content_Marketing_Investment = normalize(train_homeaudio$Content_Marketing_Investment)
train_homeaudio$Online_Marketing_Investment = normalize(train_homeaudio$Online_Marketing_Investment)
train_homeaudio$Affiliates_Investment = normalize(train_homeaudio$Affiliates_Investment)
train_homeaudio$SEM_Investment = normalize(train_homeaudio$SEM_Investment)
train_homeaudio$Radio_Investment = normalize(train_homeaudio$Radio_Investment)
train_homeaudio$Other_Investment = normalize(train_homeaudio$Other_Investment)
train_homeaudio$product_procurement_sla = normalize(train_homeaudio$product_procurement_sla)
train_homeaudio$gmv_per_unit = normalize(train_homeaudio$gmv_per_unit)
train_homeaudio$Discount = normalize(train_homeaudio$Discount)
train_homeaudio$Value = normalize(train_homeaudio$Value)


test_homeaudio = df_homeaudio[-trainIndex ,]



test_homeaudio$units = normalize(test_homeaudio$units)
test_homeaudio$gmv = normalize(test_homeaudio$gmv)
test_homeaudio$product_mrp = normalize(test_homeaudio$product_mrp)
test_homeaudio$sla = normalize(test_homeaudio$sla)
test_homeaudio$Total_Investment = normalize(test_homeaudio$Total_Investment)
test_homeaudio$TV_Investment = normalize(test_homeaudio$TV_Investment)
test_homeaudio$Digital_Investment = normalize(test_homeaudio$Digital_Investment)
test_homeaudio$Sponsorship_Investment = normalize(test_homeaudio$Sponsorship_Investment)
test_homeaudio$Content_Marketing_Investment = normalize(test_homeaudio$Content_Marketing_Investment)
test_homeaudio$Online_Marketing_Investment = normalize(test_homeaudio$Online_Marketing_Investment)
test_homeaudio$Affiliates_Investment = normalize(test_homeaudio$Affiliates_Investment)
test_homeaudio$SEM_Investment = normalize(test_homeaudio$SEM_Investment)
test_homeaudio$Radio_Investment = normalize(test_homeaudio$Radio_Investment)
test_homeaudio$Other_Investment = normalize(test_homeaudio$Other_Investment)
test_homeaudio$product_procurement_sla = normalize(test_homeaudio$product_procurement_sla)
test_homeaudio$gmv_per_unit = normalize(test_homeaudio$gmv_per_unit)
test_homeaudio$Discount = normalize(test_homeaudio$Discount)
test_homeaudio$Value = normalize(test_homeaudio$Value)






fit_homeaudio = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                     sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                     Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                     Other_Investment + gmv_per_unit + Discount + Premium_Product + Holiday + Salary_day, data = train_homeaudio)

summary(fit_homeaudio)


null = lm(gmv~1, data = train_homeaudio)
full =fit_homeaudio

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = train_homeaudio)
summary(finalaic)



fit_homeaudio_1 = lm(gmv ~ units + gmv_per_unit + sla + product_procurement_sla + 
                       Affiliates_Investment + Sponsorship_Investment + SEM_Investment + 
                       product_mrp + order_payment_type + TV_Investment + Online_Marketing_Investment + Premium_Product
                       Radio_Investment + Holiday + Content_Marketing_Investment + 
                       Discount, data = train_homeaudio)
summary(fit_homeaudio_1)


# ff=boxCox(fit_homeaudio_1)
# best_lambda=ff$x[which.max(ff$y)]
# best_lambda
# train_homeaudio$gmv =train_homeaudio$gmv^best_lambda


fit_homeaudio_2 = lm(gmv ~ units + gmv_per_unit + sla + product_procurement_sla + 
                       Affiliates_Investment + Sponsorship_Investment + SEM_Investment + 
                       product_mrp + order_payment_type + TV_Investment + Online_Marketing_Investment + 
                       Radio_Investment + Premium_Product + Holiday, data = train_homeaudio)

summary(fit_homeaudio_2)




vif(fit_homeaudio_2)



fit_homeaudio_3 = lm(gmv ~ units  + sla + product_procurement_sla + 
                        Sponsorship_Investment + SEM_Investment + 
                       product_mrp + order_payment_type + TV_Investment  + Premium_Product + 
                        Holiday, data = train_homeaudio)

  summary(fit_homeaudio_3)




vif(fit_homeaudio_3)

#Home Audio - Digital marketing & Sponsorship are significant for home audio





train_homeaudio$yhat = predict(fit_homeaudio_3)



df_c1 = train_homeaudio[c("Month","gmv","yhat")]
df_c1$Month = toString(df_c1$Month)


write.csv(df_c1,"C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP\\homeaudio_yhat.csv", row.names = TRUE)



plot(train_homeaudio$gmv, train_homeaudio$yhat)

train_homeaudio$error = train_homeaudio$yhat - train_homeaudio$gmv
train_homeaudio$sqerror = train_homeaudio$error^2
train_homeaudio$mse = mean(train_homeaudio$sqerror)
train_homeaudio$sqrt_mse = sqrt(train_homeaudio$mse)




boxplot(test_homeaudio$gmv)


Q <- quantile(test_homeaudio$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(test_homeaudio$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
test_homeaudio <- subset(test_homeaudio, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
hist(test_homeaudio$gmv)
boxplot(test_homeaudio$gmv)
hist(test_homeaudio$gmv)


test_homeaudio$yhat = predict(fit_homeaudio_3, newdata = test_homeaudio)



plot(test_homeaudio$gmv, test_homeaudio$yhat)

test_homeaudio$error = test_homeaudio$yhat - test_homeaudio$gmv
test_homeaudio$sqerror = test_homeaudio$error^2
test_homeaudio$mse = mean(test_homeaudio$sqerror)
test_homeaudio$sqrt_mse = sqrt(test_homeaudio$mse)




#Check Model Assumptions


res = rstandard(fit_homeaudio_3)   # standardized residual
yhat = predict(fit_homeaudio_3) # predicted y

# uncorrelated error
plot(res); abline(h=0); 

plot(yhat,res); abline(h=0); 

# Normality check
hist(res)
qqnorm(res);qqline(res);
#---------------------
boxplot(res)
#---------------------



#Gaming Console Model

df_game = filter(df1, df1$product_analytic_sub_category ==  'GamingConsole')


options("scipen"=999, "digits"=4)

n = nrow(df_game)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train_game = df_game[trainIndex ,]

hist(train_game$gmv)
boxplot(train_game$gmv)


Q <- quantile(train_game$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train_game$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
train_game <- subset(train_game, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
boxplot(train_game$gmv)


train_game$units = normalize(train_game$units)
train_game$gmv = normalize(train_game$gmv)
train_game$product_mrp = normalize(train_game$product_mrp)
train_game$sla = normalize(train_game$sla)
train_game$Total_Investment = normalize(train_game$Total_Investment)
train_game$TV_Investment = normalize(train_game$TV_Investment)
train_game$Digital_Investment = normalize(train_game$Digital_Investment)
train_game$Sponsorship_Investment = normalize(train_game$Sponsorship_Investment)
train_game$Content_Marketing_Investment = normalize(train_game$Content_Marketing_Investment)
train_game$Online_Marketing_Investment = normalize(train_game$Online_Marketing_Investment)
train_game$Affiliates_Investment = normalize(train_game$Affiliates_Investment)
train_game$SEM_Investment = normalize(train_game$SEM_Investment)
train_game$Radio_Investment = normalize(train_game$Radio_Investment)
train_game$Other_Investment = normalize(train_game$Other_Investment)
train_game$product_procurement_sla = normalize(train_game$product_procurement_sla)
train_game$gmv_per_unit = normalize(train_game$gmv_per_unit)
train_game$Discount = normalize(train_game$Discount)
train_game$Value = normalize(train_game$Value)


test_game = df_game[-trainIndex ,]



test_game$units = normalize(test_game$units)
test_game$gmv = normalize(test_game$gmv)
test_game$product_mrp = normalize(test_game$product_mrp)
test_game$sla = normalize(test_game$sla)
test_game$Total_Investment = normalize(test_game$Total_Investment)
test_game$TV_Investment = normalize(test_game$TV_Investment)
test_game$Digital_Investment = normalize(test_game$Digital_Investment)
test_game$Sponsorship_Investment = normalize(test_game$Sponsorship_Investment)
test_game$Content_Marketing_Investment = normalize(test_game$Content_Marketing_Investment)
test_game$Online_Marketing_Investment = normalize(test_game$Online_Marketing_Investment)
test_game$Affiliates_Investment = normalize(test_game$Affiliates_Investment)
test_game$SEM_Investment = normalize(test_game$SEM_Investment)
test_game$Radio_Investment = normalize(test_game$Radio_Investment)
test_game$Other_Investment = normalize(test_game$Other_Investment)
test_game$product_procurement_sla = normalize(test_game$product_procurement_sla)
test_game$gmv_per_unit = normalize(test_game$gmv_per_unit)
test_game$Discount = normalize(test_game$Discount)
test_game$Value = normalize(test_game$Value)




fit_game = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                Other_Investment + Premium_Product + gmv_per_unit + Discount + Premium_Product  + Holiday + Salary_day, data = train_game)

summary(fit_game)


null = lm(gmv~1, data = train_game)
full =fit_game

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = train_game)
summary(finalaic)



fit_game_1 = lm(gmv ~ product_mrp + units + Premium_Product + Discount + gmv_per_unit + 
                  TV_Investment + Sponsorship_Investment + Digital_Investment + 
                  Online_Marketing_Investment + SEM_Investment + Affiliates_Investment + Premium_Product + 
                  Salary_day, data = train_game)
summary(fit_game_1)

vif(fit_game_1)

# 
# ff=boxCox(fit_game_1)
# best_lambda=ff$x[which.max(ff$y)]
# best_lambda
# train_game$gmv =train_game$gmv^best_lambda


fit_game_2 = lm(gmv ~ product_mrp + units + Premium_Product + Discount  + 
                  TV_Investment + Sponsorship_Investment + Digital_Investment + Premium_Product + 
                  Online_Marketing_Investment +
                  SEM_Investment, data = train_game)
summary(fit_game_2)



vif(fit_game_2)


fit_game_3 = lm(gmv ~ product_mrp + units + Premium_Product + Discount  + 
                  TV_Investment + Sponsorship_Investment + Digital_Investment +
                  Online_Marketing_Investment , data = train_game)
summary(fit_game_3)

vif(fit_game_3)


train_game$yhat = predict(fit_game_3)



df_c1 = train_game[c("Month","gmv","yhat")]
df_c1$Month = toString(df_c1$Month)


write.csv(df_c1,"C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP\\game_yhat.csv", row.names = TRUE)




plot(train_game$gmv, train_game$yhat)

train_game$error = train_game$yhat - train_game$gmv
train_game$sqerror = train_game$error^2
train_game$mse = mean(train_game$sqerror)
train_game$sqrt_mse = sqrt(train_game$mse)



test_game$yhat = predict(fit_game_3, newdata = test_game)

plot(test_game$gmv, test_game$yhat)

test_game$error = test_game$yhat - test_game$gmv
test_game$sqerror = test_game$error^2
test_game$mse = mean(test_game$sqerror)
test_game$sqrt_mse = sqrt(test_game$mse)



#Game Console - Sponsorship investment is significant for game

res = rstandard(fit_game_3)   # standardized residual
yhat = predict(fit_game_3) # predicted y

# uncorrelated error
plot(res); abline(h=0); 

plot(yhat,res); abline(h=0); 

# Normality check
hist(res)
qqnorm(res);qqline(res);
#---------------------
boxplot(res)
#---------------------




#Camera Accessories Model

df_cam_acc = filter(df1, df1$product_analytic_sub_category ==  'CameraAccessory')


options("scipen"=999, "digits"=4)

n = nrow(df_cam_acc)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train_cam_acc = df_cam_acc[trainIndex ,]

hist(train_cam_acc$gmv)
boxplot(train_cam_acc$gmv)


Q <- quantile(train_cam_acc$gmv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train_cam_acc$gmv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
train_cam_acc <- subset(train_cam_acc, gmv > (Q[1] - 1.5*iqr) & gmv < (Q[2]+1.5*iqr))
boxplot(train_cam_acc$gmv)


train_cam_acc$units = normalize(train_cam_acc$units)
train_cam_acc$gmv = normalize(train_cam_acc$gmv)
train_cam_acc$product_mrp = normalize(train_cam_acc$product_mrp)
train_cam_acc$sla = normalize(train_cam_acc$sla)
train_cam_acc$Total_Investment = normalize(train_cam_acc$Total_Investment)
train_cam_acc$TV_Investment = normalize(train_cam_acc$TV_Investment)
train_cam_acc$Digital_Investment = normalize(train_cam_acc$Digital_Investment)
train_cam_acc$Sponsorship_Investment = normalize(train_cam_acc$Sponsorship_Investment)
train_cam_acc$Content_Marketing_Investment = normalize(train_cam_acc$Content_Marketing_Investment)
train_cam_acc$Online_Marketing_Investment = normalize(train_cam_acc$Online_Marketing_Investment)
train_cam_acc$Affiliates_Investment = normalize(train_cam_acc$Affiliates_Investment)
train_cam_acc$SEM_Investment = normalize(train_cam_acc$SEM_Investment)
train_cam_acc$Radio_Investment = normalize(train_cam_acc$Radio_Investment)
train_cam_acc$Other_Investment = normalize(train_cam_acc$Other_Investment)
train_cam_acc$product_procurement_sla = normalize(train_cam_acc$product_procurement_sla)
train_cam_acc$gmv_per_unit = normalize(train_cam_acc$gmv_per_unit)
train_cam_acc$Discount = normalize(train_cam_acc$Discount)
train_cam_acc$Value = normalize(train_cam_acc$Value)


test_cam_acc = df_cam_acc[-trainIndex ,]




test_cam_acc$units = normalize(test_cam_acc$units)
test_cam_acc$gmv = normalize(test_cam_acc$gmv)
test_cam_acc$product_mrp = normalize(test_cam_acc$product_mrp)
test_cam_acc$sla = normalize(test_cam_acc$sla)
test_cam_acc$Total_Investment = normalize(test_cam_acc$Total_Investment)
test_cam_acc$TV_Investment = normalize(test_cam_acc$TV_Investment)
test_cam_acc$Digital_Investment = normalize(test_cam_acc$Digital_Investment)
test_cam_acc$Sponsorship_Investment = normalize(test_cam_acc$Sponsorship_Investment)
test_cam_acc$Content_Marketing_Investment = normalize(test_cam_acc$Content_Marketing_Investment)
test_cam_acc$Online_Marketing_Investment = normalize(test_cam_acc$Online_Marketing_Investment)
test_cam_acc$Affiliates_Investment = normalize(test_cam_acc$Affiliates_Investment)
test_cam_acc$SEM_Investment = normalize(test_cam_acc$SEM_Investment)
test_cam_acc$Radio_Investment = normalize(test_cam_acc$Radio_Investment)
test_cam_acc$Other_Investment = normalize(test_cam_acc$Other_Investment)
test_cam_acc$product_procurement_sla = normalize(test_cam_acc$product_procurement_sla)
test_cam_acc$gmv_per_unit = normalize(test_cam_acc$gmv_per_unit)
test_cam_acc$Discount = normalize(test_cam_acc$Discount)
test_cam_acc$Value = normalize(test_cam_acc$Value)





fit_cam_acc = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                   sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                   Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                   Other_Investment + gmv_per_unit + Discount + Premium_Product + Holiday + Salary_day, data = train_cam_acc)

summary(fit_cam_acc)


null = lm(gmv~1, data = train_cam_acc)
full =fit_cam_acc

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = train_cam_acc)
summary(finalaic)



fit_cam_acc_1 = lm(gmv ~ units + gmv_per_unit + Holiday + product_mrp + Discount + 
                     order_payment_type + Sponsorship_Investment + Content_Marketing_Investment + Premium_Product +
                     product_procurement_sla + Affiliates_Investment, data = train_cam_acc)
summary(fit_cam_acc_1)

# 
# 
# ff=boxCox(fit_cam_acc_1)
# best_lambda=ff$x[which.max(ff$y)]
# best_lambda
# train_cam_acc$gmv =train_cam_acc$gmv^best_lambda
# 

fit_cam_acc_2 = lm(gmv ~ units + gmv_per_unit + Holiday + product_mrp + Discount + 
                     order_payment_type + Sponsorship_Investment + Content_Marketing_Investment + Premium_Product + 
                     product_procurement_sla + Affiliates_Investment, data = train_cam_acc)
summary(fit_cam_acc_2)


vif(fit_cam_acc_2)

fit_cam_acc_3 = lm(gmv ~ units   + product_mrp + Discount + 
                     order_payment_type + Sponsorship_Investment + Content_Marketing_Investment + Premium_Product + 
                     product_procurement_sla , data = train_cam_acc)
summary(fit_cam_acc_3)


vif(fit_cam_acc_3)


train_cam_acc$yhat = predict(fit_cam_acc_3)



df_c1 = train_cam_acc[c("Month","gmv","yhat")]
df_c1$Month = toString(df_c1$Month)


write.csv(df_c1,"C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP\\cam_acc_yhat.csv", row.names = TRUE)






plot(train_cam_acc$gmv, train_cam_acc$yhat)

train_cam_acc$error = train_cam_acc$yhat - train_cam_acc$gmv
train_cam_acc$sqerror = train_cam_acc$error^2
train_cam_acc$mse = mean(train_cam_acc$sqerror)
train_cam_acc$sqrt_mse = sqrt(train_cam_acc$mse)



test_cam_acc$yhat = predict(fit_cam_acc_3, newdata = test_cam_acc)

plot(test_cam_acc$gmv, test_cam_acc$yhat)

test_cam_acc$error = test_cam_acc$yhat - test_cam_acc$gmv
test_cam_acc$sqerror = test_cam_acc$error^2
test_cam_acc$mse = mean(test_cam_acc$sqerror)
test_cam_acc$sqrt_mse = sqrt(test_cam_acc$mse)





#Camera Accessory - Affiliates inv is significant for Cam accesssory

res = rstandard(fit_cam_acc_3)   # standardized residual
yhat = predict(fit_cam_acc_3) # predicted y

# uncorrelated error
plot(res); abline(h=0); 

plot(yhat,res); abline(h=0); 

# Normality check
hist(res)
qqnorm(res);qqline(res);
#---------------------
boxplot(res)
#---------------------





