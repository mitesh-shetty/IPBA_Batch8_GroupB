getwd()
install.packages('car')
library('car')
setwd("C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP")
getwd()
df = read.csv("aggregated_v3.csv")
df1 = df
sum(is.na(df1))
df1[is.na(df1)] = 0
sum(is.na(df1))

df1$order_payment_type = as.factor(df1$order_payment_type)
df1$product_analytic_sub_category = as.factor(df1$product_analytic_sub_category)
df1$Premium_Product = as.factor(df1$Premium_Product)

df1$Holiday = as.factor(df1$Holiday)
df1$Salary_day = as.factor(df1$Salary_day)


#MinMax Scalar Normalization

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

df1$units = normalize(df1$units)
df1$gmv = normalize(df1$gmv)
df1$product_mrp = normalize(df1$product_mrp)
df1$sla = normalize(df1$sla)
df1$Total_Investment = normalize(df1$Total_Investment)
df1$TV_Investment = normalize(df1$TV_Investment)
df1$Digital_Investment = normalize(df1$Digital_Investment)
df1$Sponsorship_Investment = normalize(df1$Sponsorship_Investment)
df1$Content_Marketing_Investment = normalize(df1$Content_Marketing_Investment)
df1$Online_Marketing_Investment = normalize(df1$Online_Marketing_Investment)
df1$Affiliates_Investment = normalize(df1$Affiliates_Investment)
df1$SEM_Investment = normalize(df1$SEM_Investment)
df1$Radio_Investment = normalize(df1$Radio_Investment)
df1$Other_Investment = normalize(df1$Other_Investment)
df1$product_procurement_sla = normalize(df1$product_procurement_sla)
df1$gmv_per_unit = normalize(df1$gmv_per_unit)
df1$Discount = normalize(df1$Discount)
df1$Value = normalize(df1$Value)


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



fit_camera = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
             sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
             Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
             Other_Investment + gmv_per_unit + Discount + Premium_Product + Holiday + Salary_day, data = df_camera)

summary(fit_camera)


null = lm(gmv~1, data = df_camera)
full =fit_camera

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = df_camera)
summary(finalaic)



fit_camera_1 = lm(gmv ~ units + product_mrp + Affiliates_Investment + Discount + 
                    gmv_per_unit + order_payment_type  + Online_Marketing_Investment + 
                    Content_Marketing_Investment + Digital_Investment + Other_Investment + 
                    Premium_Product + Holiday, data = df_camera)
summary(fit_camera_1)

vif(fit_camera_1)


ff=boxCox(fit_camera_1)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df_camera$gmv =df_camera$gmv^best_lambda


fit_camera_2 = lm(gmv ~ units + product_mrp + Affiliates_Investment + Discount + 
                    gmv_per_unit + order_payment_type  + Online_Marketing_Investment + 
                    Content_Marketing_Investment + Digital_Investment + Other_Investment + 
                    Premium_Product + Holiday, data = df_camera)
summary(fit_camera_2)


fit_camera_3 = lm(gmv ~ units + product_mrp + Affiliates_Investment + Discount + 
                    gmv_per_unit + order_payment_type  + Online_Marketing_Investment + 
                    Content_Marketing_Investment + Digital_Investment  + 
                     Holiday, data = df_camera)
summary(fit_camera_3)




vif(fit_camera_3)
#Check Model Assumptions

fit_camera_4 = lm(gmv ~ units + product_mrp + Affiliates_Investment + Discount + 
                   order_payment_type  + Online_Marketing_Investment + 
                    Content_Marketing_Investment + Digital_Investment  + 
                    Holiday, data = df_camera)
summary(fit_camera_4)


vif(fit_camera_4)




fit_camera_5 = lm(gmv ~ units + product_mrp  + Discount + 
                    order_payment_type  + Online_Marketing_Investment + 
                     Digital_Investment  + 
                    Holiday, data = df_camera)
summary(fit_camera_5)


vif(fit_camera_5)



res = rstandard(fit_camera_5)   # standardized residual
yhat = predict(fit_camera_5) # predicted y

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



fit_speaker = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                  sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                  Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                  Other_Investment + gmv_per_unit + Discount  + Holiday + Salary_day, data = df_speaker)

summary(fit_speaker)


null = lm(gmv~1, data = df_speaker)
full =fit_speaker

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = df_speaker)
summary(finalaic)



fit_speaker_1 = lm(gmv ~ units + product_mrp + sla + Holiday + Sponsorship_Investment + 
                     SEM_Investment + Online_Marketing_Investment + Total_Investment + 
                     order_payment_type + Discount, data = df_speaker)
summary(fit_speaker_1)



ff=boxCox(fit_speaker_1)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df_speaker$gmv =df_speaker$gmv^best_lambda


fit_speaker_2 = lm(gmv ~ units + product_mrp + sla + Holiday + Sponsorship_Investment + 
                     SEM_Investment + Online_Marketing_Investment + Total_Investment + 
                     order_payment_type + Discount, data = df_speaker)
summary(fit_speaker_2)


fit_speaker_3 = lm(gmv ~ units + product_mrp + sla + Holiday + Sponsorship_Investment + 
                     SEM_Investment + Online_Marketing_Investment  + 
                     order_payment_type , data = df_speaker)
summary(fit_speaker_3)



vif(fit_speaker_3)
#Check Model Assumptions


res = rstandard(fit_speaker_3)   # standardized residual
yhat = predict(fit_speaker_3) # predicted y

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



fit_homeaudio = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                   sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                   Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                   Other_Investment + gmv_per_unit + Discount  + Holiday + Salary_day, data = df_homeaudio)

summary(fit_homeaudio)


null = lm(gmv~1, data = df_homeaudio)
full =fit_homeaudio

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = df_homeaudio)
summary(finalaic)



fit_homeaudio_1 = lm(gmv ~ units  + product_mrp + order_payment_type + 
                       product_procurement_sla + Discount + Holiday + Digital_Investment + 
                       Sponsorship_Investment, data = df_homeaudio)
summary(fit_homeaudio_1)



ff=boxCox(fit_homeaudio_1)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df_homeaudio$gmv =df_homeaudio$gmv^best_lambda


fit_homeaudio_2 = lm(gmv ~ units  + product_mrp  + 
                       product_procurement_sla + Discount + Holiday + Digital_Investment + 
                       Sponsorship_Investment, data = df_homeaudio)
summary(fit_homeaudio_2)




vif(fit_homeaudio_2)
#Check Model Assumptions


res = rstandard(fit_homeaudio_2)   # standardized residual
yhat = predict(fit_homeaudio_2) # predicted y

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



fit_game = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                     sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                     Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                     Other_Investment + gmv_per_unit + Discount + Premium_Product  + Holiday + Salary_day, data = df_game)

summary(fit_game)


null = lm(gmv~1, data = df_game)
full =fit_game

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = df_game)
summary(finalaic)



fit_game_1 = lm(gmv ~ units + product_mrp + gmv_per_unit + Holiday + Discount + 
                       product_procurement_sla + Premium_Product  + 
                       Sponsorship_Investment, data = df_game)
summary(fit_game_1)



ff=boxCox(fit_game_1)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df_game$gmv =df_game$gmv^best_lambda


fit_game_2 = lm(gmv ~ units + product_mrp + gmv_per_unit + Holiday + Discount + 
                  product_procurement_sla + Premium_Product  + 
                  Sponsorship_Investment, data = df_game)
summary(fit_game_2)



vif(fit_game_2)
#Check Model Assumptions

fit_game_3 = lm(gmv ~ units + product_mrp  + Holiday  + 
                   Premium_Product  + 
                  Sponsorship_Investment, data = df_game)
summary(fit_game_3)

vif(fit_game_3)

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



fit_cam_acc = lm(gmv ~ order_payment_type +  units + product_mrp + product_procurement_sla+
                sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
                Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
                Other_Investment + gmv_per_unit + Discount  + Holiday + Salary_day, data = df_cam_acc)

summary(fit_cam_acc)


null = lm(gmv~1, data = df_cam_acc)
full =fit_cam_acc

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = df_cam_acc)
summary(finalaic)



fit_cam_acc_1 = lm(gmv ~ units + gmv_per_unit + Holiday + product_mrp + Discount + 
                  order_payment_type + Sponsorship_Investment + Content_Marketing_Investment + 
                  product_procurement_sla + Affiliates_Investment, data = df_cam_acc)
summary(fit_cam_acc_1)



ff=boxCox(fit_cam_acc_1)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df_cam_acc$gmv =df_cam_acc$gmv^best_lambda


fit_cam_acc_2 = lm(gmv ~ units + gmv_per_unit + Holiday + product_mrp + Discount + 
                     order_payment_type + Sponsorship_Investment + Content_Marketing_Investment + 
                     product_procurement_sla + Affiliates_Investment, data = df_cam_acc)
summary(fit_cam_acc_2)




fit_cam_acc_3 = lm(gmv ~ units   + product_mrp + Discount + 
                     order_payment_type + Sponsorship_Investment + Content_Marketing_Investment + 
                     product_procurement_sla + Affiliates_Investment, data = df_cam_acc)
summary(fit_cam_acc_3)


vif(fit_cam_acc_3)

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





