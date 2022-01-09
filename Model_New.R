getwd()

setwd("C:\\Users\\p.ravindra.rokade\\Documents\\IPBA - IIM Indore\\BYOP")
getwd()
df = read.csv("aggregated_v1.csv")
df1 = df
sum(is.na(df1))
df1[is.na(df1)] = 0
sum(is.na(df1))

df1$order_payment_type = as.factor(df1$order_payment_type)
df1$product_analytic_sub_category = as.factor(df1$product_analytic_sub_category)
df1$Premium_Product = as.factor(df1$Premium_Product)


fit_1 = lm(gmv ~ order_payment_type + product_analytic_sub_category + units + product_mrp + product_procurement_sla+
             sla  + Total_Investment + TV_Investment + Digital_Investment + Sponsorship_Investment + Content_Marketing_Investment+ 
             Online_Marketing_Investment + Affiliates_Investment + SEM_Investment + Radio_Investment + 
             Other_Investment + gmv_per_unit + Discount + Premium_Product + Holiday + Salary_day, data = df1)

summary(fit_1)



null = lm(gmv~1, data = df1)
full =fit_1

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = lm(aicmodel$terms, data = df1)
summary(finalaic)





df1$product_analytic_sub_categoryAudioAccessory = ifelse(df1$product_analytic_sub_category=="AudioAccessory",1,0)
df1$product_analytic_sub_categoryAudioMP3Player = ifelse(df1$product_analytic_sub_category=="AudioMP3Player",1,0)
df1$product_analytic_sub_categoryCamera = ifelse(df1$product_analytic_sub_category=="Camera",1,0)
df1$product_analytic_sub_categoryCameraAccessory = ifelse(df1$product_analytic_sub_category=="CameraAccessory",1,0)
df1$product_analytic_sub_categoryCameraStorage = ifelse(df1$product_analytic_sub_category=="CameraStorage",1,0)
df1$product_analytic_sub_categoryGameMembershipCards = ifelse(df1$product_analytic_sub_category=="GameMembershipCards",1,0)
df1$product_analytic_sub_categoryGamingAccessory = ifelse(df1$product_analytic_sub_category=="GamingAccessory",1,0)
df1$product_analytic_sub_categoryHomeAudio = ifelse(df1$product_analytic_sub_category=="HomeAudio",1,0)
df1$product_analytic_sub_categoryHomeTheatre = ifelse(df1$product_analytic_sub_category=="HomeTheatre",1,0)
df1$product_analytic_sub_categorySpeaker = ifelse(df1$product_analytic_sub_category=="Speaker",1,0)
df1$product_analytic_sub_categoryTVVideoSmall = ifelse(df1$product_analytic_sub_category=="TVVideoSmall",1,0)



fit_7 = lm(gmv ~ product_analytic_sub_category+
             units + product_mrp+
             gmv_per_unit+
             Discount+
             Holiday+
             Premium_Product+
             sla+
             Sponsorship_Investment+
             Content_Marketing_Investment+
             Digital_Investment, data = df1)
summary(fit_7)


ff=boxCox(fit_7)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df1$gmv =df1$gmv^best_lambda


fit_7_1 = lm(gmv ~ product_analytic_sub_category+
             units + product_mrp+
             gmv_per_unit+
             Discount+
             Holiday+
             Premium_Product+
             sla+
             Sponsorship_Investment+
             Content_Marketing_Investment+
             Digital_Investment, data = df1)
summary(fit_7_1)




#Run before Box_cox
fit_8 = lm(gmv ~ product_analytic_sub_categoryAudioMP3Player + 
             product_analytic_sub_categoryCamera+
             product_analytic_sub_categoryCameraAccessory+
             product_analytic_sub_categoryGamingAccessory+
             product_analytic_sub_categoryHomeAudio+
             product_analytic_sub_categoryHomeTheatre+
             product_analytic_sub_categorySpeaker +
             product_analytic_sub_categoryTVVideoSmall+
             units + product_mrp+
             gmv_per_unit+
             Discount+
             Holiday+
             Premium_Product+
             sla+
             Sponsorship_Investment+
             Content_Marketing_Investment+
             Digital_Investment, data = df1)
summary(fit_8)


library(car)

ff=boxCox(fit_8)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df1$gmv =df1$gmv^best_lambda



#run after box_cox
fit_9 = lm(gmv ~ product_analytic_sub_categoryAudioMP3Player + 
             product_analytic_sub_categoryCamera+
             product_analytic_sub_categoryCameraAccessory+
             product_analytic_sub_categoryGamingAccessory+
             product_analytic_sub_categoryHomeAudio+
             product_analytic_sub_categoryHomeTheatre+
             product_analytic_sub_categorySpeaker +
             product_analytic_sub_categoryTVVideoSmall+
             units + product_mrp+
             gmv_per_unit+
             Discount+
             Holiday+
             Premium_Product+
             sla+
             Sponsorship_Investment+
             Content_Marketing_Investment+
             Digital_Investment, data = df1)
summary(fit_9)


vif(fit_9)

fit_10 = lm(gmv ~ product_analytic_sub_categoryAudioMP3Player + 
             product_analytic_sub_categoryCamera+
             product_analytic_sub_categoryCameraAccessory+
             product_analytic_sub_categoryGamingAccessory+
             product_analytic_sub_categoryHomeAudio+
             product_analytic_sub_categoryHomeTheatre+
             product_analytic_sub_categorySpeaker +
             product_analytic_sub_categoryTVVideoSmall+
             units + product_mrp+
             Discount+
             Holiday+
             Premium_Product+
             sla+
             Sponsorship_Investment+
             Content_Marketing_Investment+
             Digital_Investment, data = df1)
summary(fit_10)


vif(fit_10)


fit_11 = lm(gmv ~ product_analytic_sub_categoryAudioMP3Player + 
              product_analytic_sub_categoryCamera+
              product_analytic_sub_categoryCameraAccessory+
              product_analytic_sub_categoryGamingAccessory+
              product_analytic_sub_categoryHomeAudio+
              product_analytic_sub_categoryHomeTheatre+
              product_analytic_sub_categorySpeaker +
              product_analytic_sub_categoryTVVideoSmall+
              units + product_mrp+
              Discount+
              Holiday+
              sla+
              Sponsorship_Investment+
              Content_Marketing_Investment+
              Digital_Investment, data = df1)
summary(fit_11)


vif(fit_11)
#Check Model Assumptions


res = rstandard(fit_11)   # standardized residual
yhat = predict(fit_11) # predicted y

# uncorrelated error
plot(res); abline(h=0); 

plot(yhat,res); abline(h=0); 


#---------------------
# multicollinearity



fit_10 = lm(gmv ~ product_analytic_sub_category +
             units + product_mrp+
             Discount+
             Holiday+
             Premium_Product+
             sla+
             Sponsorship_Investment+
             Content_Marketing_Investment+
             Digital_Investment, data = df1)
summary(fit_10)



library(car)

ff=boxCox(fit_10)
best_lambda=ff$x[which.max(ff$y)]
best_lambda
df1$gmv =df1$gmv^best_lambda

vif(fit_10)
barplot(vif(fit_10))


# linearity check.....

#---------------------


#---------------------
# Normality check
hist(res)
qqnorm(res);qqline(res);
#---------------------
boxplot(res)
#---------------------

