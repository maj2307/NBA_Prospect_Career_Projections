setwd("~/Documents/Year 6/Fall/STAT 482")

packages = c('bestNormalize','RSQLite' , 'kader', 'rvest', 'tidyverse', 'knitr', 'goftest', 'ggplot2', 'ggiraphExtra')
lapply(packages, library, character.only = T)
library(MASS)
library(pscl)


nba = read.csv("nba.csv")
nba$Position = as.factor(nba$Position)
nba$College = as.factor(nba$College)
nba_raw = nba
#Player Removal
removal = c("Andrew Bynum", "Al Jefferson", "Kendrick Perkins", "Tyson Chandler", 
            "Amar'e Stoudemire", "Giannis Antetokounmpo", "Sebastian Telfair", "J.R. Smith", 
            "Primoz Brezec", "Qyntel Woods", "Kedrick Brown")
remIndex = rep.int(0, length(removal))
for (i in 1:length(removal)) {
  remIndex[i] = which(nba$player == removal[i])
}
nba = nba[-remIndex,]
#nba[c(339,468),"NCAA_AST"] = 2.7
#nba[c(337,440,441,468),"NCAA_STL"] = 1.43
#nba[450, "NCAA_STL"] = 1.59
#nba = nba[-c(1,154,270),]
#nba[c(333,433), "NCAA_BLK"] = 2.0
#nba[241,"NCAA_BLK"] = 1.3
nba$cat_ws[nba$ws < 3.80] = "Low"
nba$cat_ws[nba$ws >= 3.8 & nba$ws<15.6 ] = "Lower Medium"
nba$cat_ws[nba$ws >= 15.6 & nba$ws<35.9 ] = "Higher Medium"
nba$cat_ws[nba$ws>=35.9 & nba$ws<61.3] = "High"
nba$cat_ws[nba$ws >= 61.3 & nba$ws<78.06] = "Very High"
nba$cat_ws[nba$ws>=78.06]="Elite"
nba$cat_ws <- factor(nba$cat_ws, levels = c("Elite","Very High", "High", "Higher Medium", "Lower Medium", "Low"))

p1<-ggplot(data=nba)+geom_mosaic(aes(x=product(cat_ws,Pick),fill=cat_ws))+labs(x="Pick Ascending Left To Right",y="Percent",fill="Success Level")
p2<- p1 + scale_y_continuous(labels = percent)


plotmeans(ws ~ Pick, data = nba, frame = FALSE,
          mean.labels = TRUE, connect = FALSE)
  
  sp + scale_x_continuous(name="Speed of cars", limits=c(0, 30)) +
  scale_y_continuous(name="Stopping distance", limits=c(0, 150))

p<-ggplot(nba, aes(x=Pick,y=ws, fill=cat_ws)) +
  geom_bar(stat="identity")
p<-p+labs(y="Win Shares",fill="Success Level")






#Train-Test Split
nba$pick_l[nba$Pick<=5]="Top 5"
nba$pick_l[nba$Pick>5 & nba$Pick<=10]="Top 10"
nba$pick_l[nba$Pick>10 & nba$Pick<=15]="Late Lottery"
nba$pick_l[nba$Pick>15 & nba$Pick <= 20]="Mid-Late First Round"
nba$pick_l[nba$Pick>20]="Late First Round"

install.packages('plyr')
library(plyr)
factor(nba$pick_l)
count(nba$pick_l)
ggplot(nba,aes(x=,y=,))
plot(count(nba$pick_l),nba$cat_ws)
ggplot(nba) + geom_bar(aes(x = cat_ws,fill=pick_l))


nba %>%  
  filter(Pick==1)->nba11

nba<-nba[,c(3,5:16,18,19,20,22)]
g<-ggplot(nba,aes(x=Pick,y=ws))+geom_point(alpha=.5)
for(i in 1:30){
  g<-g+annotate("point",i,a[i],color="blue")+annotate("point",i,b[i],color="red")
}

g<-ggplot(nba,aes(x=Pick,y=ws))+geom_point()

a<-tapply(nba$ws, nba$Pick, mean)
b<-tapply(nba$ws, nba$Pick, median)

plot(1:30,a,type="p",col="red")
points(1:30,b,col="blue")


set.seed(34)
smp.size = sample.int(n = nrow(nba), size = floor(.8*nrow(nba)), replace = F)
train = nba[smp.size, ]
test  = nba[-smp.size, ]

nbaws <- summarySE(nba, measurevar="ws", groupvars="Pick")

ggplot(nbaws, aes(x=Pick, y=ws,color)) + 
  geom_errorbar(aes(ymin=ws-se, ymax=ws+se), width=.1) +
  geom_line() +
  geom_point()

ggplot(tgc, aes(x=dose, y=len, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)


###########################################################################################
#Model Selection
regMat.ln = expand.grid(c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE))
regressors.ln = names(regMat.ln) = names(nba[,c(3,5:16,18,19)])
num_var.ln = apply(regMat.ln, 1,sum)
allModelsList.ln = apply(regMat.ln, 1, function(x) as.formula(
  paste(c("ws ~ 1", regressors.ln[x]),
        collapse=" + ")) )
allModelsResults.ln = lapply(allModelsList.ln,
                           function(x) glm(x, family=gaussian,
                                           data=train))
ourAIC.ln = unlist(lapply(allModelsResults.ln, function(x)
  AIC(x)))
ourBIC.ln = unlist(lapply(allModelsResults.ln, function(x)
  BIC(x)))
ourResid.Dev.ln = unlist(lapply(allModelsResults.ln, function(x)
  x$deviance))
ourResid.Dev.df.ln = unlist(lapply(allModelsResults.ln, function(x)
  x$df.residual))
ourNull.Dev.ln = unlist(lapply(allModelsResults.ln, function(x)
  x$null.deviance))
ourNull.Dev.df.ln = unlist(lapply(allModelsResults.ln, function(x)
  x$df.null))
ourDev.pval.ln = unlist(lapply(length(allModelsResults.ln), function(x)
  1-pchisq(ourNull.Dev.ln[x]-ourResid.Dev.ln[x],ourNull.Dev.df.ln[x]-ourResid.Dev.df.ln[x])))
ourloglik.ln = unlist(lapply(allModelsResults.ln, function(x)
  as.numeric(logLik(x)) ))
ourAIC.rank.ln = rank(ourAIC.ln)
ourBIC.rank.ln = rank(ourBIC.ln)
ourAvg.rank.ln = apply(data.frame(ourAIC.rank.ln, ourBIC.rank.ln),1,mean)
results.ln = data.frame( model = as.character(allModelsList),
                       Num.Var = num_var.ln,
                       loglik=ourloglik.ln,
                       aic=ourAIC.ln,
                       bic=ourBIC.ln,
                       Residual.Deviance=ourResid.Dev.ln,
                       Residual.df=ourResid.Dev.df.ln,
                       Null.Deviance = ourNull.Dev.ln,
                       Null.df = ourNull.Dev.df.ln,
                       Dev.pval = ourDev.pval.ln,
                      aic.rank = ourAIC.rank.ln,
                      bic.rank = ourBIC.rank.ln,
                      avg.rank = ourAvg.rank.ln
)

#write_csv(results, "LinModelComps.csv")
##################################################################################

# Training Models
fit.train.null = lm(ws ~ 1, data = train)
fit.train.1 = lm(ws ~ 1 + Pick, data = train)
fit.train.6 = lm(ws ~ 1 + Pick + Age + Inches + Weight + NCAA_fgpct + NCAA_STL, data = train)
fit.train.7 = lm(ws ~ 1 + Pick + Age + Inches + Weight + NCAA_fgpct
                 + NCAA_ftpct + NCAA_STL, data = train)

#Prediction Values
predict.6 = predict(fit.train.6, test)
predict.7 = predict(fit.train.7, test)


mspe.6 = mean((test$ws - predict.6)^2)
mspe.7 = mean((test$ws - predict.7)^2)


rmse.6<-sqrt(mspe.6)
rmse.7<-sqrt(mspe.7)



###########################################################################################
regMat = expand.grid(c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE),
                     c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE),
                     c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE),
                     c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE))
regressors = names(regMat) = names(nba[,c(3,5:16,18,19)])
num_var = apply(regMat, 1,sum)
allModelsList = apply(regMat, 1, function(x) as.formula(
  paste(c("group ~ 1", regressors[x]),
        collapse=" + ")) )
allModelsResults = lapply(allModelsList,
                          function(x) glm.nb(x, link=log,
                                          data=train))
ourAIC = unlist(lapply(allModelsResults, function(x)
  AIC(x)))
ourBIC = unlist(lapply(allModelsResults, function(x)
  BIC(x)))
ourResid.Dev = unlist(lapply(allModelsResults, function(x)
  x$deviance))
ourResid.Dev.df = unlist(lapply(allModelsResults, function(x)
  x$df.residual))
ourNull.Dev = unlist(lapply(allModelsResults, function(x)
  x$null.deviance))
ourNull.Dev.df = unlist(lapply(allModelsResults, function(x)
  x$df.null))
ourDev.pval = unlist(lapply(length(allModelsResults), function(x)
  1-pchisq(ourNull.Dev[x]-ourResid.Dev[x],ourNull.Dev.df[x]-ourResid.Dev.df[x])))
ourloglik = unlist(lapply(allModelsResults, function(x)
  as.numeric(logLik(x)) ))
ourAIC.rank = rank(ourAIC)
ourBIC.rank = rank(ourBIC)
ourAvg.rank = apply(data.frame(ourAIC.rank, ourBIC.rank),1,mean)
results = data.frame( model = as.character(allModelsList),
                      Num.Var = num_var,
                      loglik=ourloglik,
                      aic=ourAIC,
                      bic=ourBIC,
                      Residual.Deviance=ourResid.Dev,
                      Residual.df=ourResid.Dev.df,
                      Null.Deviance = ourNull.Dev,
                      Null.df = ourNull.Dev.df,
                      Dev.pval = ourDev.pval,
                      aic.rank = ourAIC.rank,
                      bic.rank = ourBIC.rank,
                      avg.rank = ourAvg.rank
)



#Prediction Values
predict.6 = predict(fit.train.6, test)
#Mean Square Prediction Error
mspe.6 = mean((test$ws - predict.6)^2)
resids <- test$ws - predict.6
std.resids = resids/sqrt(sum(resids^2)/(length(resids)-1))


plot(predict.6, resids, xlab = "Fitted", ylab = "Residuals", main = "Test Residuals vs Fitted")
lines(loess.smooth(predict.6, resids), col = "red")
abline(h = 0, lty = 2, col = "grey")
qqnorm(std.resids, ylab = "Standardized Test Residuals")
qqline(std.resids, lty = 2)
plot(predict.6, sqrt(abs(std.resids)), xlab = "Fitted", ylab = "sqrt(abs(Standardized Test Residuals))",
     main = "Scale-Location")
lines(loess.smooth(predict.6, sqrt(abs(std.resids))), col = "red")

