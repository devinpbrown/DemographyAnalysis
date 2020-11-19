library(stargazer)
library(MASS)
library(viridis)

setwd('~/Dropbox/PhDApps/WritingSample')
final_df = read.csv('finaldata.csv')
#checking missingness
test_df <- final_df[, c('intercon', 'country_pop', 'aggdifxx', 'gdppc', 'polity2')]
outcome <- test_df[, 'intercon']
vars <- test_df[,c('country_pop','aggdifxx','gdppc','polity2')]
nacount <- apply(vars, 1, function(x) any(is.na(x)))
nacount2 <- outcome[nacount]
nacount2
sum(nacount2, na.rm=T)/length(na.omit(nacount2))
#intercon = 0.4812 which is nearly even draws from both, biasing us toward 0

test_df <- final_df[, c('rebellion', 'country_pop', 'aggdifxx', 'gdppc', 'polity2')]
outcome <- test_df[, 'rebellion']> 0
vars <- test_df[,c('country_pop','aggdifxx','gdppc','polity2')]
nacount <- apply(vars, 1, function(x) any(is.na(x)))

nacount2 <- outcome[nacount]
sum(nacount2, na.rm=T)/length(na.omit(nacount2))
#country pop = 0.27, this is removing more 0 than 1s which is expected



#running a simple logit regression on intercommunal violence (not violence with the state):
pop_logit_df = na.omit(final_df[, c('intercon', 'country_pop', 'aggdifxx', 'gdppc', 'polity2')])
con_logit_df = na.omit(final_df[, c('intercon', 'groupcon', 'aggdifxx', 'gdppc', 'polity2')])

pop_logit <- glm(intercon ~ country_pop + aggdifxx + gdppc + polity2, 
                 data=pop_logit_df,
                 family = binomial)
summary(pop_logit)

con_logit <- glm(intercon ~ groupcon + aggdifxx + gdppc + polity2, 
                 data=con_logit_df,
                 family = binomial)
summary(con_logit)
stargazer(pop_logit, con_logit)


#creating confidence interval plots
pdf(file = 'logit_coef.pdf', width = 6, height = 4)
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), tcl=-0.5, mar=c(3,4,2,1), cex.lab=1, cex.axis=1.1)
plot(rev(coef(pop_logit)), seq(1.1,5.1,1), yaxt='n', pch = 19, col="blue",
     xlim = c(min(confint(con_logit)), max(confint(con_logit))),
     ylim = c(0.75, 5.25),
     ylab = '', xlab = 'Estimates')
segments(x0 = rev(confint(pop_logit)[,1]), x1 = rev(confint(pop_logit)[,2]),
         y0 = seq(1.1,5.1,1), y1 = seq(1.1,5.1,1), col='blue')
points(rev(coef(con_logit)), seq(0.9,4.9,1), col = 'red')
segments(x0 = rev(confint(con_logit)[,1]), x1 = rev(confint(con_logit)[,2]), y0 = seq(0.9,4.9,1), y1 = seq(0.9,4.9,1), col='red', lty=1)
abline(v=0, lty = 3)
abline(h=seq(1.5,4.5,1), lty=4, lwd = 0.3)
axis(2, at = 1:5,
     labels = c('Polity IV','GDP/capita','Agg. Diff.','Population \n Group Con.','(Intercept)'),las = 2, cex.axis = 0.75, tck=0)
title("Confidence Intervals for Logit Coefficients", cex.main = 1, font.main=1)
legend('bottomright', legend = c('Population', 'Group Con.'), col = c('blue','red'), lty=1, pch=c(19, 1), cex = 0.75, bty='n')
dev.off()

#odds plot
pdf(file = 'logit_odds.pdf', width = 6, height = 4)
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), tcl=-0.5, mar=c(3,4,2,1), cex.lab=1, cex.axis=1.1)
plot(rev(exp(coef(pop_logit))), seq(1.1,5.1,1), yaxt='n', pch = 19, col="blue",
     xlim = c(min(exp(confint(con_logit))), max(exp(confint(con_logit)))),
     ylim = c(0.75,5.25),
     ylab = '', xlab = 'Odds Ratio')
segments(x0 = rev(exp(confint(pop_logit)[,1])), x1 = rev(exp(confint(pop_logit)[,2])),
         y0 = seq(1.1,5.1,1), y1 = seq(1.1,5.1,1), col='blue')
points(rev(exp(coef(con_logit))), seq(0.9,4.9,1), col='red')
segments(x0=rev(exp(confint(con_logit)[,1])), x1=rev(exp(confint(con_logit)[,2])), y0=seq(0.9,4.9,1), y1=seq(0.9,4.9,1), col='red')
abline(v=1, lty = 3)
abline(h=seq(1.5,4.5,1), lty=4, lwd = 0.3)
axis(2, at = 1:5,
     labels = c('Polity IV','GDP/capita','Agg. Diff','Population \n Group Con.','(Intercept)'),las = 2, cex.axis = 0.75, tck=0)
title("Confidence Intervals for Odds Ratios", cex.main = 1, font.main=1)
legend('bottomright', legend = c('Population', 'Group Con.'), col = c('blue','red'), lty=1, pch=c(19, 1), cex = 0.75, bty='n')
dev.off()


#comparing the models
anova(pop_logit, con_logit)
#pop's deviance is smaller, it does a better job fitting the values

binary_err_rate = function(mod.formula, data, seed = 1234, train = .8, ndraws = 1000){
  set.seed(seed)
  MSE = c() #mean squared error
  for(i in 1:ndraws){ 
    samp = sample(1:nrow(data), nrow(data)*train)
    train_data = data[samp,]
    test_data = data[-samp,]
    mod = glm(mod.formula, data = train_data,   
              family = binomial(link = 'logit'))
    probs = predict(mod, test_data, type = 'response')
    MSE = c(MSE, mean((probs - (as.numeric(test_data[, as.character(mod.formula[2])]) - 1))^2, na.rm = T)) #taking the probability of it being 1 or 0, taking the actual outcome subtracting the two and squaring them then taking the mean (these are squared error loss functions)
  }
  return(mean(MSE))
}

binary_err_rate(mod.formula = pop_logit$formula, data = pop_logit_df)
#1.23354 
binary_err_rate(mod.formula = con_logit$formula, data = con_logit_df)
#1.235212
#again, pop significantly outperforms groupcon


#now running the ordered logit regression on rebellion score
pop_olr_df = na.omit(final_df[, c('rebellion', 'country_pop', 'aggdifxx', 'gdppc', 'polity2')])
con_olr_df = na.omit(final_df[, c('rebellion', 'groupcon', 'aggdifxx', 'gdppc', 'polity2')])

pop_olr <- polr(as.factor(rebellion) ~ country_pop + aggdifxx + gdppc + polity2, 
                data=pop_olr_df)
summary(pop_olr)

con_olr <- polr(as.factor(rebellion) ~ groupcon + aggdifxx + gdppc + polity2, 
                data=con_olr_df)
summary(con_olr)


#plotting coefficients
pdf(file = 'olr_coef.pdf', width = 6, height = 4)
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), tcl=-0.5, mar=c(3,4,2,1), cex.lab=1, cex.axis=1.1)
plot(rev(coef(pop_olr)), seq(1.1,4.1,1), yaxt='n', pch = 19, col="blue",
     xlim = c(min(confint(pop_olr)), max(confint(con_olr))),
     ylim = c(0.75, 4.25),
     ylab = '', xlab = 'Estimates')
segments(x0 = rev(confint(pop_olr)[,1]), x1 = rev(confint(pop_olr)[,2]),
         y0 = seq(1.1,4.1,1), y1 = seq(1.1,4.1,1), col='blue')
points(rev(coef(con_olr)), seq(0.9,3.9,1), col = 'red')
segments(x0 = rev(confint(con_olr)[,1]), x1 = rev(confint(con_olr)[,2]), y0 = seq(0.9,3.9,1), y1 = seq(0.9,3.9,1), col='red', lty=1)
abline(v=0, lty = 3)
abline(h=seq(1.5,3.5,1), lty=4, lwd = 0.3)
axis(2, at = 1:4,
     labels = c('Polity IV','GDP/capita','Agg. Diff.','Population \n Group Con.'),las = 2, cex.axis = 0.75, tck=0)
title("Confidence Intervals for OLR Coefficients", cex.main = 1, font.main=1)
legend('bottomright', legend = c('Population', 'Group Con.'), col = c('blue','red'), lty=1, pch=c(19, 1), cex = 0.75, bty='n')
dev.off()

#creating predicted probability comp graphs
dataToPlot = data.frame('country_pop' = seq(min(pop_olr_df$country_pop), max(pop_olr_df$country_pop), length.out = 1000),
                        'aggdifxx' = mean(pop_olr_df$aggdifxx),
                        'gdppc' = mean(pop_olr_df$gdppc),
                        'polity2' = mean(pop_olr_df$polity2))

pred = predict(pop_olr, newdata = dataToPlot, type = 'probs')
cols = viridis(8)

pdf(file = 'pop_olr_comp_probs.pdf', width = 6, height = 4)
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), tcl=-0.5, mar=c(3,3,1,1))
plot(pred[,1] ~ dataToPlot$country_pop, type = 'l', col = cols[1], ylim = c(0,0.8), xlab = 'Country Population', ylab='Predicted Probabilities')
for (i in 2:8){
  points(pred[,i] ~ dataToPlot$country_pop, col = cols[i], type = 'l')
}
legend('topright', col = cols, legend = 0:7, lty = 1, horiz = T, cex = 0.7, bty = 'n')
rug(pop_olr_df$country_pop)
dev.off()

pdf(file = 'pop_olr_comp_probs_zoom.pdf', width = 6, height=4)
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), tcl=-0.5, mar=c(3,3,1,1))
plot(pred[,1] ~ dataToPlot$country_pop, type = 'l', col = cols[1], ylim = c(0,0.25), xlab = 'Country Population', ylab = 'Predicted Probabilities')
for (i in 2:8){
  points(pred[,i] ~ dataToPlot$country_pop, col = cols[i], type = 'l')
}
legend('topright', col = cols, legend = 0:7, lty = 1, horiz = T, cex = 0.7, bty = 'n')
rug(pop_olr_df$country_pop)
dev.off()

dataToPlot = data.frame('groupcon' = seq(min(con_olr_df$groupcon), max(con_olr_df$groupcon), length.out = 1000),
                        'aggdifxx' = mean(con_olr_df$aggdifxx),
                        'gdppc' = mean(con_olr_df$gdppc),
                        'polity2' = mean(con_olr_df$polity2))

pred = predict(con_olr, newdata = dataToPlot, type = 'probs')

pdf(file = 'con_olr_comp_probs.pdf', width = 6, height = 4)
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), tcl=-0.5, mar=c(3,3,1,1))
plot(pred[,1] ~ dataToPlot$groupcon, type = 'l', col = cols[1], ylim = c(0,0.95), xlab = 'Group Concentration', ylab='Predicted Probabilities')
for (i in 2:8){
  points(pred[,i] ~ dataToPlot$groupcon, col = cols[i], type = 'l')
}
legend('topright', col = cols, legend = 0:7, lty = 1, horiz = T, cex = 0.7, bty = 'n')
dev.off()

pdf(file = 'con_olr_comp_probs_zoom.pdf', width = 6, height=4)
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), tcl=-0.5, mar=c(3,3,1,1))
plot(pred[,1] ~ dataToPlot$groupcon, type = 'l', col = cols[1], ylim = c(0,0.15), xlab = 'Group Concentration', ylab = 'Predicted Probabilities')
for (i in 2:8){
  points(pred[,i] ~ dataToPlot$groupcon, col = cols[i], type = 'l')
}
legend('topright', col = cols, legend = 0:7, lty = 1, horiz = T, cex = 0.7, bty = 'n')
dev.off()

#comparing the two models
anova(pop_olr, con_olr)
#model 2 is better

polr_toMax = function(expl, data, seed = 1234, train = .8, ndraws = 1000){
  set.seed(seed)
  toMax = c()
  require(MASS)
  for(i in 1:ndraws){ #can parallelize if extra cores are available
    samp = sample(1:nrow(data), nrow(data)*train)
    train_data = data[samp,]
    test_data = data[-samp,]
    mod = polr(as.factor(rebellion) ~ get(expl) + aggdifxx + gdppc + polity2, 
               data=train_data)
    probs = predict(mod, test_data, type = 'probs')
    probSum = sum(unlist(lapply(1:nrow(test_data), function(x){
      probs[x, test_data[x, 'rebellion']]
    })), na.rm = T)
    toMax = c(toMax, probSum)
  }
  return(mean(toMax))
}

#this takes each observation in the test_data, looks at the observed value then using the train data tests the probability that using the train model it could have guessed the observed point. Then adds those probabilities up.

#warning: these functions take quite a while to run
polr_toMax(expl = 'country_pop', data = pop_olr_df)
#104.9341
polr_toMax(expl = 'groupcon', data = con_olr_df)
#102.8578
