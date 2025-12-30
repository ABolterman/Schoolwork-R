library(alr4)
data(package="alr4")
View(Forbes)
help("Forbes")
?Forbes
#indepent x: bp;  dependent y:lpress

###
x<-Forbes$bp
y<-Forbes$lpres
xbar<- mean(Forbes$bp)
ybar<- mean(Forbes$lpres)
c(xbar, ybar)

SXY<- sum((x-xbar)*(y-ybar))
SXY

SXX<-sum((x-xbar)*(x-xbar))
SXX
SYY<- sum((y-ybar)*(y-ybar))

SYY

beta1_hat<-SXY/SXX
beta1_hat
beta0_hat<- ybar-beta1_hat*xbar
beta0_hat

E(Y|X)=E(lpress|bp)=-42.13778+0.8954937*bp

rxy<-cor(Forbes$bp, Forbes$lpres)
beta1_hat<-rxy*sqrt(SYY/SXX)
beta1_hat


# Confidence interval for beta_0
# CI = beta0_hat-ME <= beta0 <= beta0_hat + ME
# ME = t_crit*se(beta_0)
RSS = SYY-beta1_hat^2*SXX
df = dim(Forbes)[1]-2
sigma_sq_hat = RSS/(df)
t_crit = qt(.95, df = 15)

se_beta0_hat = sqrt(sigma_sq_hat * (1/17+xbar^2/SXX))

L = beta0_hat - t_crit*se_beta0_hat
U = beta0_hat + t_crit*se_beta0_hat

c(L,U)


# Confidence Interval for beta_1
se_beta1_hat = sqrt(sigma_sq_hat / SXX)

L = beta1_hat - t_crit*se_beta1_hat
U = beta1_hat + t_crit*se_beta1_hat
c(L,U)



# Prediction interval when x = 200

x = Forbes$bp
y = Forbes$lpres
# beta0_hat; beta calculated
y_telda = beta0_hat+beta1_hat*200
var_y_telda = sigma_sq_hat*(1+ 1/dim(Forbes)[1] + ((200-xbar)^2/SXX))
se_y_telda = sqrt(var_y_telda)

t_crit = qt(.005, df = 17-2, lower.tail = FALSE)
L = y_telda - t_crit*se_y_telda
U = y_telda + t_crit*se_y_telda

c(L,U)


# Prediction interval for data in original scale
l_orig = 10^(L/100)
u_orig = 10^(U/100)
c(l_orig, u_orig)


## Chapter 3

library(alr4)
# Response variable = Female life expectancy

mod1 = lm(UN11$lifeExpF ~ UN11$fertility)
summary(mod1)

mod2 = lm(UN11$lifeExpF ~ UN11$ppgdp)
summary(mod2)

mod3 = lm(UN11$lifeExpF ~ UN11$fertility + UN11$ppgdp)
summary(mod3)

anova(mod3)


library(ggplot2)

babies_data = read.csv("babies.csv", header = TRUE, sep = ",")

ggplot(babies_data, aes(gestation, bwt)) + geom_point()
m = lm(bwt~gestation, data = babies_data)
m$coefficients
m$residuals
m$fitted.values
summary(m)
resid(m)
predict(m)
y = babies_data$bwt
y_hat = m$fitted.values

y-y_hat


### Residual Analysis

plot(m)

babies_data = na.omit(babies_data)
y = babies_data$bwt
m = lm(bwt~gestation, data = babies_data)
y_hat = m$fitted.values
y-y_hat

babies_data$residual = resid(m)
babies_data$yhat = predict(m)

babies_data$resisidual1 = babies_data$bwt - babies_data$yhat











