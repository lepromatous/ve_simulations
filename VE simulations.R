#  WHat does it look like with 5% prevalence of outcome?
## VE = 70%
1-((12/1000) / (40/1000)) ### 12 cases vaccine group, 40 cases non vaccine group
## 70%

(12+40)/1000 # ~ 5% prevalence  ; 4% in unexposed

covid <- c(rep(1, times=12), rep(0, times=500-12), rep(1, times=40), rep(0, times=500-40))
vaccine.status <- c(rep(1, times=500), rep(0, times=500))

### with logistic
mod1 <- glm(covid ~ vaccine.status, family="binomial")
1-exp(mod1$coefficients) # ~72% ve

#### with poisson
poisson.rev<-function(a){
  require(sandwich) || install.packages(sandwich) 
  cov.m1 <- vcovHC(a, type = "HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate = coef(a), `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
                   pnorm(abs(coef(a)/std.err), lower.tail = FALSE), LL = coef(a) - 1.96 * 
                   std.err, UL = coef(a) + 1.96 * std.err)
  RR.CI<-exp(r.est[,c(1,4,5)])
  output<-cbind(RR.CI, r.est[,3])
  colnames(output)<-c("Risk Ratio", "Lower 95% CI", "Upper 95% CI", "P-value")
  output<-round(output,3)
return(output)
}
mod1.pois <- mod1 <- glm(covid ~ vaccine.status, family="poisson")
mod1.pois.adj <- poisson.rev(mod1.pois)
1 - mod1.pois.adj[2,1] ## 70% ve - exactly our crude estimate.  Logistic overestimates. 






###########################################################################################
##### what about 20% prevalence of covid?
## VE = 71%
1-((45/1000) / (155/1000)) ### 45 cases vaccine group, 155 cases non vaccine group
## 71%

(45 + 150)/1000 # ~ 20% prevalence  ; 15% in unexposed

covid <- c(rep(1, times=45), rep(0, times=500-45), rep(1, times=150), rep(0, times=500-150))
vaccine.status <- c(rep(1, times=500), rep(0, times=500))

### with logistic
mod1 <- glm(covid ~ vaccine.status, family="binomial")
1-exp(mod1$coefficients) # ~77% ve

#### with poisson
poisson.rev<-function(a){
  require(sandwich) || install.packages(sandwich) 
  cov.m1 <- vcovHC(a, type = "HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate = coef(a), `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
                   pnorm(abs(coef(a)/std.err), lower.tail = FALSE), LL = coef(a) - 1.96 * 
                   std.err, UL = coef(a) + 1.96 * std.err)
  RR.CI<-exp(r.est[,c(1,4,5)])
  output<-cbind(RR.CI, r.est[,3])
  colnames(output)<-c("Risk Ratio", "Lower 95% CI", "Upper 95% CI", "P-value")
  output<-round(output,3)
  return(output)
}
mod1.pois <- mod1 <- glm(covid ~ vaccine.status, family="poisson")
mod1.pois.adj <- poisson.rev(mod1.pois)
1 - mod1.pois.adj[2,1] ## 70% ve - exactly our crude estimate.  Logistic overestimates. 




############################################################################################
##### 30% like this: https://www.nejm.org/doi/pdf/10.1056/NEJMoa2106599
## VE = 63%
1-((167/1472) / (1072/3420)) ### 
## 63%

(1472)/(3420+1472) # ~ 30% prevalence  

covid <- c(rep(1, times=1472), rep(0, times=3420))
vaccine.status <- c(rep(1, times=167), rep(0, times=1472-167), rep(1, times=1072), rep(0, times=3420-1072))

### with logistic
mod1 <- glm(covid ~ vaccine.status, family="binomial")
1-exp(mod1$coefficients) # ~72% ve

#### with poisson
poisson.rev<-function(a){
  require(sandwich) || install.packages(sandwich) 
  cov.m1 <- vcovHC(a, type = "HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate = coef(a), `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
                   pnorm(abs(coef(a)/std.err), lower.tail = FALSE), LL = coef(a) - 1.96 * 
                   std.err, UL = coef(a) + 1.96 * std.err)
  RR.CI<-exp(r.est[,c(1,4,5)])
  output<-cbind(RR.CI, r.est[,3])
  colnames(output)<-c("Risk Ratio", "Lower 95% CI", "Upper 95% CI", "P-value")
  output<-round(output,3)
  return(output)
}
mod1.pois <- mod1 <- glm(covid ~ vaccine.status, family="poisson")
mod1.pois.adj <- poisson.rev(mod1.pois)
1 - mod1.pois.adj[2,1] ## 62.3% ve - odds overestimates 10 absolute percentage points






###########################################################################################
##### what about 40% prevalence of covid?
### example: https://jamanetwork.com/journals/jama/fullarticle/2786039
## VE = 71%
1-((90/1000) / (310/1000)) ### 90 cases vaccine group, 310 cases non vaccine group
## 71%

(90 + 310)/1000 # ~ 40% prevalence  ; 31% in unexposed

covid <- c(rep(1, times=90), rep(0, times=500-90), rep(1, times=310), rep(0, times=500-310))
vaccine.status <- c(rep(1, times=500), rep(0, times=500))

### with logistic
mod1 <- glm(covid ~ vaccine.status, family="binomial")
1-exp(mod1$coefficients) # ~87% ve

#### with poisson
poisson.rev<-function(a){
  require(sandwich) || install.packages(sandwich) 
  cov.m1 <- vcovHC(a, type = "HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate = coef(a), `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
                   pnorm(abs(coef(a)/std.err), lower.tail = FALSE), LL = coef(a) - 1.96 * 
                   std.err, UL = coef(a) + 1.96 * std.err)
  RR.CI<-exp(r.est[,c(1,4,5)])
  output<-cbind(RR.CI, r.est[,3])
  colnames(output)<-c("Risk Ratio", "Lower 95% CI", "Upper 95% CI", "P-value")
  output<-round(output,3)
  return(output)
}
mod1.pois <- mod1 <- glm(covid ~ vaccine.status, family="poisson")
mod1.pois.adj <- poisson.rev(mod1.pois)
1 - mod1.pois.adj[2,1] ## 71% ve - exactly our crude estimate.  Logistic overestimates. 

