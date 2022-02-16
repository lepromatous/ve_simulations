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





##################################################
#### simulate data
##################################################

makedata <- function(total.covid, total.nocovid, total.vaccine.covid, total.vaccine.nocovid){
    
    covid <- c(rep(1, times=total.covid), rep(0, times=total.nocovid))   
    vaccine.status <- c(rep(1, times=total.vaccine.covid), rep(0, times=total.covid-total.vaccine.covid), rep(1, times=total.vaccine.nocovid), rep(0, times=total.nocovid-total.vaccine.nocovid))
        
    df <- data.frame(covid, vaccine.status)
    
    prevalence <- round(total.covid / (total.covid+ total.nocovid)*100,2)
    risk.vax <- total.vaccine.covid / (total.vaccine.nocovid+total.vaccine.covid)
    risk.unvax <- (total.covid - total.vaccine.covid) / ((total.covid - total.vaccine.covid) + (total.nocovid - total.vaccine.nocovid))
    rr <- risk.vax / risk.unvax
    cVE <- round((1- rr) *100,2)
    
    # LOGISTIC REGRESSION VE
    mod.log <- glm(covid ~ vaccine.status, family="binomial")
    log.ve <- round((1-exp(mod.log$coefficients))*100,2)[2]
    
    log.ve.ci <- 1-(exp(confint(mod.log)))[2,]
    log.ve.ci <- paste0("(", round(log.ve.ci[2]*100,3), " - ",round(log.ve.ci[1]*100,3), ")")
    log.out <- paste(log.ve, log.ve.ci)
    
    
    ### POISSON VE
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
      output<-round(output,5)
      return(output)
    }
    
    mod.pois <- glm(covid ~ vaccine.status, family="poisson")
    mod.pois <- poisson.rev(mod.pois)[2,]
    ve.pois <- (1-mod.pois)*100
    pois.out <- paste(ve.pois[1], paste0("(", ve.pois[3], " - ", ve.pois[2], ")"))
    
    mod.nb <- MASS::glm.nb(covid ~ vaccine.status)
    ve.nb <- round((1-exp(yo$coefficients))*100,2)[2]
    nb.ci <- round((1-(exp(confint(mod.nb))[2,]))*100,3)
    nb.out <- paste(ve.nb[1], paste0("(", nb.ci[2], " - ", nb.ci[1], ")"))
    
    
    out <- data.frame(
      "Prevalence" = prevalence,
      "Actual VE" = cVE,
      "Logistic VE" = log.out,
      "Poisson VE" = pois.out,
      "Negative Binomial VE" = nb.out
    )
    library(gt)
    
    out %>%
      
      gt::gt() %>%
      
      tab_options(
        table.align = "center"
      ) %>%
      
      cols_align("center", columns = everything()) %>%
      
      cols_label(
        Prevalence = "Prevalence of COVID-19 (%)",
        Actual.VE = "VE, Actual (%)%",
        Logistic.VE = "VE, Logistic (%)%",
        Poisson.VE = "VE, Poisson w/REV (%)%",
        Negative.Binomial.VE = "VE, Neg Bin (%)"
      ) -> out.tab
      
      
    return(list(df, out.tab))
    }

### matches NEJM
makedata(total.covid=1472, total.nocovid=3420, total.vaccine.covid=167, total.vaccine.nocovid=1072)


test <- makedata(total.covid=54, total.nocovid=(2000-54), total.vaccine.covid=12, total.vaccine.nocovid=1000-40)[[1]]

makedata(total.covid=54, total.nocovid=(2000-54), total.vaccine.covid=12, total.vaccine.nocovid=1000-40)[[2]]




