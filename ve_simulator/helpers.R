library(gt)
library(tidyverse)


makedata <- function(n.vaccinated.case=314, n.case=1983, n.vaccinated.control = 1386, n.control=2530){
  
  ### create some other variables
  n.unvaccinated.case = n.case - n.vaccinated.case
  n.unvaccinated.control = n.control - n.vaccinated.control
  
  ### build df
  covid <- c(rep(1, times=n.case), rep(0, times=n.control))   
  vaccine.status <- c(rep(1, times=n.vaccinated.case),  
                      rep(0, times=n.unvaccinated.case),
                      rep(1, times=n.vaccinated.control),
                      rep(0, times=n.unvaccinated.control))
  df <- data.frame(covid, vaccine.status)

 
  ### compute some values
  prevalence <- round(n.case / (n.case+ n.control)*100,2)

  prevalence.unexposed <- round(n.unvaccinated.case/n.case*100,2)
  
  risk.vax <- n.vaccinated.case / (n.vaccinated.case + n.vaccinated.control)
  risk.unvax <- n.unvaccinated.case/ (n.unvaccinated.case + n.unvaccinated.control)
  rr <- risk.vax / risk.unvax
  cVE <- round((1- rr) *100,2)
  
  # LOGISTIC REGRESSION VE
  mod.log <- glm(covid ~ vaccine.status, family="binomial")
  log.ve <- round((1-exp(mod.log$coefficients))*100,2)[2]
  
  log.ve.ci <- 1-(exp(confint(mod.log)))[2,]
  log.ve.ci <- paste0("(", round(log.ve.ci[2]*100,2), " - ",round(log.ve.ci[1]*100,2), ")")
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
  ve.pois <-  round((1-mod.pois)*100,2)
  pois.out <- paste(ve.pois[1], paste0("(", ve.pois[3], " - ", ve.pois[2], ")"))
  
  mod.nb <- MASS::glm.nb(covid ~ vaccine.status)
  ve.nb <- round((1-exp(mod.nb$coefficients))*100,2)[2]
  nb.ci <- round((1-(exp(confint(mod.nb))[2,]))*100,2)
  nb.out <- paste(ve.nb[1], paste0("(", nb.ci[2], " - ", nb.ci[1], ")"))
  
  
  out <- data.frame(
    "Prevalence" = prevalence,
    "Prevalence Unexposed" = prevalence.unexposed,
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
      Prevalence = md("**Prevalence of COVID-19**"),
      Prevalence.Unexposed = md("**Prevalence of COVID-19 Among Unvaccinated**"),
      Actual.VE = md("*Actual*"),
      Logistic.VE = md("*Logistic*"),
      Poisson.VE = md("*Poisson w/REV*"),
      Negative.Binomial.VE = md("*Neg Bin*"),
    ) -> out.tab
  
  out.tab %>%
    cols_width(
      1 ~ px(110),
      2 ~ px(110),
      3 ~ px(90),
      everything() ~ px(175)
    ) %>%
    tab_spanner(
      label = md("**Vaccine Effectiveness Estimates, Regression**"),
      columns = c(
        3:6)
    )-> out.tab
  
  
  return(list(df, out.tab))
}



