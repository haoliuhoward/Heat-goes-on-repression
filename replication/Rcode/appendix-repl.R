########
# [Online Appendix] Replication script for "And the Heat Goes on: Policing Repression and The Modalities of Power"
#######

rm(list=ls(all=TRUE))
cat("\014")

# Load packages -------------------
source('replication/Rcode/loadPkg.R')
packs = c('tidyverse', 'xtable', 'estimatr', 'lmtest', 'lme4',"MASS", 'multiwayvcov', 'sandwich', 'lubridate',
          'texreg','ggplot2', 'RColorBrewer', 'extrafont')
loadPkg(packs)

# Read data ----------------------
load("replication/data/modData-repl.rda")

# standardize covariates
modData[,4:ncol(modData)] = modData[,4:ncol(modData)] %>% scale()


# Analysis (Appendix) ----------------

# A1. first stage reduced form (test strong instrument)
source("replication/Rcode/cluster_se.R")

lm1 <- lm(damage_decay ~ fault,data = modData)
vcov.lm1 <- cluster_se(model_result = lm1, data = modData, cluster = "munid")
standard.errors <- coeftest(lm1, vcov. = vcov.lm1)[,2]
p.values <- coeftest(lm1, vcov. = vcov.lm1)[,4]

lm2 <- lm(damage_decay ~ fault + timecount + timecount2 + timecount3, data = modData)
vcov.lm2 <- cluster_se(model_result = lm2, data = modData, cluster = "munid")
standard.errors_2 <- coeftest(lm2, vcov. = vcov.lm2)[,2]
p.values_2 <- coeftest(lm2, vcov. = vcov.lm2)[,4]

labs2 = c("Intercept", "Earthquake Fault Zone", "Time", "Time$^{2}$", "Time$^{3}")

texreg(list(lm1, lm2), include.ci = F,
       stars = c(0.01, 0.05, 0.10),
       override.se= list(standard.errors, standard.errors_2),
       override.p = list(p.values, p.values_2),
       digits=4,
       custom.coef.names = labs2,
       include.variance = F,
       include.dispersion = F,
       include.aic = T, include.bic = T,
       include.adjrs = T,
       include.nobs = T,
       include.loglik = T,
       include.groups = F,
       include.intercept = F,
       include.fstatistic = T,
       fontsize =  "footnotesize",
       custom.note = paste("%stars.", "Standard Error Clustered at the Municipal Level."), 
       caption.above = T,
       file = "replication/paper/reducedForm.tex") # intercept omitted 


## A2. Time cubics --------------------------------------------
iv = iv_robust(PoliceRepression_bin ~ damage_decay + 
                 CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop | fault +  
                 CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop, 
               alpha = 0.05,
               data = modData, se_type = "stata", clusters = munid)  

iv1 = iv_robust(PoliceRepression_bin ~ damage_decay + 
                  CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                  militarybase + militarybasespat + coppedge | fault +
                  CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                  militarybase + militarybasespat + coppedge , 
                alpha = 0.05,
                data = modData, se_type = "stata", clusters = munid)

iv2 = iv_robust(PoliceRepression_bin ~ damage_decay + PoliceRepression_sw + 
                  CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop + 
                  militarybase + militarybasespat + coppedge | fault + PoliceRepression_sw + 
                  CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                  militarybase + militarybasespat + coppedge , 
                alpha = 0.05,
                data = modData, se_type = "stata", clusters = munid) 

iv_s2sls = iv_robust(PoliceRepression_bin ~ damage_decay + 
                       PoliceRepression_sw + 
                       CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                       militarybase + militarybasespat + coppedge | fault +
                       CaAHPN_sw + MobyAHPN_sw + ViolAHPN_sw + Campaign_sw + lnpop_sw + coppedge_sw + militarybase_sw + militarybasespat_sw + 
                       CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                       militarybase + militarybasespat + coppedge , 
                     alpha = 0.05,
                     data = modData, se_type = "stata", clusters = munid)  

iv_s2sls_spline = iv_robust(PoliceRepression_bin ~ damage_decay + 
                              PoliceRepression_sw + 
                              CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                              militarybase + militarybasespat + coppedge +
                              timecount + timecount2 + timecount3 | fault +
                              CaAHPN_sw + MobyAHPN_sw + ViolAHPN_sw + Campaign_sw + lnpop_sw + coppedge_sw + militarybase_sw + militarybasespat_sw +
                              CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                              militarybase + militarybasespat + coppedge +
                              timecount + timecount2 + timecount3 ,
                            alpha = 0.05,
                            data = modData, se_type = "stata", clusters = munid) 

labs_spline = c("Intercept",
                "Infrastructural Damage", "Overt Challenges", "Mobilization Activities", "Campaign Activities", "Insurgent Violence",
                "Ln Population", "HQ Location", "HQ Distance", "Democratic Inclusion", "Spatial Lag", "Time", "Time$^{2}$","Time$^{3}$")


texreg(list(iv ,iv1, iv2, iv_s2sls,iv_s2sls_spline), include.ci = F,
       caption="IV Results (2SLS): Police Repression",
       label = "tab:main_spline",
       digits=4,
       stars = c(0.01, 0.05, 0.10),
       custom.model.names =c("2SLS","2SLS","2SLS",
                             "S-2SLS",
                             "S-2SLS") ,
       custom.coef.names = labs_spline,
       fontsize = "footnotesize",
       include.variance = F,
       include.dispersion = F,
       include.aic = T, include.bic = T,
       include.adjrs = T,
       include.nobs = T,
       include.loglik = T,
       include.groups = F,
       include.intercept = F,
       caption.above = T,
       include.fstatistic = T,
       custom.note = paste("%stars. ", "Standard Error Clustered at the Municipal Level."), 
       file = "replication/paper/main_spline.tex") # intercept omitted 


## A3. Year FE --------------------------------------------
iv.fe = iv_robust(PoliceRepression_bin ~ damage_decay + 
                    CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop | fault +  
                    CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop, 
                  alpha = 0.05,
                  data = modData, se_type = "stata", clusters = munid, fixed_effects = ~year)  

# time-invariant variable coppedge dropped
iv1.fe = iv_robust(PoliceRepression_bin ~ damage_decay + 
                     CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                     militarybase + militarybasespat | fault +
                     CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                     militarybase + militarybasespat, 
                   alpha = 0.05,
                   data = modData, se_type = "stata", clusters = munid, fixed_effects = ~year)

iv2.fe = iv_robust(PoliceRepression_bin ~ damage_decay + PoliceRepression_sw + 
                     CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop + 
                     militarybase + militarybasespat | fault + PoliceRepression_sw + 
                     CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                     militarybase + militarybasespat , 
                   alpha = 0.05,
                   data = modData, se_type = "stata", clusters = munid, fixed_effects = ~year) 

iv_s2sls.fe = iv_robust(PoliceRepression_bin ~ damage_decay + 
                          PoliceRepression_sw + 
                          CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                          militarybase + militarybasespat | fault +
                          CaAHPN_sw + MobyAHPN_sw + ViolAHPN_sw + Campaign_sw + lnpop_sw + coppedge_sw + militarybase_sw + militarybasespat_sw + 
                          CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                          militarybase + militarybasespat , 
                        alpha = 0.05,
                        data = modData, se_type = "stata", clusters = munid, fixed_effects = ~year)  

labs_FE = c(
  "Infrastructural Damage", "Overt Challenges", "Mobilization Activities", "Campaign Activities", "Insurgent Violence",
  "Ln Population", "HQ Location", "HQ Distance", "Spatial Lag")

texreg(list(iv.fe, iv1.fe, iv2.fe, iv_s2sls.fe), include.ci = F,
       caption="IV Results (2SLS): Police Repression (Year FE)", 
       label = "tab:main_yearFE",
       digits=4,
       stars = c(0.01, 0.05, 0.10),
       custom.model.names =c("2SLS","2SLS","2SLS",
                             "S-2SLS") ,
       custom.coef.names = labs_FE,
       fontsize = "footnotesize",
       caption.above = T,
       custom.note = paste("%stars. ", "Standard Error Clustered at the Municipal Level."),
       file = "replication/paper/main_yearFE.tex") 


## A.4 Interactive effect ------------------------------------
load("replication/data/modData-repl.rda")

# combining dissent
modData$dissent =  modData$CaAHPN + modData$MobyAHPN 

modData$damage_decayXdissent = modData$damage_decay*modData$dissent 

iv_int2 = iv_robust(PoliceRepression_bin ~ damage_decayXdissent + 
                      damage_decay + dissent +
                      lnpop | fault + 
                      damage_decay + dissent +
                      lnpop , 
                    alpha = 0.05,
                    data = modData, se_type = "stata", clusters = munid)

iv1_int2 = iv_robust(PoliceRepression_bin ~  damage_decayXdissent + 
                       damage_decay + dissent +
                       lnpop +
                       militarybase + militarybasespat + coppedge| fault +  
                       damage_decay + dissent +
                       lnpop +
                       militarybase + militarybasespat + coppedge, 
                     alpha = 0.05,
                     data = modData, se_type = "stata", clusters = munid)  

iv2_int2 = iv_robust(PoliceRepression_bin ~ damage_decayXdissent + 
                       damage_decay + dissent +
                       PoliceRepression_sw + 
                       lnpop +
                       militarybase + militarybasespat + coppedge| fault + PoliceRepression_sw + 
                       damage_decay + dissent+
                       lnpop +
                       militarybase + militarybasespat + coppedge, 
                     alpha = 0.05,
                     data = modData, se_type = "stata", clusters = munid) 

iv_s2sls_int2 = iv_robust(PoliceRepression_bin ~ damage_decayXdissent + 
                            damage_decay + dissent+
                            PoliceRepression_sw + 
                            lnpop +
                            militarybase + militarybasespat + coppedge| fault +
                            damage_decay + dissent+
                            CaAHPN_sw + MobyAHPN_sw + ViolAHPN_sw + Campaign_sw + lnpop_sw + coppedge_sw + militarybase_sw + militarybasespat_sw + 
                            lnpop +
                            militarybase + militarybasespat + coppedge, 
                          alpha = 0.05,
                          data = modData, se_type = "stata", clusters = munid)


labs_inter = c("Intercept", "Dissent x Infrastructural Damage",
               "Infrastructural Damage", "Dissent",
               "Ln Population", "HQ Location", "HQ Distance", "Democratic Inclusion", "Spatial Lag")

texreg(list(iv_int2, iv1_int2, iv2_int2, iv_s2sls_int2), include.ci = F,
       caption="IV Results (2SLS): Police Repression (Interactive Effect)", 
       label = "tab:main_inter",
       digits=4,
       stars = c(0.01, 0.05, 0.10),
       ci.force = F,
       custom.model.names =c("2SLS","2SLS","2SLS",
                             "S-2SLS") ,
       custom.coef.names = labs_inter,
       fontsize = "scriptsize",
       include.intercept = F,
       caption.above = T,
       include.fstatistic = T,
       custom.note = paste("%stars. ", "Standard Error Clustered at the Municipal Level."),
       file = "replication/paper/main_inter2.tex") 


## Figure A.1 Time trend ----------------------
df = modData %>% dplyr::select(year, PoliceRepression_bin) %>% group_by(year) %>% dplyr::summarise(repress.count = sum(PoliceRepression_bin))

df$year = paste0(df$year, "-01-01") %>% ymd(.)
par(mfrow=c(1,1), mar=c(0, 0, 0, 0))  # default margin

q <-ggplot(data=df, aes(x=year, y=repress.count, group=1) )+
  geom_line(color="blue") + theme(axis.text.x = element_text(angle = 30)) + ylab("Repression Count") + theme_bw()
q

png(filename="replication/paper/time-series.png",width = 800, height = 500, res = 100)
q
dev.off()
