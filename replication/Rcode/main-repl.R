########
# Replication script for "And the Heat Goes on: Policing Repression and The Modalities of Power"
#######

rm(list=ls(all=TRUE))
cat("\014")

# Load packages -------------------
source('replication/Rcode/loadPkg.R')
packs = c('tidyverse', 'xtable', 'estimatr', 'texreg','ggplot2', 'RColorBrewer', 'extrafont')

loadPkg(packs)


# Read data ----------------------
load("replication/data/modData-repl.rda")


# Descriptive Stats -------------------

# 1) table 1
vars = c("PoliceRepression_bin", "fault", "damage_decay",
         "CaAHPN", "MobyAHPN", "Campaign","ViolAHPN",
         "coppedge", "lnpop", "militarybase", "militarybasespat")

stats = modData %>% dplyr::select(vars) %>% as.data.frame()

stats_tab = brotools::describe(stats) %>%  dplyr::select(variable, mean ,sd, min, max) %>% as.data.frame()

# change names
stats_tab$variable = stats_tab$variable %>% str_replace(., "PoliceRepression_bin", "Police Repression") %>%
  str_replace(., "CaAHPN", "Overt Challenges") %>%
  str_replace(., "MobyAHPN", "Mobilization Activities") %>%
  str_replace(., "Campaign", "Campaign Activity") %>%
  str_replace(., "damage_decay", "Infrastructural Damage") %>%
  str_replace(., "militarybasespat", "HQ Distance") %>%
  str_replace(., "militarybase", "HQ Location") %>%
  str_replace(., "fault", "Motagua Fault Line") %>%
  str_replace(., "ViolAHPN", "Insurgent Violence") %>%
  str_replace(., "coppedge", "Democratic Inclusion") %>%
  str_replace(., "lnpop", "Ln Population")

# change order
stats_tab = stats_tab[c(10, 5, 4, 1, 9, 2, 11, 6, 7, 8, 3),]
rownames(stats_tab) = NULL

tab = xtable(stats_tab, type = "html", digits = 2, caption = "Descriptive Statistics" )
tab

print.xtable(tab, scalebox=1, type = "latex",caption.placement = "top", include.rownames=TRUE ,
             file = paste0("replication/paper/des_stats.tex"), size = "footnotesize" )


# 2) Figure 1
# Load mapping data 
load('replication/data/mapData.rda')

pal3 <- colorRampPalette(c("white", "red")) # Create the color palette and breaks
breaks <- c(quantile(damage_data$damage))

q <- ggplot(data = polygon_data, aes(x = long, y = lat, group = group))
q <- q + geom_polygon(color = "lightgrey", aes(fill = factor(damage))) 
q <- q + ggtitle("Blue Circles: Epicenters, Black Line: Motagua Fault")
q <- q + labs(fill="Damage")
q <- q + scale_fill_manual( values = pal3(19), breaks= breaks )

# add new layer (point data) 
q <- q + geom_point(aes(x = longitude, y = latitude, group = NULL), data = point_data, color = alpha("blue", .35), size = point_data$mag) 
q <- q + geom_point(aes(x = longitude, y = latitude, group = NULL), data = point_data, color = alpha("blue", .35), size = 0.2) 

# add fault data
q <- q + geom_line(aes(x = long, y = lat, group=NULL), data = polyline_fault_data, color = alpha("black", .75), size =1.5)
q <- q + theme(text = element_text(size=10,  family="serif"))
q

png(filename='replication/paper/map_quake_muni2.png' ,width = 600, height = 500, res= 100)
q
dev.off()


# Analysis (Manuscript) ----------------

modData[,4:ncol(modData)] = modData[,4:ncol(modData)] %>% scale() # standardize covariates

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
                       CaAHPN_sw + MobyAHPN_sw + ViolAHPN_sw + Campaign_sw + lnpop_sw + coppedge_sw + militarybase_sw + militarybasespat_sw + # instruments for the spatial lag of DV
                       CaAHPN + MobyAHPN + Campaign + ViolAHPN + lnpop +
                       militarybase + militarybasespat + coppedge , 
                     alpha = 0.05,
                     data = modData, se_type = "stata", clusters = munid)  

labs = c("Intercept",
         "Infrastructural Damage", "Overt Challenges", "Mobilization Activities", "Campaign Activities", "Insurgent Violence","Ln Population",
         "HQ Location", "HQ Distance", "Democratic Inclusion", "Spatial Lag")

texreg(list(iv ,iv1, iv2, iv_s2sls), include.ci = F,
       caption="IV Results (2SLS): Police Repression", 
       label = "tab:main",
       digits=4,
       stars = c(0.01, 0.05, 0.10),
       custom.model.names =c("2SLS","2SLS","2SLS",
                             "S-2SLS") ,
       custom.coef.names = labs,
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
       file = "replication/paper/main.tex") # intercept omitted 

