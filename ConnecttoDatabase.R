# ========================= Load libraries===============================================
library("ggplot2");library("RODBC");library("dplyr");library("plotly");library(MASS)
# library("DBI");library("odbc");library("lubridate")

# =========================  Connect to databases =======================================
db = odbcConnect("Production")
qry = "SELECT Produit,Primenette,CapitalAssure,police FROM Policeassurance"
tblprod = sqlQuery(db,qry)

# ========================== Manipulate data  ===========================================
tb = tblprod %>%
  filter(Produit == 1212) %>%
  mutate(taux = Primenette/CapitalAssure * 1000) %>% 
  filter(CapitalAssure >0 & CapitalAssure <600e+06) %>% 
  filter(taux>0 & taux<10)

# ========================== Summarise ==================================================
summary(tb$taux)
summarise

# ========================== Graphic=====================================================
p = ggplot(data = tb, aes(x = CapitalAssure, y = taux)) +
  geom_point(aes(text = paste("Police:", police),alpha=0.1,position = "jitter"))+
  geom_smooth(method = "lm")
plotly::ggplotly(p ,tooltip = "text")
# ========================== Graphic with Polty =========================================
# fit =  fitdistr(tb$taux, "lognormal")
# ggplot(data = tb, aes(x=taux)) +
#   geom_density(colour="black", fill="blue",alpha = 0.4)+
#   stat_function(fun = dlnorm, size = 1, color = "red",
#               args = list(mean = 0.8, sd = 0.8))
# fit
odbcClose(db)


