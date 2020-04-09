  final <- read.csv("MODIS_activefire/99_summary/final_fire_spei_year_eco.csv")
  d_model <- subset(final,fire_type=="new" & year!=2019 & year!=2002)
  policy_threshold = 2008
  d_model$policy <- NA
  d_model$policy[d_model$year <=policy_threshold] <- "no"
  d_model$policy[d_model$year >policy_threshold] <- "yes"
  d_model$policy <- as.factor(d_model$policy)
  m_af_brazil <- lm(data=subset(d_model,id_country =="brazil"),N_km  ~ spei + policy)
  m_af_amazon <- lm(data=subset(d_model,id_country =="amazon"),N_km  ~ spei + policy)
  final <- read.csv("MODIS_burntArea/99_summary/final_fire_spei_year_eco.csv")
  d_model <- subset(final,fire_type=="new" & year!=2019 & year!=2002)
  policy_threshold = 2008
  d_model$policy <- NA
  d_model$policy[d_model$year <=policy_threshold] <- "no"
  d_model$policy[d_model$year >policy_threshold] <- "yes"
  d_model$policy <- as.factor(d_model$policy)
  m_ba_brazil <- lm(data=subset(d_model,id_country =="brazil"),N_km  ~ spei + policy)
  m_ba_amazon <- lm(data=subset(d_model,id_country =="amazon"),N_km  ~ spei + policy)
  
  summary(m_ba_brazil)
  summary(m_ba_amazon)
  summary(m_af_brazil)
  summary(m_af_amazon)





