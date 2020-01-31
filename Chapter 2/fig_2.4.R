library(OpenMx)

dat <- read.csv("03-data-unstd.csv", header=FALSE)
colnames(dat) <- c('country', 'year', 'sev', 'inc', 'edu', 'age', 'gini', 'gdpcap')
dat$gdp <- log(dat$gdpcap * 1000)

upper <- dat[!duplicated(dat$country),
             c('country','gini','gdpcap','gdp')]

bet <- mxModel(
  "bet", type="RAM",
  mxData(upper, 'raw', primaryKey = 'country'),
  latentVars = c("gdp", "edu", "sev"),
  mxPath("one", "gdp", free=FALSE, labels="data.gdp"),
  mxPath("gdp", c("edu","sev"),
         labels=paste0(c("edu","sev"),"_on_gdp")),
  mxPath("one", c("edu","sev"),
         labels=paste0(c("edu","sev"),"_int")),
  mxPath(c("edu","sev"), arrows=2, connect="unique.pairs",
         values=c(1,0,1),
         labels=c("edu_bvar","sev_with_edu","sev_bvar")))

w1 <- mxModel(
  "within", type="RAM", bet,
  mxData(dat, 'raw'),
  manifestVars = c('inc','edu','sev'),
  latentVars = c('age'),
  mxPath('one', 'inc', labels="inc_int"),
  mxPath('one', 'age', free=FALSE, labels='data.age'),
  mxPath(c('inc','sev','edu'), arrows=2, values=1,
         labels=paste0(c('inc','sev','edu'),'_var')),
  mxPath(c('inc','age','edu'), 'sev',
         labels=paste0('sev_on_',c('inc','age','edu'))),
  mxPath('age','edu', labels='edu_on_age'),
  mxPath(c('age','edu'), 'inc',
         labels=paste0('inc_on_',c('age','edu'))),
  mxPath('bet.edu', 'edu', free=FALSE, values=1, joinKey="country"),
  mxPath('bet.sev', 'sev', free=FALSE, values=1, joinKey="country"))

wfit <- mxRun(w1)

summary(wfit)  # 460384.2

# Mplus seems worse?
#cat(deparse(round(coef(wfit),2)))
mplus <- c(sev_on_inc = 0.036, inc_on_edu = 0.422, sev_on_edu = 0.049,
           inc_on_age = 0.003,  edu_on_age = -0.297, sev_on_age = -0.033,
           inc_var = 5.231, edu_var = 4.189,  sev_var = 0.441, 
           inc_int = 4.204, edu_on_gdp = -0.046, sev_on_gdp = 0.318,
           edu_bvar = 0.537, sev_with_edu = -0.045, sev_bvar = 0.1,
           edu_int = 5.05,  sev_int = -2.8)

w2 <- omxSetParameters(w1, labels = names(mplus), values=mplus)
w2 <- mxModel(w2, mxComputeOnce('fitfunction', 'fit'))
w2 <- mxRun(w2)
w2$output$fit  # 489917.3
