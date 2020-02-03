library(OpenMx)

dat <- read.csv("pisa_domrep_MPLUS_final.csv", header = FALSE)
names(dat) <- c("schoolid", "vid", "dl1", "dl2", 
                 "isc", "ils", "hwc")
dat[ dat == -999 ] <- NA

upper <- dat[!duplicated(dat$schoolid), c('schoolid'), drop=FALSE]

paths <- list(
  mxPath(c("vid", "dl1", "dl2", "isc", "ils", "hwc"),
         arrows=2, values=1))

ind <- c("vid", "dl1", "dl2", "isc", "ils", "hwc")

bet <- mxModel(
  "bet", type="RAM",
  mxData(upper, 'raw', primaryKey = 'schoolid'),
  latentVars = c(ind, "dig"),
  mxPath('dig', ind, free=c(F,rep(T,5)), values=1),
  mxPath('dig', arrows=2, values=1),
  paths)

sch <- mxModel(
  "within", type="RAM", bet,
  mxData(dat, 'raw'),
  latentVars = c("fun","sch"),
  manifestVars = ind,
  mxPath('one', ind),
  mxPath("fun", c("vid", "dl1", "dl2"),
         values=1, free=c(F,T,T)),
  mxPath("sch", c("isc", "ils", "hwc"),
         values=1, free=c(F,T,T)),
  mxPath(c("fun","sch"), arrows=2, values=1),
  mxPath("fun","sch",arrows=2, values=.1),
  paths,
  mxPath(paste0('bet.',ind), ind,
         free=FALSE, values=1, joinKey="schoolid"))

sch <- mxRun(sch)
summary(sch)
