library(OpenMx)

dat <- read.csv("pisa_domrep_MPLUS_final.csv", header = FALSE)
names(dat) <- c("schoolid", "vid", "dl1", "dl2", 
                 "isc", "ils", "hwc")
dat[ dat == -999 ] <- NA

upper <- dat[!duplicated(dat$schoolid), c('schoolid'), drop=FALSE]

paths <- list(
  mxPath("fun", c("vid", "dl1", "dl2"),
         values=1, free=c(F,T,T)),
  mxPath("sch", c("isc", "ils", "hwc"),
         values=1, free=c(F,T,T)),
  mxPath(c("fun","sch"), arrows=2, values=1),
  mxPath("fun","sch",arrows=2, values=.1),
  mxPath(c("vid", "dl1", "dl2", "isc", "ils", "hwc"),
         arrows=2, values=1))

ind <- c("vid", "dl1", "dl2", "isc", "ils", "hwc")

bet <- mxModel(
  "bet", type="RAM",
  mxData(upper, 'raw', primaryKey = 'schoolid'),
  latentVars = c(ind, "fun","sch"),
  paths)

sch <- mxModel(
  "within", type="RAM", bet,
  mxData(dat, 'raw'),
  latentVars = c("fun","sch"),
  manifestVars = ind,
  mxPath('one', ind),
  paths,
  mxPath(paste0('bet.',ind), ind,
         free=FALSE, values=1, joinKey="schoolid"))

sch <- mxRun(sch)
summary(sch)   # 64426.04

#cat(deparse(round(coef(sch),4)))
mplus <- c(
  "within.A[2,7]" = 1.151, "within.A[3,7]" = 1.06, 
  "within.A[5,8]" = 1.041,  "within.A[6,8]" = 0.752, 
  "within.S[1,1]" = 0.698, "within.S[2,2]" = 0.429,  
  "within.S[3,3]" = 0.778, "within.S[4,4]" = 0.501, 
  "within.S[5,5]" = 0.385,  "within.S[6,6]" = 1.124, 
  "within.S[7,7]" = 1.058, "within.S[7,8]" = 0.693,  
  "within.S[8,8]" = 1.21, "within.M[1,1]" = 3.226, 
  "within.M[1,2]" = 3.041,  "within.M[1,3]" = 2.866, 
  "within.M[1,4]" = 3.057, "within.M[1,5]" = 2.986,  
  "within.M[1,6]" = 3.105, "bet.A[2,7]" = 0.754, 
  "bet.A[3,7]" = 0.554,  "bet.A[5,8]" = 0.88, 
  "bet.A[6,8]" = 0.896, "bet.S[1,1]" = 0.017, 
  "bet.S[2,2]" = 0.0, "bet.S[3,3]" = 0.008, 
  "bet.S[4,4]" = 0.002,  "bet.S[5,5]" = 0.007, 
  "bet.S[6,6]" = 0.022, "bet.S[7,7]" = 0.331, 
  "bet.S[7,8]" = 0.269, "bet.S[8,8]" = 0.248)

w2 <- omxSetParameters(sch, labels = names(mplus), values=mplus)
w2 <- mxModel(w2, mxComputeOnce('fitfunction', 'fit'))
w2 <- mxRun(w2)
w2$output$fit  # 64433.95
