library(OpenMx)

dat <- read.table("WERS_Mplus_final.csv", header = FALSE)
names(dat) <- c("compid", "persid", "hw1", "hw2", "hw3", 
                 "re1", "re2", "skl", "pay",  "mnemps")
dat[ dat == -999 ] <- NA
dat$nem <- log(dat$mnemps)

upper <- dat[!duplicated(dat$compid), c('compid', 'nem'), drop=FALSE]

bet <- mxModel(
  "bet", type="RAM",
  mxData(upper, 'raw', primaryKey = 'compid'),
  latentVars = c('skl', 'nem'),
  mxPath('one','nem',free=FALSE,labels='data.nem'),
  mxPath('nem','skl', labels="skl_on_nem"),
  mxPath('one', 'skl', labels="skl_int"),
  mxPath('skl',arrows=2, labels="skl_bvar"))

skl <- mxModel(
  "within", type="RAM", bet,
  mxData(dat, 'raw'),
  latentVars = c("res","har"),
  manifestVars = c(paste0('hw',1:3), paste0('re',1:2), 'pay','skl'),
  mxPath("res", paste0('re',1:2),
         values=1, free=c(F,T), labels=paste0('res_by_re',1:2)),
  mxPath("har", paste0('hw',1:3),
         values=1, free=c(F,T,T), labels=paste0('har_by_hw',1:3)),
  mxPath(c(paste0('hw',1:3), paste0('re',1:2),'skl'), arrows=2, values=1,
         labels=paste0(c(paste0('hw',1:3), paste0('re',1:2),'skl'),'_var')),
  mxPath(c('res','pay','har'), arrows=2, values=1,
         labels=paste0(c('res','pay','har'), '_var')),
  mxPath('res','pay',arrows=2, labels="res_with_pay"),
  mxPath(c('res','pay'), 'har',
         labels=paste0('har_on_',c('res','pay'))),
  mxPath(c('res','pay','har'), 'skl',
         labels=paste0('skl_on_',c('res','pay','har'))),
  mxPath('one', c(paste0('hw',1:3), paste0('re',1:2), 'pay'),
         labels=paste0(c(paste0('hw',1:3), paste0('re',1:2), 'pay'), '_int')),
  mxPath('bet.skl', 'skl', free=FALSE, values=1, joinKey="compid"),
  mxAlgebra(A[7,9] * A[9,8], name="ie"),
  mxAlgebra(ie + A[7,8], name="total"))

skl <- mxRun(skl)
summary(sch)
