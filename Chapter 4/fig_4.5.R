library(OpenMx)

mxOption(key='Number of Threads', value=parallel::detectCores() - 1L)

dat <- read.table("WERS_Mplus_final.csv", header = FALSE)
names(dat) <- c("compid", "persid", "hw1", "hw2", "hw3", 
                 "re1", "re2", "skl", "pay",  "mnemps")
dat[ dat == -999 ] <- NA
dat$nem <- log(dat$mnemps)

upper <- dat[!duplicated(dat$compid), c('compid', 'nem'), drop=FALSE]

mkCommonPaths <- function(m) {
  list(
    mxPath("res", paste0('re',1:2),
           values=1, free=c(F,T),
           labels=paste0('res_',m,'by_re',1:2)),
    mxPath("har", paste0('hw',1:3),
           values=1, free=c(F,T,T),
           labels=paste0('har_',m,'by_hw',1:3)),
    mxPath(c('res','pay','har'), 'skl',
           labels=paste0('skl_',m,'on_',c('res','pay','har'))),
    mxPath(c('res','pay'), 'har',
           labels=paste0('har_',m,'on_',c('res','pay'))),
    mxPath(c(paste0('hw',1:3), paste0('re',1:2),'skl'), arrows=2, values=1,
           labels=paste0(c(paste0('hw',1:3), paste0('re',1:2),'skl'),'_',m,'var')),
    mxPath(c('res','pay','har'), arrows=2, values=1,
           labels=paste0(c('res','pay','har'), '_',m,'var'))
  )
}

bet <- mxModel(
  "bet", type="RAM",
  mxData(upper, 'raw', primaryKey = 'compid'),
  manifestVars = 'nem',
  latentVars = c(paste0('hw',1:3), paste0('re',1:2), 'pay', 'skl',
                 "res","har"),
  mxPath(c('nem','pay'),'skl', labels=paste0('skl_bon_',c('nem','pay'))),
  mxPath('pay','nem', arrows=2, labels="pay_with_nem"),
  mxPath('one', c('skl','pay','nem'),
         labels=paste0(c('skl','pay','nem'), "_int")),
  mxPath(c('skl','pay','nem'),arrows=2,
         labels=paste0(c('skl','pay','nem'), "_bvar")),
  mxPath('nem', c("res","har"), labels=paste0(c("res","har"),'_bon_nem')),
  mxPath('one', c(paste0('hw',1:3), paste0('re',1:2)),
         labels=paste0(c(paste0('hw',1:3), paste0('re',1:2)), '_int')),
  mkCommonPaths('b'))

skl <- mxModel(
  "within", type="RAM", bet,
  mxData(dat, 'raw'),
  latentVars = c("res","har"),
  manifestVars = c(paste0('hw',1:3), paste0('re',1:2), 'pay','skl'),
  mxPath('res','pay',arrows=2, labels="res_with_pay"),
  mxPath(paste0('bet.',c(paste0('hw',1:3), paste0('re',1:2), 'pay','skl')),
         c(paste0('hw',1:3), paste0('re',1:2), 'pay','skl'),
         free=FALSE, values=1, joinKey="compid"),
  mxPath('one','pay',free=FALSE),
  mkCommonPaths(''))

skl <- mxRun(skl)
summary(skl)
