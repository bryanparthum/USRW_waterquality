# Written by: Bryan Parthum; bparthum@gmail.com ; Feb 2020

###################################################################################
##########################     1) Table 2 - WTP-Space    ##########################
##########################     2) Appendix - Certainty Adjustment  ################
##########################     3) Appendix - Preference Space  ####################
##########################     4) Appendix - WTP from Preference Space  ###########
###################################################################################

####################################################
#####################################   PACKAGE SHOP
####################################################

# install.packages('data.table')
# install.packages('stargazer')

####################################################
########   Check for and load Packages   ###########
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = T))
  {
    install.packages(x, dep = T)
    if(!require(x, character.only = T)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("data.table","stargazer",'memisc') ## you can add more packages here
lapply(packages, pkgTest)

####################################################
#####################   Load and generate dataframes    
####################################################

## Set working directory
setwd("C:/Users/parthum2/Box/Sangamon/analysis/analyze")

## LOAD NULL MODELS
null_pooled <- readRDS(file="estimates/null_pooled.rds")
null_rural  <- readRDS(file="estimates/null_rural.rds")
null_urban  <- readRDS(file="estimates/null_urban.rds")

## LOAD WTP-SPACE MODELS
wtp_pooled  <- readRDS(file="estimates/wtp_pooled.rds")
wtp_asc     <- readRDS(file="estimates/wtp_asc.rds")
wtp_rural   <- readRDS(file="estimates/wtp_rural.rds")
wtp_urban   <- readRDS(file="estimates/wtp_urban.rds")

## LOAD CERTAINTY ADJUSTMENT MODELS
wtp_cert1   <- readRDS(file="estimates/wtp_certainty_1.rds")
wtp_cert2   <- readRDS(file="estimates/wtp_certainty_2.rds")

## LOAD PREFERENCE SPACE MODELS
pref_pooled <- readRDS(file="estimates/pref_pooled.rds")
pref_asc    <- readRDS(file="estimates/pref_asc.rds")
pref_rural  <- readRDS(file="estimates/pref_rural.rds")
pref_urban  <- readRDS(file="estimates/pref_urban.rds")

## LOAD PREFERENCE SPACE WTP 
pref_pooled_wtp <- readRDS(file="estimates/pref_pooled_wtp.rds")
pref_asc_wtp    <- readRDS(file="estimates/pref_asc_wtp.rds")
pref_rural_wtp  <- readRDS(file="estimates/pref_rural_wtp.rds")
pref_urban_wtp  <- readRDS(file="estimates/pref_urban_wtp.rds")

####################################################
######################################  MAIN RESULTS
####################################################

## TESTS
lrt_wtp_pooled <- round(lrtest(wtp_pooled, null_pooled)[2,4],2)
lrt_wtp_rural <- round(lrtest(wtp_rural, null_rural)[2,4],2)
lrt_wtp_urban <- round(lrtest(wtp_urban, null_urban)[2,4],2)
lrt_wtp_asc <- round(lrtest(wtp_asc, null_pooled)[2,4],2)

lrt <- round(2*(as.numeric(logLik(wtp_urban))+as.numeric(logLik(wtp_rural))-as.numeric(logLik(wtp_pooled))),2)
qchisq(p = 0.95, df = 63)
lrt > qchisq(p = 0.95, df = 63)
pchisq(lrt, 63,lower.tail=F)

lrt.asc <- round(2*(as.numeric(logLik(wtp_asc))-as.numeric(logLik(wtp_pooled))),2)
qchisq(p = 0.95, df = 16)
lrt.asc > qchisq(p = 0.95, df = 16)
pchisq(lrt, 16,lower.tail=F)

mcf_pooled = round(1-(as.numeric(logLik(wtp_pooled))/as.numeric(logLik(null_pooled))),2)
mcf_rural = round(1-(as.numeric(logLik(wtp_rural))/as.numeric(logLik(null_rural))),2)
mcf_urban = round(1-(as.numeric(logLik(wtp_urban))/as.numeric(logLik(null_urban))),2)
mcf_asc = round(1-(as.numeric(logLik(wtp_asc))/as.numeric(logLik(null_pooled))),2)

wtp_space <- mtable("Full Sample"=wtp_pooled,"ASC Heterogeneity"=wtp_asc,"Rural"=wtp_rural,"Urban"=wtp_urban,
               digits=2,
               summary.stats = c("N","Log-likelihood","AIC",'BIC'))
write.mtable(wtp_space, file = "output/tables/wtp_space.doc", format='delim')
lrt
lrt.asc
mcf_pooled
mcf_asc
mcf_rural
mcf_urban

####################################################
#############################  CERTAINTY ADJUSTMENTS
####################################################

## TESTS
lrt_wtp_pooled <- round(lrtest(wtp_pooled, null_pooled)[2,4],2)
lrt_wtp_cert1 <- round(lrtest(wtp_cert1, null_pooled)[2,4],2)
lrt_wtp_cert2 <- round(lrtest(wtp_cert2, null_pooled)[2,4],2)

mcf_pooled = round(1-(as.numeric(logLik(wtp_pooled))/as.numeric(logLik(null_pooled))),2)
mcf_cert1 = round(1-(as.numeric(logLik(wtp_cert1))/as.numeric(logLik(null_pooled))),2)
mcf_cert2 = round(1-(as.numeric(logLik(wtp_cert2))/as.numeric(logLik(null_pooled))),2)

wtp_certainty <- mtable("No Adjustment"=wtp_pooled,"Cartainty Adj. 1"=wtp_cert1,"Cartainty Adj. 2"=wtp_cert2,
                    digits=2,
                    summary.stats = c("N","Log-likelihood","AIC"))
write.mtable(wtp_certainty, file = "output/tables/wtp_certainty.doc", format='delim')
mcf_pooled
mcf_cert1
mcf_cert2

####################################################
##################################  PREFERENCE SPACE
####################################################

## TESTS PREFERENCE-SPACE
pref_lrt_wtp_pooled <- round(lrtest(pref_pooled, null_pooled)[2,4],2)
pref_lrt_wtp_asc <- round(lrtest(pref_asc, null_pooled)[2,4],2)
pref_lrt_wtp_rural <- round(lrtest(pref_rural, null_rural)[2,4],2)
pref_lrt_wtp_urban <- round(lrtest(pref_urban, null_urban)[2,4],2)

pref_lrt <- round(2*(as.numeric(logLik(pref_urban))+as.numeric(logLik(pref_rural))-as.numeric(logLik(pref_pooled))),2)
qchisq(p = 0.95, df = 63)
pref_lrt > qchisq(p = 0.95, df = 63)
pchisq(pref_lrt, 63,lower.tail=F)
pref_lrt

pref_mcf_pooled = round(1-(as.numeric(logLik(pref_pooled))/as.numeric(logLik(null_pooled))),2)
pref_mcf_asc = round(1-(as.numeric(logLik(pref_asc))/as.numeric(logLik(null_pooled))),2)
pref_mcf_rural = round(1-(as.numeric(logLik(pref_rural))/as.numeric(logLik(null_rural))),2)
pref_mcf_urban = round(1-(as.numeric(logLik(pref_urban))/as.numeric(logLik(null_urban))),2)

pref_space <- mtable("Full Sample"=pref_pooled,"ASC Heterogeneity"=pref_asc,"Rural"=pref_rural,"Urban"=pref_urban,
                    digits=4,
                    summary.stats = c("N","Log-likelihood","AIC",'BIC'))
write.mtable(pref_space, file = "output/tables/pref_space.doc", format='delim')
pref_mcf_pooled
pref_mcf_asc
pref_mcf_rural
pref_mcf_urban

####################################################
############################  PREFERENCE SPACE - WTP
####################################################

pref_space_wtp <- capture.output(stargazer(pref_pooled_wtp,pref_asc_wtp,pref_rural_wtp,pref_urban_wtp,
                                 title=c("Empirical Distributions of MWTP from Preference-space Models"),
                                 keep.stat=c("n","lr","aic",'bic','ll'),
                                 digits=2,digits.extra=0,no.space=T,align=F,notes.append=T,object.names=F,model.names=F,model.numbers=T,
                                 column.labels = c("Pooled",'ASC Heterogeneity','Rural',"Urban"),
                                 add.lines = list(c('AIC',round(AIC(pref_pooled),2),round(AIC(pref_asc),2),round(AIC(pref_rural),2),round(AIC(pref_urban),2)),
                                                  c('McF R2',pref_mcf_pooled,pref_mcf_asc,pref_mcf_rural,pref_mcf_urban),
                                                  c('LRT','pref_lrt','','','')),
                                 type='text',
                                 out="output/tables/pref_space_wtp.doc"))

####################################################
###############################  STANDARD DEVIATIONS
####################################################

vcov(wtp_pooled, what = "ranp", type = "sd", se = "true")
vcov(wtp_asc, what = "ranp", type = "sd", se = "true")
vcov(wtp_rural, what = "ranp", type = "sd", se = "true")
vcov(wtp_urban, what = "ranp", type = "sd", se = "true")

vcov(pref_pooled, what = "ranp", type = "sd", se = "true")
vcov(pref_asc, what = "ranp", type = "sd", se = "true")
vcov(pref_rural, what = "ranp", type = "sd", se = "true")
vcov(pref_urban, what = "ranp", type = "sd", se = "true")

####################################################
################################  CORELLATION MATRIX
####################################################

vcov(wtp_pooled, what = "ranp", type = "cor")
vcov(wtp_rural, what = "ranp", type = "cor")
vcov(wtp_urban, what = "ranp", type = "cor")


## END OF SCRIPT. Have a great day!