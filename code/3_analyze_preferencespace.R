## Written by: Bryan Parthum; bparthum@gmail.com ; February 2019

###################################################################################
##########################   PREFERENCE-SPACE MODELS    ###########################
###################################################################################

####################################################
########   Check for and load Packages   ###########
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("gmnl","mlogit","data.table") ## you can add more packages here
lapply(packages, pkgTest)

####################################################
#####################   Load and generate dataframes
####################################################

data <- readRDS('data.rds')

## Create mlogit data
d <-  mlogit.data(data,
                  id.var='ind_id',
                  chid.var = 'group_id',
                  choice='choice',
                  shape='long',
                  alt.var='alt',
                  opposite=c('cost'))

####################################################
############################################  POOLED
####################################################

pref.pooled <-  gmnl(choice ~ cost + asc + distance + fish_div + fish_pop + algal_blooms + nitro_target | 0 ,
                     data=d,
                     ranp=c(asc='n',distance='n',fish_div='n',fish_pop='n',algal_blooms='n',nitro_target='n'),
                     R=500,
                     model='mixl',
                     halton=NA,
                     panel=T,
                     correlation=T,
                     seed=42,
                     method = "bhhh",
                     iterlim = 500)
summary(pref.pooled)

####################################################
#################################  ASC HETEROGENEITY
####################################################

pref.asc <-  gmnl(choice ~ cost + asc + asc_rural + asc_water_knowledge +distance + fish_div + fish_pop + algal_blooms + nitro_target | 0 ,
                     data=d,
                     ranp=c(asc='n',asc_rural='n',asc_water_knowledge='n',distance='n',fish_div='n',fish_pop='n',algal_blooms='n',nitro_target='n'),
                     R=500,
                     model='mixl',
                     halton=NA,
                     panel=T,
                     correlation=T,
                     seed=42,
                     method = "bhhh",
                     iterlim = 500)
summary(pref.asc)

####################################################
#############################################  RURAL
####################################################

pref.rural <-  gmnl(choice ~ cost + asc + distance + fish_div + fish_pop + algal_blooms + nitro_target | 0 ,
                     data=d[d$rural_identify==1],
                     ranp=c(asc='n',distance='n',fish_div='n',fish_pop='n',algal_blooms='n',nitro_target='n'),
                     R=500,
                     model='mixl',
                     halton=NA,
                     panel=T,
                     correlation=T,
                     seed=42,
                     method = "bhhh",
                     iterlim = 500)
summary(pref.rural)

####################################################
#############################################  URBAN
####################################################

pref.urban <-  gmnl(choice ~ cost + asc + distance + fish_div + fish_pop + algal_blooms + nitro_target | 0 ,
                    data=d[d$rural_identify==0],
                    ranp=c(asc='n',distance='n',fish_div='n',fish_pop='n',algal_blooms='n',nitro_target='n'),
                    R=500,
                    model='mixl',
                    halton=NA,
                    panel=T,
                    correlation=T,
                    seed=42,
                    method = "bhhh",
                    iterlim = 500)
summary(pref.urban)

####################################################
###############################################  WTP
####################################################

pref.pooled.wtp <- wtp.gmnl(pref.pooled, wrt = "cost")
pref.asc.wtp <- wtp.gmnl(pref.asc, wrt = "cost")
pref.rural.wtp <- wtp.gmnl(pref.rural, wrt = "cost")
pref.urban.wtp <- wtp.gmnl(pref.urban, wrt = "cost")

## END OF SCRIPT. Have a nice day! 