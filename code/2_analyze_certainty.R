## Written by: Bryan Parthum; bparthum@gmail.com ; February 2020

###################################################################################
#############################   CERTAINTY ADJUSTMENT    ###########################
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
d1 <-  mlogit.data(data,
                  id.var='ind_id',
                  chid.var = 'group_id',
                  choice='choice_certainty_1',
                  shape='long',
                  alt.var='alt',
                  opposite=c('cost'))

## Create mlogit data
d2 <-  mlogit.data(data,
                   id.var='ind_id',
                   chid.var = 'group_id',
                   choice='choice_certainty_2',
                   shape='long',
                   alt.var='alt',
                   opposite=c('cost'))

####################################################
########################################   WTP Space
####################################################

## SET RANDOM PARAMETERS AND STARTING MATRIX
ranp_n <- 7
start <- c(1,rep(0,7),rep(0,.5*ranp_n*(ranp_n+1)),0,0)

## RECODED IF CERTAINTY = 'NOT VERY'
wtp.cert.1 <-  gmnl(choice_certainty_1 ~ cost + asc + distance + fish_div + fish_pop + algal_blooms + nitro_target | 0 | 0 | 0 | 1,
                   data=d1,
                   ranp=c(cost='ln',asc='n',distance='n',fish_div='n',fish_pop='n',algal_blooms='n',nitro_target='n'),
                   R=500,
                   model='gmnl',
                   halton=NA,
                   panel=T,
                   correlation=T,
                   seed=42,
                   fixed = c(T,rep(F,36),T),
                   start = start,
                   method = "bhhh",
                   iterlim = 500)
summary(wtp.cert.1)

## RECODED IF CERTAINTY = 'NOT VERY' or 'SOMEWHAT'
wtp.cert.2 <-  gmnl(choice_certainty_2 ~ cost + asc + distance + fish_div + fish_pop + algal_blooms + nitro_target | 0 | 0 | 0 | 1,
                    data=d2,
                    ranp=c(cost='ln',asc='n',distance='n',fish_div='n',fish_pop='n',algal_blooms='n',nitro_target='n'),
                    R=500,
                    model='gmnl',
                    halton=NA,
                    panel=T,
                    correlation=T,
                    seed=42,
                    fixed = c(T,rep(F,36),T),
                    start = start,
                    method = "bhhh",
                    iterlim = 500)
summary(wtp.cert.2)

## END OF SCRIPT. Have a nice day! 