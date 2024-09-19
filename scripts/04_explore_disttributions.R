################################################################################
################# WA Env Health GLMM Analysis ##########################
#################      Step 3: Explore Data      ###############################
################################################################################
# packages

library(pacman)
p_load(car,MASS,lme4,readr,here)

# read in count and covariate data 
all_dat <- read_csv(here("data", "covariates", "WIDE_COUNTS_ALL_ENV_URB_SITES_1000m.csv"))

# read in richness/diversity data 
all_dat_veg <- read_csv(here("data", "covariates", "vegan_sites_all_covs.csv"))

colnames(all_dat_veg)
hist(all_dat_veg$shannon.di)
hist(all_dat_veg$richness)





all_dat$Raccoon <- all_dat$Raccoon+0.00001

# look at distribution of data 

qqp(all_dat$Raccoon, "norm")
qqp(all_dat$Raccoon, "lnorm")

# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I
# have shown below.

nbinom <- fitdistr(all_dat$Raccoon, "Negative Binomial")
qqp(all_dat$Raccoon, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr(all_dat$Raccoon, "Poisson")
qqp(all_dat$Raccoon, "pois",  lambda = poisson$estimate[[1]])

all_dat$Raccoon2 <- all_dat$Raccoon+0.00001
gamma <- fitdistr(all_dat$Raccoon2, "gamma")
qqp(all_dat$Raccoon2, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

# Raccoon seems to fit negative binomial best 


nbinom <- fitdistr(all_dat$Coyote, "Negative Binomial")
qqp(all_dat$Coyote, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr(all_dat$Coyote, "Poisson")
qqp(all_dat$Coyote, "pois",  lambda = poisson$estimate[[1]])

all_dat$Coyote2 <- all_dat$Coyote+0.00001
gamma <- fitdistr(all_dat$Coyote2, "gamma")
qqp(all_dat$Coyote, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

# COyote seems to fit negative binomial best -- minus some major outliers 






# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I
# have shown below.

# diversity qqplots 

nbinom <- fitdistr(all_dat_veg$shannon.di, "Negative Binomial")
qqp(all_dat_veg$shannon.di, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# negative binom is not appropriate bc we don't have integers 

poisson <- fitdistr(all_dat_veg$shannon.di, "Poisson")
qqp(all_dat_veg$shannon.di, "pois", lambda = poisson$estimate[[1]])
# same w poisson 


all_dat_veg$shannondi2 <- all_dat_veg$shannon.di+0.00001
gamma <- fitdistr(all_dat_veg$shannondi2, "gamma")
qqp(all_dat_veg$shannondi2, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

?qqp
