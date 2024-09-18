################################################################################
#################  WA Env Health GLMM Analysis ####### #########################
################# Step 3: Calculate Species Richness, ##########################
#################     Diversity, & Abundance           #########################
#################   Yasmine Hentati yhentati@uw.edu    #########################
################################################################################



# packages
library(readr)
library(here)
library(tidyr)
library(vegan)
library(ggplot2)
library(viridis)
library(cowplot)
library(dplyr)
library(tibble)

# read in count data
counts <- read_csv(here("data", "wa_counts.csv"))

# add together all occurrences of each spp per site (for grouped spp) 
counts_sum <- counts %>% group_by(species, site, city, season) %>% summarise(ySum = sum(count))
counts_sum


# pivot into wide format 
wide_counts <- counts_sum %>% pivot_wider(names_from = "species", values_from = "ySum")
wide_counts

# replace all NAs with 0s
wide_counts <- wide_counts %>% mutate_all(~replace(., is.na(.), 0))
wide_counts

# keep first unique site/season combo
counts_all <- wide_counts %>% distinct(site, season, .keep_all = TRUE)
colnames(counts_all)
glimpse(counts_all)
# pivot back to long format 
counts_long <- pivot_longer(counts_all, cols = 4:20, names_to = "species")
head(counts_long)

###############################################################################




# using vegan

# with this method, we will get rid of seasons altogether 
counts_year <- counts_long  %>% dplyr::select(-season) %>% 
  group_by(site, species) %>% summarize(value = sum(value))
glimpse(counts_year)

# pivot to wide 
counts_wide_yr <- counts_year %>% pivot_wider(names_from = "species", 
                                              values_from = "value")
glimpse(counts_wide_yr)

# calculate richness
richness <- specnumber(counts_wide_yr) 
counts_wide_yr$richness <- richness
counts_wide_yr

# shannon's div
div_vegan <- counts_year %>%
  group_by(site) %>% 
  summarise(N=sum(value),
            shannon.di=diversity(value, index = "shannon", MARGIN = 2),
            simpson.di=diversity(value, index = "simpson", MARGIN = 2),
            inv.simpson.di=diversity(value, index = "invsimpson", MARGIN = 2)) %>%
  arrange(-shannon.di)

# bind together
rich_vegan <- counts_wide_yr %>% dplyr::select(site, richness)

vegandf <- left_join(div_vegan, rich_vegan, by = "site")

# pielou's evenness
evenness <- vegandf$shannon.di/log(vegandf$richness)
vegandf$evenness <- evenness

write_csv(vegandf, here("data", "vegan_sites.csv"))



##### Import results into data set 

# bind to covariates

sitecovs <- read_csv(here("data", "covariates", "ALL_ENV_URB_SITES_1000m.csv"))
sewa_sitecovs <- read_csv(here("data", "covariates", "SEWA_ALL_ENV_URB_SITES_1000m.csv"))
tawa_sitecovs <- read_csv(here("data", "covariates", "TAWA_ALL_ENV_URB_SITES_1000m.csv"))

vegandf <- left_join(vegandf, sitecovs, by = "site")

sewa_vegandf <- vegandf %>% filter(city == "sewa") %>%
  left_join(sewa_sitecovs, by = "site")

tawa_vegandf <- vegandf %>% filter(city == "tawa") %>%
  left_join(tawa_sitecovs, by = "site")

write_csv(vegandf, here("data", "covariates", "vegan_sites_all_covs.csv"))
write_csv(sewa_vegandf, here("data", "covariates", "sewa_vegan_sites_all_covs.csv"))
write_csv(sewa_vegandf, here("data", "covariates", "tawa_vegan_sites_all_covs.csv"))

# bind wide counts to covariates 
# add J as well 

counts_all <- left_join(counts_all, counts[,c("site","season", "days.active")], 
                        by = c("site","season"))

all_wide <- left_join(counts_all, sitecovs, by = c("city", "site")) %>%
  distinct() 

write_csv(all_wide, here("data", "covariates", "WIDE_COUNTS_ALL_ENV_URB_SITES_1000m.csv"))

# seattle wide data 
sewa_counts_all <- counts_all %>% filter(city == "sewa") %>%
  left_join(counts[,c("site","season", "days.active")], 
                        by = c("site","season"))

sewa_all_wide <- left_join(sewa_counts_all, sewa_sitecovs, by = c("site")) %>%
  distinct() 

write_csv(sewa_all_wide, here("data", "covariates", "SEWA_WIDE_COUNTS_ALL_ENV_URB_SITES_1000m.csv"))


# tacoma wide data
tawa_counts_all <- counts_all %>% filter(city == "tawa") %>%
  left_join(counts[,c("site","season", "days.active")], 
            by = c("site","season"))

tawa_all_wide <- left_join(tawa_counts_all, tawa_sitecovs, by = c("site")) %>%
  distinct() 

write_csv(tawa_all_wide, here("data", "covariates", "TAWA_WIDE_COUNTS_ALL_ENV_URB_SITES_1000m.csv"))
