# load all risk of bias results

# study-level RoB1 based on "out.04.rob.scraper"
RoB1.01  <- read.csv(paste0('out.04.rob.scraper', '/scraped_rob_wide_processed.csv'))

# study-level RoB1 based on "out.03.zips"
RoB1.02  <- read.csv(paste0('out.03.zips', '/scraped_rob_wide_processed.csv'))

# study-level RoB2 based on "out.03.zips"
RoB2.01b <- read.csv(paste0('out.03.zips', '/RoB2b.csv'))

# estimate-level RoB2 based on "out.03.zips"
RoB2.01  <- read.csv(paste0('out.03.zips', '/RoB2.csv'))
