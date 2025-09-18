#!/usr/bin/env Rscript
#
# Child vs adults
################################################################################
library(tidyverse)

# output dir
outdir <- 'out.08.select.child.health'
dir.create( outdir, showWarnings = FALSE )

# load all 'Child Health' Intervention studies
df <- readr::read_csv( 'misc/raw--child-health.csv.gz' )
df[ is.na( df$Keywords ), 'Keywords' ] <- "No keywords"

df$Source <- NULL
df$ISSN <- NULL
df$Publisher <- NULL
df$Abstract <- NULL
df$URL <- NULL
df$`Cochrane Review Group Code` <- NULL
df$`Cochrane Review ID` <- NULL

# check title and keywords on 'adult'
id1 <- stringr::str_detect( tolower( df$Title ), 'adult' ) 
id2 <- stringr::str_detect( tolower( df$Keywords ), 'adult' ) 
id3 <- id1 | id2

summary( id1 )
summary( id2 )
summary( id3 )

# new column
df$group <- 'children'
df[ id3, 'group' ] <- 'children_and_adults'

# get papers excluding 'adult' from title or keywords
out <- df |> select( group, DOI, Title, Year )

# write
readr::write_tsv( out, file = paste0( outdir, '/child_health_intervention_reviews.tsv' ), quote = 'all' )

# children children_and_adults 
#     1992                 828 
summary( as.factor( out$group ) )
