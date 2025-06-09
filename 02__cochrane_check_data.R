#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
# Check which systematic reviews have rm5 or zip data.
#
################################################################################
library( "rvest" )
library( "dplyr" )
library( "xml2" )
library( "httr" )

###
# Function to scrape download data URL from Cochrane review page
##
scrape_cochrane_download_url <- function(url)
{
    tryCatch({
        # Read the webpage
        page <- read_html(url)
        
        # Method 1: Look for the download-stats-data-link class
        download_link <- page %>%
            html_node(".download-stats-data-link a") %>%
            html_attr("href")
        
        # Method 2: Alternative approach - look for "Download data" text
        if (is.na(download_link)) {
            download_link <- page %>%
                html_nodes("a") %>%
                .[html_text(.) == "Download data"] %>%
                html_attr("href") %>%
                first()
        }
        
        # Method 3: Look within supplementary materials section
        if (is.na(download_link)) {
            # Find the supplementary materials section
            supp_section <- page %>%
                html_nodes(".nav-section-header") %>%
                .[html_text(.) %>% stringr::str_detect("Supplementary materials")]
            
            if (length(supp_section) > 0) {
                # Get the next sibling list containing the links
                download_link <- supp_section %>%
                    html_node("+ ul") %>%
                    html_nodes("a") %>%
                    .[html_text(.) %>% stringr::str_detect("Download data")] %>%
                    html_attr("href") %>%
                    first()
            }
        }
        
        # If relative URL, make it absolute
        if (!is.na(download_link) && download_link != "" && !stringr::str_detect(download_link, "^http")) {
            base_url <- stringr::str_extract(url, "^https?://[^/]+")
            download_link <- paste0(base_url, download_link)
        }
        
        if( is.na( download_link ) )
            return( NA )
        
        if( download_link == "" )
            return( NA )
        
        # clean
        download_link <- stringr::str_remove( download_link, "\\?.*$")
        
        return(download_link)
        
    }, error = function(e) {
        message("Error scraping URL: ", e$message)
        return(NA)
    })
}

###
# Function to scrape multiple Cochrane review URLs
##
scrape_multiple_cochrane_urls <- function(urls)
{
    results <- data.frame(
        review_url = urls,
        download_url = character(length(urls)),
        stringsAsFactors = FALSE
    )
    
    for (i in seq_along(urls)) {
        cat("Processing URL", i, "of", length(urls), "\n")
        results$download_url[i] <- scrape_cochrane_download_url(urls[i])
        
        # Be polite - add a small delay between requests
        Sys.sleep(1)
    }
    
    return(results)
}

###
# Save html website
##
save_webpage <- function(url, filename)
{
    tryCatch({
        # Read the webpage
        webpage <- read_html(url)
        
        # Save to file
        write_html(webpage, filename)
        
        cat("Successfully saved", url, "to", filename, "\n")
        
        # Be polite - add a small delay between requests
        Sys.sleep(1)
        
    }, error = function(e) {
        cat("Error saving webpage:", e$message, "\n")
    })
}


################################################################################

# outdir
outdir <- 'out.02.cochrane.check.data'
dir.create( outdir, showWarnings = FALSE )

# get input data
df <- readr::read_tsv( 'out.01.cochrane.most.recent/reviews.tsv', show_col_types = FALSE )

# make urls
df$urls <- paste0( "https://www.cochranelibrary.com/cdsr/doi/", df$DOI )

head( df )
tail( df )

# Create row indices for chunking
row_indices <- 1:nrow( df )
chunks <- split( row_indices, ceiling( row_indices / 250 ) )

# container
all <- NULL

# loop over chunks
for (chunk_num in 1:length( chunks ) ) 
{
    chunk_rows <- chunks[[ chunk_num ]]
    
    dfsub <- df[ chunk_rows, ]
    
    # get download url to rm5/zip Supplementary data
    results <- scrape_multiple_cochrane_urls( dfsub$urls )
    dfsub$url_rm5 <- results$download_url
    
    # add to container
    all <- rbind( all, dfsub )
    
    # Save results
    save_file <- paste0( outdir, "/results_chunk_", chunk_num, ".rds" )
    saveRDS( dfsub, save_file )
    cat( "Saved chunk", chunk_num, "with", length( chunk_rows ), "rows\n" )
}


# output file
outfile <- paste0( outdir, '/reviews_with_rm5_url.tsv' )

# Write
readr::write_tsv( all, outfile, quote = "needed" )

