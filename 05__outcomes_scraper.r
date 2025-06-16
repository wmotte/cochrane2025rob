# Load required libraries
#
# Extract Primary and Secondary outcomes as reported in SR.
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
library( "rvest" )
library( "dplyr" )
library( "purrr" )
library( "stringr" )
library( "plyr" )

################################################################################

###
# Helper function to extract outcomes for a specific type (e.g., Primary)
##
extract_outcome_type <- function(section, outcome_type) 
{
    # Find the subsection (h5) that matches the outcome type
    subsections <- section %>% html_elements("section")
    
    target_subsection <- subsections[sapply(subsections, function(x) {
        title <- tryCatch({
            x %>% html_element("*[class*='title']") %>% html_text(trim = TRUE)
        }, error = function(e) "")
        grepl(outcome_type, title, ignore.case = TRUE)
    })]
    
    if (length(target_subsection) == 0) {
        return(NA)
    }
    
    # Extract list items from the subsection
    # This structure is common in Cochrane reviews
    list_items <- target_subsection[[1]] %>% 
        html_elements("ol li p") %>% 
        html_text(trim = TRUE)
    
    # If no ordered list items found, try unordered lists
    if (length(list_items) == 0) {
        list_items <- target_subsection[[1]] %>% 
            html_elements("ul li p") %>% 
            html_text(trim = TRUE)
    }
    
    # If still no items, try any paragraph within the section
    if (length(list_items) == 0) {
        list_items <- target_subsection[[1]] %>% 
            html_elements("p") %>% 
            html_text(trim = TRUE)
        # Remove empty paragraphs which are sometimes used for spacing
        list_items <- list_items[list_items != ""]
    }
    
    if (length(list_items) == 0) {
        return(NA)
    }
    
    # Combine all found outcomes into a single string, separated by " | "
    return(paste(list_items, collapse = " | "))
}

###
# Function to extract outcomes from the dedicated HTML section
##
extract_outcomes_from_html <- function( url ) 
{
    # Read the HTML file with explicit UTF-8 encoding
    page <- xml2::read_html( url, encoding = "UTF-8")
    
    # Find the "Types of outcome measures" section
    outcomes_sections <- page %>% 
        html_elements("section") %>%
        .[sapply(., function(x) {
            title <- tryCatch({
                x %>% html_element("*[class*='title']") %>% html_text(trim = TRUE)
            }, error = function(e) "")
            # Match the section title
            grepl("Types of outcome measures", title, ignore.case = TRUE)
        })]
    
    # If the section doesn't exist, return NA for all outcomes
    if (length(outcomes_sections) == 0) {
        return(list(
            primary_outcomes_html = NA,
            secondary_outcomes_html = NA
        ))
    }
    
    outcomes_section <- outcomes_sections[[1]]
    
    primary <- extract_outcome_type(outcomes_section, "Primary outcomes")
    secondary <- extract_outcome_type(outcomes_section, "Secondary outcomes") 
    
    return(list(
        primary_outcomes_html = primary,
        secondary_outcomes_html = secondary
    ))
}



################################################################################
# END CUSTOM FUNCTIONS
################################################################################

# MODIFICATION: Updated output directory name
outdir <- 'out.05.outcomes.scraper'
dir.create( outdir, showWarnings = FALSE )

# Initialize an empty container for all scraped data
all <- NULL

# get infiles
infiles <- list.files( 'out.03.cochrane.downloads/', '*_main.html' )

# Main Processing Loop
ind <- 1
for( infile in infiles )
{
    print( paste0( "[", ind, "] -> ", infile ) ); ind <- ind + 1
    url <- paste0( 'out.03.cochrane.downloads/', infile )
    
    df <- extract_outcomes_from_html( url )

    df <- data.frame( df )
    
    # Add the source filename as a column for traceability
    df$infile <- infile

    # Combine the data from the current file with all previous files
    all <- plyr::rbind.fill( all, df )
}


# Finalization and Output
if (!is.null(all) && nrow(all) > 0) 
{
    # Remove any columns that are entirely NA after combining all files
    all <- all[, colSums( is.na( all ) ) < nrow( all ) ]
    
    # Bring 'infile' to the first position
    if ("infile" %in% names(all)) {
        all <- all[, c("infile", setdiff(names(all), "infile"))]
    }

    # output filename
    readr::write_tsv( all, paste0( outdir, '/scraped_outcomes.tsv' ), quote = "needed" )
    
    # Print a summary of the scraping process
    print(paste("Processed", nrow(all), "studies from", length(infiles), "files"))
    print("Outcome columns extracted:")
    outcome_cols <- names(all)[grepl("outcomes", names(all), ignore.case = TRUE)]
    print(outcome_cols)
    
} else {
    print("No data was extracted from any of the files.")
}

