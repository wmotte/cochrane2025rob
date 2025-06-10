# Load required libraries
library( "rvest" )
library( "dplyr" )
library( "purrr" )
library( "stringr" )
library( "plyr" )

################################################################################

###
# Function to extract data from a single study table
##
extract_study_data <- function(study_table) 
{
    # Extract study name from table heading
    study_name <- study_table %>% 
        html_element(".table-title") %>% 
        html_text(trim = TRUE)
    
    # Get the actual table element
    table_elem <- study_table %>% 
        html_element("table")
    
    # Extract all rows
    rows <- table_elem %>% 
        html_elements("tr")
    
    # Initialize lists to store data
    study_characteristics <- list()
    risk_of_bias <- list()
    
    # Track which section we're in
    current_section <- "characteristics"
    
    # Process each row
    for (i in seq_along(rows)) {
        row <- rows[[i]]
        cells <- row %>% html_elements("td")
        
        # Skip header rows or rows with less than 2 cells
        if (length(cells) < 2) next
        
        # Check if this is the risk of bias section header
        cell_text <- cells[[1]] %>% html_text(trim = TRUE)
        
        if (grepl("Risk of bias", cell_text, ignore.case = TRUE)) {
            current_section <- "risk_of_bias"
            next
        }
        
        # Skip bias header row
        if (cell_text == "Bias") next
        
        # Extract data based on current section
        if (current_section == "characteristics") {
            if (length(cells) >= 2) {
                field_name <- cells[[1]] %>% html_text(trim = TRUE)
                field_value <- cells[[2]] %>% html_text(trim = TRUE)
                
                # Clean up field names
                field_name <- str_replace_all(field_name, "[^A-Za-z0-9_]", "_")
                field_name <- str_replace_all(field_name, "_+", "_")
                field_name <- str_remove(field_name, "^_|_$")
                
                if (field_name != "" && field_value != "") {
                    study_characteristics[[field_name]] <- field_value
                }
            }
        } else if (current_section == "risk_of_bias") {
            if (length(cells) >= 3) {
                bias_type <- cells[[1]] %>% html_text(trim = TRUE)
                judgment <- cells[[2]] %>% html_text(trim = TRUE)
                support <- cells[[3]] %>% html_text(trim = TRUE)
                
                # Clean up bias type name
                bias_type_clean <- str_replace_all(bias_type, "[^A-Za-z0-9_]", "_")
                bias_type_clean <- str_replace_all(bias_type_clean, "_+", "_")
                bias_type_clean <- str_remove(bias_type_clean, "^_|_$")
                
                if (bias_type_clean != "" && judgment != "") {
                    risk_of_bias[[bias_type_clean]] <- list(
                        bias_type = bias_type,
                        judgment = judgment,
                        support = support
                    )
                }
            }
        }
    }
    
    # Return structured data for this study
    return(list(
        study_name = study_name,
        characteristics = study_characteristics,
        risk_of_bias = risk_of_bias
    ))
}

###
# Function to scrape studies and risk of bias from a webpage
##
scrape_studies_risk_bias <- function(url)
{
    # explicit utf-8 required for correct parsing of characters like -, ä, é, ô, etc.
    page <- xml2::read_html( url, encoding = "UTF-8")
    
    # Find the characteristicStudies section
    studies_section <- page %>% 
        html_element("section.characteristicStudies")
    
 
    if (is.na(studies_section)) {
        warning("No characteristicStudies section found on the webpage")
        return( NULL )
    }
  
    # Find all study tables within the section
    study_tables <- studies_section %>% 
    html_elements("div.table")
    
    if (length(study_tables) == 0) {
        warning("No study tables found")
        return(NULL)
    }
  
    # Extract data from all studies
    studies_data <- map(study_tables, extract_study_data)
    
    # Remove any NULL entries
    studies_data <- studies_data[!sapply(studies_data, is.null)]
    
    return(studies_data)
}

###
# Function to convert extracted data to a more analysis-friendly format
##
format_studies_data <- function(studies_data) 
{
  
  # Create a data frame for study characteristics
  studies_df <- map_dfr(studies_data, function(study) {
    chars <- study$characteristics
    chars$study_name <- study$study_name
    return(as_tibble(chars))
  })
  
  # Create a data frame for risk of bias assessments
  risk_bias_df <- map_dfr(studies_data, function(study) {
    rob_data <- study$risk_of_bias
    if (length(rob_data) > 0) {
      rob_df <- map_dfr(rob_data, function(bias) {
        return(tibble(
          bias_type = bias$bias_type,
          judgment = bias$judgment,
          support = bias$support
        ))
      })
      rob_df$study_name <- study$study_name
      return(rob_df)
    }
    return(NULL)
  })
  
  return(list(
    studies = studies_df,
    risk_of_bias = risk_bias_df,
    raw_data = studies_data
  ))
}

###
# Function to scrape from a local HTML file (for testing)
##
scrape_studies_from_file <- function(file_path) {
  studies_data <- scrape_studies_risk_bias(file_path)
  return(format_studies_data(studies_data))
}

###
# Function to move 'bias' columns to the left, sorted alphabetically
##
reorder_bias_columns <- function(df)
{
    # Find columns containing 'bias' (case-insensitive)
    bias_cols <- names(df)[grep("bias", names(df), ignore.case = TRUE)]
    
    # Sort bias columns alphabetically
    bias_cols <- sort(bias_cols)
    
    # Get remaining columns (not containing 'bias')
    other_cols <- setdiff(names(df), bias_cols)
    
    # Reorder data frame: bias columns first, then others
    df <- df[c(bias_cols, other_cols)]
    
    # Select study_name first, then all other columns
    df <- df[, c( "study_name", setdiff( names( df ), "study_name" ) ) ]
    
    return(df)
}

###
# Reorder columns with case-insensitive and partial matching
##
reorder_risk_columns_flexible <- function(df, sort_alphabetically = TRUE, 
                                          case_sensitive = FALSE, exact_match = TRUE) 
{
    # Define the risk terms to search for
    risk_terms <- c("Low risk", "High risk", "Unclear risk")
    
    # Function to check if a column contains any of the risk terms
    contains_risk <- function(column) {
        # Convert to character to handle factors
        col_char <- as.character(column)
        # Remove NA values for checking
        col_char <- col_char[!is.na(col_char)]
        
        if (exact_match) {
            if (case_sensitive) {
                any(col_char %in% risk_terms)
            } else {
                any(tolower(col_char) %in% tolower(risk_terms))
            }
        } else {
            # Partial matching using grep
            if (case_sensitive) {
                any(sapply(risk_terms, function(term) any(grepl(term, col_char, fixed = TRUE))))
            } else {
                any(sapply(risk_terms, function(term) any(grepl(term, col_char, ignore.case = TRUE))))
            }
        }
    }
    
    # Find columns that contain risk terms
    risk_col_indices <- sapply(df, contains_risk)
    risk_cols <- names(df)[risk_col_indices]
    
    # Sort risk columns alphabetically if requested
    if (sort_alphabetically) {
        risk_cols <- sort(risk_cols)
    }
    
    # Get remaining columns (not containing risk terms)
    other_cols <- setdiff(names(df), risk_cols)
    
    # Reorder data frame: risk columns first, then others
    df <- df[c(risk_cols, other_cols)]

    # Select study_name first, then all other columns
    df <- df[, c( "study_name", setdiff( names( df ), "study_name" ) ) ]
        
    # Select study_name first, then all other columns
    df <- df[, c( "infile", setdiff( names( df ), "infile" ) ) ]
    
    return(df)
}

###
# Function to extract primary outcomes
##
extract_primary <- function(text) {
    # More robust pattern to handle various primary outcome formats
    # Matches: Primary:, Primary outcome:, Primary outcomes:, Primary endpoint, etc.
    primary_pattern <- "(?i)primary\\s*(?:outcome[s]?[s]?|endpoint[s]?)?\\s*[:]?\\s*(.*?)(?=(?:secondary|$))"
    
    match <- str_match(text, primary_pattern)
    if (!is.na(match[1, 2])) {
        result <- str_trim(match[1, 2])
        # Clean up any trailing colons or extra whitespace
        result <- str_replace(result, "\\s*:*\\s*$", "")
        return(result)
    } else {
        return(NA)
    }
}

###
# Function to extract secondary outcomes  
##
extract_secondary <- function(text) {
    # More robust pattern to handle various secondary outcome formats
    # Matches: Secondary:, Secondary outcome:, Secondary outcomes:, Secondary endpoint, etc.
    secondary_pattern <- "(?i)secondary\\s*(?:outcome[s]?[s]?|endpoint[s]?)?\\s*[:]?\\s*(.*?)$"
    
    match <- str_match(text, secondary_pattern)
    if (!is.na(match[1, 2])) {
        result <- str_trim(match[1, 2])
        # Clean up any leading/trailing colons or extra whitespace
        result <- str_replace(result, "^\\s*:*\\s*", "")
        result <- str_replace(result, "\\s*:*\\s*$", "")
        return(result)
    } else {
        return(NA)
    }
}

################################################################################
# END CUSTOM FUNCTIONS
################################################################################

# outdir
outdir <- 'out.04.rob.scraper'
dir.create( outdir, showWarnings = FALSE )

# container
all <- NULL

# get infiles
infiles <- list.files( 'out.03.cochrane.downloads/', '*_references.html' )


# scrape
#urls <- c( "out.03.cochrane.downloads/14651858.CD000985.pub3_references.html",
#            "out.03.cochrane.downloads/14651858.CD000984.pub3_references.html" )


for( infile in infiles[1:24] )
{
    print( infile )
    url <- paste0( 'out.03.cochrane.downloads/', infile )
    
    # get raw utf-8 formatted list 
    raw <- format_studies_data( scrape_studies_risk_bias( url ) )
    
    # format
    df <- raw$studies
    
    # only keep row with study name present
    df <- df[ df$study_name != "", ]
    
    # Remove columns where all values are NA
    df <- df[, colSums(is.na(df)) < nrow(df)]
    
    # add infile
    df$infile <- infile
    
    # Using plyr package for a "full outer join" operation on columns.
    all <- plyr::rbind.fill( all, df )
}

# Remove columns where all values are NA
all <- all[, colSums( is.na( all ) ) < nrow( all ) ]
all <- reorder_risk_columns_flexible( all )

# Extract (raw) primary and secondary outcomes
all <- all %>%
    mutate(
        primary_outcomes = sapply( Outcomes, extract_primary ),
        secondary_outcomes = sapply( Outcomes, extract_secondary )
    )



