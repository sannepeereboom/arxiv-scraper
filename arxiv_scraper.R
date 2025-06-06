require(rvest)
require(stringr)
require(readr)

arxiv_scraper <- function(url, from_date = NA, to_date = NA, output_file = "arxiv_results.csv", download_pdf = F) {

  # max results and sort by date ascending for possible batching
  url <- str_replace(url, "size=[0-9]+", "size=200")
  url <- str_replace(url, "order=[^&]*", "order=submitted_date")
  if(!is.na(from_date) && !is.na(to_date)) {
    url <- str_replace(url, "date-filter_by=all_dates", "date-filter_by=date_range")
    url <- str_replace(url, "date-from_date=[^&]*", paste0("date-from_date=", from_date))
    url <- str_replace(url, "date-to_date=[^&]*", paste0("date-to_date=", to_date))
  }
  
  # total number of results
  html <- read_html(url)
  total_results <- html %>%
    html_element('.is-clearfix') %>%
    html_text2() %>%
    str_extract(regex(' ([0-9])*,*([0-9])* ')) %>%
    str_remove_all("[^0-9]") %>%
    as.numeric()
  rm(html)
  
  cat('Total results:', total_results, '\n\n')
  if (total_results > 10000) {
    cat('Warning: arXiv only shows the first 10,000 results (up to 50 pages). Consider batching searches by date range.\n')
  }
  
  # extract data from all pages
  total_pages <- ceiling(min(total_results, 10000) / 200)
  
  for (page in 0:(total_pages - 1)) {
    cat('Scraping page', page + 1, 'of', total_pages, '\n')
    
    # adjust url based on page
    start_index <- page * 200
    page_url <- paste0(url, "&start=", start_index)
    html <- read_html(page_url)
    
    # get results
    results <- html %>%
      html_elements(xpath = '/html/body/main/div[2]/ol') %>%
      html_elements('.arxiv-result')
    
    # extract metadata per result
    extract_metadata <- function(result) {
      arxiv_identifier <- result %>%
        html_element(xpath = './/p[contains(@class, "list-title")]//a[starts-with(., "arXiv:")]') %>%
        html_text2()
      
      title <- result %>%
        html_element('p.title.is-5') %>%
        html_text2()
      
      authors <- result %>%
        html_elements('p.authors a') %>%
        html_text2() %>%
        paste(collapse = '; ')
      
      abstract <- result %>%
        html_element('p.abstract span.abstract-full') %>%
        html_text2() %>%
        str_remove_all(' ?△ Less$')
      
      date <- result %>%
        html_element(xpath = './/p[@class="is-size-7" and contains(., "Submitted")]') %>%
        html_text2() %>%
        str_extract(regex('[0-9]{4}'))
      
      categories <- result %>%
        html_elements('div.tags.is-inline-block .tag') %>%
        html_text2() %>%
        paste(collapse = '; ')
      
      doi <- result %>%
          html_element('div.tags.has-addons span.tag a') %>%
          html_text2()
      
      journal_reference <- result %>%
          html_element(xpath = './/p[contains(@class, "comments") and contains(., "Journal ref:")]') %>%
        html_text2() %>%
          str_extract('Journal ref: (.*)') %>%
          str_remove('Journal ref: ')
      
      comments <- result %>%
          html_element(xpath = './/p[contains(@class, "comments") and contains(., "Comments:")]') %>%
        html_text2() %>%
          str_extract('Comments: (.*)') %>%
          str_remove('Comments: ')
      
      pdf_link <- result %>%
        html_element('.list-title a[href*="/pdf/"]') %>%
        html_attr('href')
      
      df<- data.frame(
        arXiv_identifier = arxiv_identifier,
        title = title,
        authors = authors,
        abstract = abstract,
        date = date,
        category = categories,
        doi = doi,
        journal_reference = journal_reference,
        comments = comments,
        pdf_link = pdf_link,
        stringsAsFactors = F
      )
      return(df)
    }
    
    # create df of all results on this page
    page_data <- lapply(results, extract_metadata)
    page_data <- do.call(rbind, page_data)
    
    # add page results to csv
    write_excel_csv(page_data, output_file, append = TRUE, col_names = !file.exists(output_file))
    
    
    # download PDFs
    if (download_pdf) {
      if(!dir.exists("./arxiv_scraped_pdfs")) {
        dir.create("./arxiv_scraped_pdfs")
      }
      for (i in seq_along(page_data$pdf_link)) {
        pdf_url <- page_data$pdf_link[i]
        pdf_name <- page_data$arXiv_identifier[i] %>%
          str_remove_all(., "arXiv:") %>%
          paste0(".pdf")
          
        download.file(pdf_url, paste0("./arxiv_scraped_pdfs/", pdf_name))
      }
      cat("Downloaded PDFs for", nrow(page_data), "records.\n")
    }
    
    # to avoid overloading servers
    Sys.sleep(2)

  }
  
  cat("Scraping completed.\n")
}

### EXAMPLE
# url <- "https://arxiv.org/search/advanced?advanced=&terms-0-operator=AND&terms-0-term=%22language+model*%22+OR+%22large+language+model*%22+OR+%22generative+language+model*%22+OR+LLM*+OR+ChatGPT+OR+GPT*+OR+llama*+OR+claude*+OR+palm*+OR+gemini*+OR+deepseek*+OR+%22generative+pre-trained%22&terms-0-field=abstract&terms-1-operator=AND&terms-1-term=psycholog*+OR+cognit*+OR+psychometric*+OR+assess*+OR+measur*+OR+test*+OR+scale*+OR+instrument*+OR+behavior*+OR+abilit*+OR+capabilit*+OR+skill*+OR+%22latent+trait*%22+OR+%22latent+variable*%22+OR+%22latent+construct*%22+OR+%22latent+factor*%22+OR+%22latent+dimension*%22+OR+%22latent+structure*%22+OR+%22latent+characteristic*%22+OR+%22underlying+trait*%22+OR+%22underlying+variable*%22+OR+%22underlying+construct*%22+OR+%22underlying+factor*%22+OR+%22underlying+dimension*%22+OR+%22underlying+structure*%22+OR+%22underlying+characteristic*%22+OR+personalit*+OR+intelligen*+OR+reason*+OR+%22theory+of+mind%22+OR+conscious*+OR+metacogniti*+OR+attitude*+OR+opinion*+OR+belief*+OR+moral*+OR+ethic*+OR+value*+OR+norm*+OR+emotion*+OR+affect*+OR+mood*+OR+%22decision+making%22+OR+judgment*+OR+%22problem+solving%22+OR+bias*+OR+creativ*+OR+%22pattern+recognition%22+OR+empath*+OR+%22self+concept%22+OR+identit*+OR+motivation*&terms-1-field=abstract&terms-2-operator=NOT&terms-2-term=multimodal+OR+engineering+OR+classification+OR+%22sentiment+analysis%22+OR+%22vision-language%22&terms-2-field=abstract&classification-physics_archives=all&classification-include_cross_list=include&date-filter_by=all_dates&date-year=&date-from_date=&date-to_date=&date-date_type=submitted_date&abstracts=show&size=50&order="

## < 10.000 results
#arxiv_scraper(url, 
#             output_file = "arxiv_results.csv", 
#             from_date = "2017-01-01", 
#             to_date = "2017-06-01", 
#             download_pdf = F)

##########################################

## Batching by date for > 10.000 results; 2017 to 2019
#timeframe <- seq.Date(as.Date("2017-01-01"), 
#                      as.Date("2020-01-01"), 
#                      by = "6 months")

## Adjust upper bound to last day of 2019
#timeframe[length(timeframe)] <- timeframe[length(timeframe)]-1

## Run export
#for(i in 1:(length(timeframe)-1)){
#  print(paste0("Scraping from ", timeframe[i], " to ", timeframe[i+1]))
#  arxiv_scraper(url, 
#                from_date = timeframe[i], 
#                to_date = timeframe[i+1], 
#                output_file = "arxiv_results.csv")
#}

##########################################

## download pdfs in a separate folder named arxiv_scraped_pdfs
## maybe not a good idea if there are thousands of results
#arxiv_scraper(url, output_file = "arxiv_results.csv", download_pdf = T)
