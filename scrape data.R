
get_link <- function(landing_page, variable_class) {
  page <- read_html(landing_page)
  
  link <- page %>% 
    html_nodes(variable_class) %>% 
    html_attr("href")
  
  return(link)
}

get_data <- function(link_prefix, file_name, landing_page, variable_class, which_link) {
  
  link <- get_link(landing_page, variable_class)[which_link]
  
  data_link <- paste0(link_prefix, link)
  
  GET(data_link, write_disk(file_name <- paste0(getwd(),"/data/",file_name,".xlsx"), overwrite = TRUE))
  
  
}


get_link_v2 <- function(landing_page, variable_class, attribute_html) {
  
  page <- read_html(landing_page)
  
  link <- page %>% 
    html_nodes(variable_class) %>% 
    html_nodes("a") %>% 
    html_attr(attribute_html)
  
  return(link)
}

get_data_v2 <- function(link_prefix, file_name, landing_page, variable_class, attribute_html, which_link) {
  
  link <- get_link_v2(landing_page, variable_class, attribute_html)[which_link]
  
  data_link <- paste0(link_prefix, link)
  
  GET(data_link, write_disk(file_name <- paste0(getwd(),"/data/",file_name,".xlsx"), overwrite = TRUE))
  
}

get_link_v3 <- function(landing_page,variable_class,attribute_html,which_link) {
  page <- read_html(landing_page)
  
  link <- page %>% 
    html_nodes(variable_class) %>% 
    html_nodes("a") %>%
    html_attr(attribute_html)
  
  return(link[which_link])
  
}

get_data_v3 <- function(link_prefix, file_name, landing_page, variable_class, attribute_html, which_link) {
  
  link <- get_link_v2(landing_page, variable_class, attribute_html)[which_link]
  
  data_link <- paste0(link_prefix, link)
  
  final_data<-GET(data_link, write_disk(file_name <- paste0(getwd(),"/data/",file_name,".xlsx"), overwrite = TRUE))
  return (final_data)
  
}



get_excel_from_zipped_file<-function(link_prefix,variable_class,homepage,zipped_file_name,sheet_name){
  link <- get_link(homepage, variable_class)[1]
  zip.url <- paste0(link_prefix, link)
  dir <- as.character(paste0(getwd(),"/data"))
  zip.file <- paste0(zipped_file_name,".zip")
  zip.combine <- as.character(paste(dir, zip.file, sep = "/"))
  download.file(zip.url, destfile = zip.combine)
  dataset<-unzip(zip.combine, exdir = dir)
  final_data <- read_excel(dataset,sheet = sheet_name)
  return (final_data)
}


get_link_v5 <- function(landing_page,variable_class,element) {
  page <- read_html(landing_page)
  
  link <- page %>% 
    html_nodes(variable_class) %>% 
    html_attr(element)
  
  return(link)
  
}

get_link_v4 <- function(landing_page,variable_class,which_link,element) {
  page <- read_html(landing_page)
  
  link <- page %>% 
    html_nodes(variable_class) %>% 
    html_attr(element)
  
  return(link[which_link])
  
}




get_data_v4 <- function(file_name, landing_page, variable_class, which_link,element) {
  
  data_link <- get_link_v5(landing_page, variable_class,element)[which_link]
  
  
  dataset<-GET(data_link, write_disk(file_name <- paste0(getwd(),"/data/",file_name,".xlsx"), overwrite = TRUE))
  
  return (dataset)}


get_data_v6 <- function(landing_page,data_prefix, variable_class, attribute_html,filename) {
  
  page <- read_html(landing_page)
  
  link <- page %>% 
    html_nodes(variable_class) %>% 
    html_nodes("a") %>% 
    html_attr(attribute_html)
  
  
  linkbit <- link[grep(".xlsx", link)]
  
  data_link=paste0(data_prefix,linkbit)
  dataset<-GET(data_link, write_disk(filename <- paste0(getwd(),"/data/",filename,".xlsx"), overwrite = TRUE))
  
  return( dataset)}