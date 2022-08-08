# install necessary packages

install.packages('jsonlite')
install.packages('rvest')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('httr')

# load necessary libraries into memory

library(jsonlite)
library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

trimStringArray = function(array) {
  if (is.array(array)) {
    for (i in 1:length(array())) {
      array[i] = trimws(array[i])
    }
  } else {
    array = trimws(array)
  }
  array
}


scrapeColumnsWM = function(game_name) {
  
  # base url
  
  url = 'https://www.transfermarkt.de/weltmeisterschaft-2022/teilnehmer/pokalwettbewerb/WM22/saison_id/2021'
  
  # read url
  
  url %>%
    read_html() -> html
  
  # variables
  
  columnValues = list() # a list containing all values
  columnNames = NULL
  box = NULL # searching for the table_header
  slugs = c()
  club_ids = c()
  
  # Obtain the correct responsive-table by name of table_header
  
  html %>%
    html_elements('.box') -> boxes
  
  # Print the table headers
  
  boxes
  
  # Iterate over each table header to find the match with table_header
  # argument
  
  for (item in boxes) {
    
    #print(item)
    
    item %>%
      html_elements('.table-header') %>%
      html_text() %>%
      trimStringArray() -> name
    
    if (length(name) != 0 && tolower(game_name) == tolower(name)) {
      box = item
      break
    }
  }
  
  box
  
  # Obtain the responsive_table
  
  box %>%
    html_elements('.responsive-table') -> responsive_table
  
  # print the responsive-table
  
  responsive_table
  
  # scrape the url with the slugs and club ids of the countries
  
  responsive_table %>%
    html_elements('tr td:first-of-type a') %>%
    html_attr('href') %>%
    strsplit('/') -> raw_url_data
  
  raw_url_data
  
  # obtain the slug of all countries
  
  raw_url_data[1][[1]][2]
  raw_url_data[1][[1]][length(raw_url_data[1][[1]])]
  
  for (i in 1:length(raw_url_data)) {
    slugs = c(slugs, raw_url_data[i][[1]][2])
    club_ids = c(club_ids, raw_url_data[i][[1]][length(raw_url_data[1][[1]])])
  }
  
  # print slugs
  
  slugs
  
  # print club_ids
  
  club_ids
  
  # obtain the columnNames
  
  responsive_table %>%
    html_elements('th') %>%
    html_text -> columnNames
  
  # print columnNames
  
  columnNames
  
  # add slug and club_id to columnNames
  
  columnNames = c(columnNames, c('slug', 'club_id'))
  
  # print columnNames again
  
  columnNames
  
  # obtain length of a all true columns
  
  responsive_table %>%
    html_elements('tbody tr:nth-of-type(1) td') %>%
    length() -> trueColumnsLength
  
  # print the length of all true columns
  
  trueColumnsLength
  
  for (i in 1:trueColumnsLength) {
    # create css selector string
    # td:not([class*="no-border-rechts"]
    string = paste0('tbody tr td:not([class*="no-border-rechts"]):nth-of-type(', i,')')
    
    # print the css selector string
    
    string
    
    # obtain all row elements of specified column
    
    responsive_table %>%
      html_elements(string) -> html_node
    
    # print all row elements of specified column
    
    html_node
    
    # only add the values if they are valid!!!
    
    if(length(html_node) != 0) {
      # obtain the children (if any) from the row elements
      # of specified column
      
      html_node %>%
        html_children() -> children
      
      # print the children
      
      children
      
      # check the number of children contained within
      
      num_of_children = length(children)
      
      # print the number of children (if any)
      
      num_of_children
      
      # check if a child contains any subchildren
      
      children %>%
        html_children() -> subchildren
      
      # print subchildren
      
      subchildren
      
      # check the number of children contained within
      
      num_of_subchildren = length(subchildren)
      
      # print number of subchildren
      
      num_of_subchildren
      
      # dynammically apply a search for the values
      # according to the number of children
      
      if (num_of_subchildren > 0) {
        responsive_table %>%
          html_elements(string) %>%
          html_elements('a > span') %>%
          html_text() -> values
      } else if (num_of_children > 0) {
        responsive_table %>%
          html_elements(string) %>%
          html_elements('a') %>%
          html_attr('title') -> values
      } else {
        responsive_table %>%
          html_elements(string) %>%
          html_text -> values
      }
      
      # wrap the obtained values within a list
      
      list(values)
      
      # iterate over all children by
      
      values = trimStringArray(values)
      
      # Add this column to the columnValues list
      
      columnValues = append(columnValues, list(values))
    }
    else {
      print(paste0("Column ", i, " is not a valid value"))
    }
    
  }
  
  # add club_id and slug to values
  
  columnValues = append(columnValues, list(slugs))
  columnValues = append(columnValues, list(club_ids))
  
  # print columnValues
  
  columnValues
  
  # return list
  list(columnNames, columnValues)
}

create_dataframeWM = function(name) {
  df_wm = scrapeColumnsWM(name)
  columns = df_wm[[2]]
  columnNames = df_wm[[1]]
  
  #print(columnNames)
  
  df_wm = data.frame(columns[[1]])
  
  for (i in 2:length(columns)) {
    df_wm[i] = columns[[i]]
  }
  
  names(df_wm) = columnNames[2:length(columnNames)]
  df_wm
}

df_wm = create_dataframeWM('teilnehmende teams an der wm22')
df_wm


scrapeColumns = function(year, country, club_id, game_name) {
  
  # base url
  
  url = paste0("https://www.transfermarkt.com/",
               country,
               "/spielplan/verein/",
               club_id,
               "/plus/1?saison_id=", year)
  
  # read url
  
  url %>%
    read_html() -> html
  
  # variables
  
  columnValues = list() # a list containing all values
  columnNames = NULL
  box = NULL # searching for the table_header
  
  # Obtain the correct responsive-table by name of table_header
  
  html %>%
    html_elements('.box') -> boxes
  
  # Print the table headers
  
  boxes
  
  # Iterate over each table header to find the match with table_header
  # argument
  
  for (item in boxes) {
    
    #print(item)
    
    if(length(html_elements(item, '.table-header') %>% html_children())!= 0) {
      item %>%
        html_elements('.table-header a img') %>%
        html_attr('title') -> name
      
      print(name)
    } else {
      item %>%
        html_elements('.table-header') %>%
        html_text() %>%
        trimStringArray() -> name
      
        #print(name)
    }

    if (length(name) != 0 && tolower(game_name) == tolower(name)) {
      box = item
      break
    }
  }
  
  box
  
  if (box == NULL) {
    # Obtain the responsive_table
    
    box %>%
      html_elements('.responsive-table') -> responsive_table
    
    # print the responsive-table
    
    responsive_table
    
    # obtain the columnNames
    
    responsive_table %>%
      html_elements('th') %>%
      html_text -> columnNames
    
    # print columnNames
    
    columnNames
    
    # obtain length of a all true columns
    
    responsive_table %>%
      html_elements('tbody tr:nth-of-type(1) td') %>%
      length() -> trueColumnsLength
    
    # print the length of all true columns
    
    trueColumnsLength
    
    for (i in 1:trueColumnsLength) {
      # create css selector string
      # td:not([class*="no-border-rechts"]
      string = paste0('tbody tr td:not([class*="no-border-rechts"]):nth-of-type(', i,')')
      
      # print the css selector string
      
      string
      
      # obtain all row elements of specified column
      
      responsive_table %>%
        html_elements(string) -> html_node
      
      # print all row elements of specified column
      
      html_node
      
      # only add the values if they are valid!!!
      
      if(length(html_node) != 0) {
        # obtain the children (if any) from the row elements
        # of specified column
        
        html_node %>%
          html_children() -> children
        
        # print the children
        
        children
        
        # check the number of children contained within
        
        num_of_children = length(children)
        
        # print the number of children (if any)
        
        num_of_children
        
        # check if a child contains any subchildren
        
        children %>%
          html_children() -> subchildren
        
        # print subchildren
        
        subchildren
        
        # check the number of children contained within
        
        num_of_subchildren = length(subchildren)
        
        # print number of subchildren
        
        num_of_subchildren
        
        # dynammically apply a search for the values
        # according to the number of children
        
        if (num_of_subchildren > 0) {
          responsive_table %>%
            html_elements(string) %>%
            html_elements('a > span') %>%
            html_text() -> values
        } else if (num_of_children > 0) {
          responsive_table %>%
            html_elements(string) %>%
            html_elements('a') %>%
            html_attr('title') -> values
        } else {
          responsive_table %>%
            html_elements(string) %>%
            html_text -> values
        }
        
        # wrap the obtained values within a list
        
        list(values)
        
        # iterate over all children by
        
        values = trimStringArray(values)
        
        # Add this column to the columnValues list
        
        columnValues = append(columnValues, list(values))
      }
      else {
        print(paste0("Column ", i, " is not a valid value"))
      }
      
    }
    
    # return list
    return list(columnNames, columnValues)
  } else {
    return NULL
  }
  
  
}

createDataframe = function(year, country, club_id, game_name) {
  
  list = scrapeColumns(year, country, club_id, game_name)
  print("test")
  print(list)
  columns = list[[2]]
  columnNames = list[[1]]
  
  #print(columns)
  #print(columnNames)
  
  # create a dataframe with first colum and append the rest (workaround)
  df_test = data.frame(columns[[1]])
  
  for (i in 2:length(columns)) {
    df_test[i] = columns[[i]]
  }
  
  names(df_test) = columnNames
  df_test
}

# Testing Section

df_new_test = createDataframe(2013, 'spanien', 3375, 'world cup 2010')
df_new_test
