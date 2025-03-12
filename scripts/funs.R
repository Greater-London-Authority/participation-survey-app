#'[Script]#'*process-2_2_2_6.R*
#'[Project]#'*participation-survey-gla (https://github.com/mawtee/participation-survey-gla)*
#'[Author]#'*M. Tibbles*
#'[Last Update]#'*16/10/2024*
#'[Description]#'*This script defines a series of functions used in the analysis* 
#'               *of the participation survey.*
#'[____________________________________________________________________________]



# Scrape and write apprenticeships data to file
#===============================================================================

scrape_and_write_survey_data <- function(release_year) {
  
  #' @description 
  #' Scrapes latest Apprenticeships data from DFE site and writes to file
  #' 
  #' @details  
  #' 
  #' @param release_year String data release year (e.g. 2022_23). Defined in global `UPDATE__RELEASE`
  #'
  #' @noRd
  print(paste0("Scraping ", release_year," Participation Survey from web and writing to file"))


  # Load HTML page
  page <- rvest::read_html(paste0(
    "https://www.gov.uk/government/statistics/participation-survey-",gsub('_', '-', release_year),"-annual-publication"
  ))
  # Scrape link to data download
  link <- page %>%
    html_nodes("a") %>%               # find all links
    html_attr("href") %>%             # find all urls
    str_subset('data_tables') %>%
    .[[1]]
  # Download 
  if (file.exists(paste0('data/',sub("^.+/", "", link)))) {
    user_confirm <- readline(paste0("Data for ",release_year," is already saved in in directory. Are you sure you want to overwrite the existing data? (y/n)"))
    if (user_confirm=='y') {
      download.file(url=link, paste0('data/',sub("^.+/", "", link)), mode = "wb")
    }
    else {
      stop(
        'Aborting update: user does does not want to overwrite existing data'
      )
    }
  } 
  else {
    download.file(url=link, paste0('data/',sub("^.+/", "", link)), mode = "wb")
  }
  
}


#' generate_regional_plot <- function(survey_path, release_year, question) {
#'   
#'   #' @description 
#'   #' Generates interactive bar chart for regional comparison of survey results
#'   #' 
#'   #' @details  
#'   #' 
#'   #' @param survey_path String path to survey data. Defined in global `SURVEY_PATH`. 
#'   #' @param release_year String data release year (e.g. 2022_23). Defined in global `RELEASE_YEAR`.
#'   #' @param sheet_letter Letter of Excel sheet
#'   #' @param sheet_number Number of Excel sheet
#'   #'
#'   #' @noRd
#'   #print(paste0("Scraping ", release_year," Participation Survey from web and writing to file"))
#'   
#'   # survey_path <- "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods"
#'   # release_year <- '2023_24'
#'   # sheet_letter <- 'A'
#'   # sheet_number <- 2
#'   # 
#'   # 
#'   # survey_path <- SURVEY_PATH
#'   # release_year <- RELEASE_YEAR
#'   # question <- list('code'=QUESTION_LIST[[1]][[1]][1],'theme'=QUESTION_LIST[[1]][[2]][1], 'color'=QUESTION_LIST[[1]][[3]][1])
#' 
#'   # Load table
#'   print(question)
#'   #browser()
#'   if (file.exists(survey_path)) {
#'     df <- read_ods(survey_path, sheet=paste0('Table_', question[['code']]), skip=3)
#'   }
#'   else {
#'     stop(
#'       'Aborting procedure - survey data does not exist in user specified path'
#'     )
#'   }
#'   
#'   # Define colour palette
#'   pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1), main_colours=question[['color']])
#'   
#'   
#'   # Extract title and subtitle
#'   title <- paste0('<span style="color:',pal[1],'">', question[['theme']],'</span>')
#'   subtitle <- paste0('Percentage (%) of respondents who ', sub(".*: ", "", tolower(gsub(r"{\s*\[[^\)]+\]}","", paste0(question[['theme']],': ',gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question']))))))
#'   # title <- gsub(r"{\s*\[[^\)]+\]}","", paste0(question[['theme']],': ',gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])))
#'   # subtitle <- "Percentage of respondents (%)"
#'   # if (nchar(gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))>1) {
#'   #   subtitle <- paste0(subtitle," who ", gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))
#'   # }
#'   
#'   #browser()
#'   # Summarise into plotting frame
#'   df_plot <- df %>% 
#'     select(
#'       level=Question,
#'       region=`Response Breakdown`,
#'       prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
#'       prop_resp_lb=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Lower estimate'),
#'       prop_resp_ub=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Upper estimate'),
#'       num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
#'     ) %>%
#'     filter(grepl('ITL1', level)) %>%
#'     mutate(color = case_when(region=='London'~pal[1], T~pal[2])) %>%
#'     mutate(across(contains('resp'),~ round(as.numeric(as.character(.x,1))))) %>%
#'     mutate(region = fct_reorder(region, -prop_resp)) %>%
#'     arrange(region) %>%
#'     mutate(drilldown=case_when(region=='London'~'london',T~''))
#'   
#'   # Plot
#'   plot <- highchart() %>%
#'    hc_chart(spacingBottom= 50) %>%
#'     hc_add_series(
#'       id='central',
#'       type='bar', 
#'       name='Regions',
#'       showInLegend=F,
#'       data=df_plot, 
#'       hcaes(name=region, x = factor(region), y = round(prop_resp,1), color=color)
#'     ) %>%
#'     hc_add_series(
#'       id='confidence',
#'       type='errorbar',
#'       name='Lower-Upper estimate',
#'       data=df_plot,
#'       hcaes(x=region, low=prop_resp_lb, high=prop_resp_ub)
#'     ) %>%
#'     hc_xAxis(
#'       type='category',
#'       title=list(enabled=F),
#'       labels = list(
#'         align='left',
#'         reserveSpace=T,
#'         style=list(
#'           fontSize='2vh',
#'           color='#9b9b9b',
#'           fontFamily = "Arial",
#'           fontWeight='300'
#'         )
#'       )
#'     ) %>%
#'     hc_yAxis(
#'       title =list(enabled=F),
#'       gridLineWidth=0,
#'       tickInterval=10,
#'       labels=list(
#'         format="{value}%",
#'         style=list(
#'           fontSize='2vh',
#'           color='#9b9b9b',
#'           fontFamily = "Arial",
#'           fontWeight='300'
#'         )
#'       )
#'     ) %>%
#'     hc_title(
#'       text=title,
#'       align='left',
#'       style = list(
#'         fontSize ="3.2vh",color = "#333333", 
#'         fontFamily = "Arial", fontWeight = "600"
#'       )
#'     ) %>%
#'     hc_subtitle(
#'       text=subtitle,
#'       align='left',
#'       style = list(
#'         fontSize ="2.4vh",color = "#333333", 
#'         fontFamily = "Arial", fontWeight = "350"
#'       )
#'     ) %>%
#'     hc_credits(
#'       enabled=T,
#'       useHTML=T,
#'       text='Chart: <a href="https://data.london.gov.uk/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence</a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
#'       position=list(
#'         align='left',
#'         x=10,
#'         y=-10 
#'       ),
#'       style =list(
#'         fontSize='1.7vh',
#'         color='#9b9b9b'
#'       )
#'     ) %>%
#'     hc_plotOptions(  #48-45
#'       bar = list(
#'         pointWidth=52
#'       ),
#'       errorbar=list(
#'         stemWidth= 1,
#'         whiskerWidth=1,
#'         whiskerLength= 50
#'       )
#'     ) %>%
#'     hc_tooltip(
#'       valueSuffix= '%',
#'       borderWidth=2.6,
#'       style=list(fontSize='1.35vh'),
#'       shape='callout',
#'       shared=T,
#'       useHTML = TRUE#,
#'       #headerFormat = ""#,
#'       #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
#'     ) %>%
#'     hc_drilldown(
#'       allowPointDrilldown = TRUE,
#'       series = list(
#'         list(
#'           name ='London',
#'           id='london',
#'           data = list_parse2(
#'             data.frame(
#'               name = c("b1", "b2", "b3", "b4", "b5"),
#'               value = c(4, 3, 1, 2, 1)
#'             )
#'           ),
#'           type = 'bar',
#'           keys = list('name', 'value', 'drilldown')
#'         )
#'       )
#'     )
#'    
#'   return(plot)
#' }
#' 
#' 
#' 
#' generate_borough_plot <- function(survey_path, bounds_path, release_year, sheet_letter, sheet_number) {
#'   
#'   #' @description 
#'   #' Generates interactive bar chart for regional comparison of survey results
#'   #' 
#'   #' @details  
#'   #' 
#'   #' @param survey_path String path to survey data. Defined in global `SURVEY_PATH`. 
#'   #' @param release_year String data release year (e.g. 2022_23). Defined in global `RELEASE_YEAR`.
#'   #' @param sheet_letter Letter of Excel sheet
#'   #' @param sheet_number Number of Excel sheet
#'   #'
#'   #' @noRd
#'   
#'   
#'   # survey_path <- "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods"
#'   # bounds_path <- "C:/Users/Matt/Downloads/london_421.geojson"
#'   # release_year <- '2023_24'
#'   # sheet_letter <- 'A'
#'   # sheet_number <- 3
#'   # release_year <- '2023_24'
#'   
#'   # Load table
#'   if (file.exists(survey_path)) {
#'     df <- read_ods(
#'       survey_path, sheet=paste0('Table_', sheet_letter, sheet_number), skip=4
#'     )
#'   }
#'   else {
#'     stop(
#'       'Aborting procedure - survey data does not exist in user specified path'
#'     )
#'   }
#'   
#'   # Extract title and subtitle
#'   title <- gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])
#'   subtitle <- "Percentage of respondents"
#'   if (nchar(gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))>1) {
#'     subtitle <- paste0(subtitle," who ", gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))
#'   }
#'   
#'   # Summarise into plotting frame
#'   df_plot <- df %>% 
#'     select(
#'       region=`ITL1 name`,
#'       borough=`Response Breakdown`,
#'       prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
#'       prop_resp_lb=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Lower estimate'),
#'       prop_resp_ub=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Upper estimate'),
#'       num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
#'     ) %>%
#'     filter(grepl('London', region)) %>%
#'     mutate(borough = fct_reorder(borough, prop_resp)) %>%
#'     mutate(across(contains('resp'),~ round(.x,1))) %>%
#'     mutate(value = prop_resp)
#'   
#'   # Define colour palette 
#'   pal <- gla_pal(gla_theme = "default", palette_type = "quantitative", palette_name='core', n = 20, main_colours='blue')
#'   min <- min(df_plot$prop_resp)
#'   max <- max(df_plot$prop_resp)
#' 
#'   # Load London boundaries
#'   if (file.exists(survey_path)) {
#'     bounds <- geojsonio::geojson_read(bounds_path, what = "list")
#'   }
#'   else {
#'     stop(
#'       'Aborting procedure - boundary data does not exist in user specified path'
#'     )
#'   }
#' 
#'   # Plot
#'   plot <- highchart(type='map') %>%
#'     hc_chart(spacingBottom= 50) %>%
#'     hc_subtitle(text = "") %>%
#'     hc_add_series(mapData=bounds, data=list_parse(df_plot), joinBy=c("name", "borough"), name="{point.name}") %>%
#'     hc_colorAxis(
#'       minColor = pal[20],
#'       maxColor = pal[1],
#'       min=min,
#'       max=max
#'     ) %>%
#'     hc_title(
#'       text=title,
#'       align='left',
#'       style = list(
#'         fontSize ="3.2vh",color = "#333333", 
#'         fontFamily = "Arial", fontWeight = "600"
#'       )
#'     ) %>%
#'     hc_subtitle(
#'       text=subtitle,
#'       align='left',
#'       style = list(
#'         fontSize ="2.4vh",color = "#333333", 
#'         fontFamily = "Arial", fontWeight = "350"
#'       )
#'     ) %>%
#'     hc_credits(
#'       enabled=T,
#'       useHTML=T,
#'       text='Chart: <a href="https://data.london.gov.uk/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence</a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
#'       position=list(
#'         align='left',
#'         x=10,
#'         y=-10 
#'       ),
#'       style =list(
#'         fontSize='1.7vh',
#'         color='#9b9b9b'
#'       )
#'     ) %>%
#'     hc_legend(
#'       valueDecimals = 0, 
#'       format = "{value}%"
#'     ) %>%
#'     hc_tooltip(
#'       valueSuffix= '%',
#'       borderWidth=2.6,
#'       shared=T,
#'       style=list(fontSize='1.35vh'),
#'       shape='callout',
#'       useHTML = TRUE, 
#'       headerFormat = "",
#'       pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
#'     ) %>%
#'     hc_mapView(
#'       projection=list(
#'         name='WebMercator'
#'       ),
#'       zoom=9.7,
#'       center=rev(c(51.51279, -0.09184))
#'     )
#'   
#'   return(plot)
#' }
#' 
#' 
#' 
#' 
#' generate_regional_proxy <- function(survey_path, release_year, question) {
#'   
#'   #' @description 
#'   #' Generates interactive bar chart for regional comparison of survey results
#'   #' 
#'   #' @details  
#'   #' 
#'   #' @param survey_path String path to survey data. Defined in global `SURVEY_PATH`. 
#'   #' @param release_year String data release year (e.g. 2022_23). Defined in global `RELEASE_YEAR`.
#'   #' @param sheet_letter Letter of Excel sheet
#'   #' @param sheet_number Number of Excel sheet
#'   #'
#'   #' @noRd
#'   #print(paste0("Scraping ", release_year," Participation Survey from web and writing to file"))
#'   
#'   # survey_path <- "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods"
#'   # release_year <- '2023_24'
#'   # sheet_letter <- 'A'
#'   # sheet_number <- 2
#'   # 
#'   # 
#'   survey_path <- SURVEY_PATH
#'   release_year <- RELEASE_YEAR
#'   question <- list('code'=REGION_QUESTION_LIST[[1]][11],'theme'=REGION_QUESTION_LIST[[2]][11], 'color'=REGION_QUESTION_LIST[[3]][11])
#'   
#'   # Load table
#'   print(question)
#'   #browser()
#'   if (file.exists(survey_path)) {
#'     df <- read_ods(survey_path, sheet=paste0('Table_', question[['code']]), skip=3)
#'   }
#'   else {
#'     stop(
#'       'Aborting procedure - survey data does not exist in user specified path'
#'     )
#'   }
#'   
#'   # Define colour palette
#'   pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1), main_colours=question[['color']])
#'   
#'   
#'   # Extract title and subtitle
#'   title <- paste0('<span style="color:',pal[1],'">', question[['theme']],'</span>')
#'   subtitle <- paste0('Percentage (%) of respondents who ', sub(".*: ", "", tolower(gsub(r"{\s*\[[^\)]+\]}","", paste0(question[['theme']],': ',gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question']))))))
#'   # title <- gsub(r"{\s*\[[^\)]+\]}","", paste0(question[['theme']],': ',gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])))
#'   # subtitle <- "Percentage of respondents (%)"
#'   # if (nchar(gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))>1) {
#'   #   subtitle <- paste0(subtitle," who ", gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))
#'   # }
#'   
#'   #browser()
#'   # Summarise into plotting frame
#'   df_plot <- df %>% 
#'     select(
#'       level=Question,
#'       region=`Response Breakdown`,
#'       prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
#'       prop_resp_lb=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Lower estimate'),
#'       prop_resp_ub=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Upper estimate'),
#'       num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
#'     ) %>%
#'     filter(grepl('ITL1', level)) %>%
#'     mutate(color = case_when(region=='London'~pal[1], T~pal[2])) %>%
#'     mutate(across(contains('resp'),~ round(as.numeric(as.character(.x,1))))) %>%
#'     mutate(region = fct_reorder(region, -prop_resp)) %>%
#'     arrange(region) 
#'   
#'   
#'   return(list(subtitle, df_plot))
#'   
#'  
#'   
#'   return(plot)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 


################################################################################

generate_region_frame <- function(survey_path, release_year, question, rank_direction) {
  
  if (file.exists(survey_path)) {
    df <- read_ods(survey_path, sheet=paste0('Table_', question[['code']]), skip=3)
  }
  else {
    stop(
      'Aborting procedure - survey data does not exist in user specified path'
    )
  }
  #title <- paste0('<span style="color:',pal[1],'">', question[['theme']],'</span>')
  title <- paste0(sub(".*: ", "", gsub(r"{\s*\[[^\)]+\]}","", paste0(question[['theme']],': ',gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])))))
  # Define colour palette
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1), main_colours=question[['color']])
  df_select <- df %>% 
    select(
      level=Question,
      region=`Response Breakdown`,
      prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
      prop_resp_lb=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Lower estimate'),
      prop_resp_ub=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Upper estimate'),
      num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
    ) 
  
  df_region <- df_select %>%
    filter(grepl('ITL1', level)) %>%
    mutate(color = case_when(region=='London'~pal[1], T~pal[2])) %>%
    mutate(across(contains('resp'),~ as.numeric(as.character(.x)))) %>%
    mutate(region = fct_reorder(region, -prop_resp)) %>%
    arrange(region) %>%
    mutate(code = question[['code']]) %>%
    mutate(num_resp = format(round(as.numeric(num_resp), 0), nsmall=0, big.mark=",")) %>%
    mutate(`rank_direction`=rank_direction)
  
  df_region_summary <- df_select %>%
    filter(row_number()==1) %>%
    mutate(level='summary') %>%
    select(-region)
  
  # create drilldown_central and drilldown_error as london-central and london-error, or "" 
  # entirely where Question_lisrt drilldown is 0
  
  return(list('dataframe'=df_region, 'summary'=df_region_summary, 'title'=title))
  
}


generate_borough_frame <- function(survey_path, release_year, question) {
  
  #question <- question <- list('code'=QUESTION_LIST[[2]][[1]][1],'theme'=QUESTION_LIST[[2]][[2]][1], 'color'=QUESTION_LIST[[2]][[3]][1])
  # Load table
  if (file.exists(survey_path)) {
    df <- read_ods(
      survey_path, sheet=paste0('Table_', question[['code']]), skip=4
    )
  }
  else {
    stop(
      'Aborting procedure - survey data does not exist in user specified path'
    )
  }
  # # Extract title and subtitle
  # title <- gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])
  # subtitle <- "Percentage of respondents"
  # if (nchar(gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))>1) {
  #   subtitle <- paste0(subtitle," who ", gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))
  # }
  # Summarise into plotting frame
  df_borough <- df %>% 
    select(
      region=`ITL1 name`,
      borough=`Response Breakdown`,
      prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
      prop_resp_lb=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Lower estimate'),
      prop_resp_ub=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Upper estimate'),
      num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
    ) %>%
    filter(grepl('London', region)) %>%
    mutate(across(contains('resp'),~ .x)) %>%
    mutate(borough = fct_reorder(borough, -prop_resp)) %>%
    arrange(borough) %>%
    mutate(code = question[['code']]) 
    #mutate(num_resp = as.character(format(round(as.numeric(num_resp), 0), nsmall=0, big.mark=",")))

  #browser()
  return(list('dataframe'=df_borough))
}

generate_headline_frame <- function(df_list, region_question_num, themes) {
   #browser()
   rankdf_list <- lapply(
     region_question_num, function(q) {
       dfrank <- df_list[[as.numeric(q)]][['region']][['dataframe']] %>%
         mutate(rank = case_when(rank_direction=='D'~rank(desc(prop_resp), ties.method='min'), T~rank(prop_resp, ties.method='min'))) %>%
         select(region, rank, color)
     }
   )
   themedf_list <- lapply(
     toupper(themes), function(theme) {
       # temp fix##############
       ARTS_QUESTIONS <- 1:4
       LIBRARIES_QUESTIONS <- 5:6
       HERITAGE_QUESTIONS <- 7:10
       SPORT_QUESTIONS <- 11:12
       TOURISM_QUESTIONS <- 13
       ########################
       themedf <- bind_rows(rankdf_list[eval(parse(text=paste0(theme,'_QUESTIONS')))]) %>%
         filter(region=='London') %>%
         mutate(average_rank = round(mean(rank),0)) %>% # TODO possibly go ordinal, not sur ehow that works with y axis, revisit later
         mutate(qnum= length(eval(parse(text=paste0(theme,'_QUESTIONS'))))) %>%
         mutate(theme=stringr::str_to_title(theme)) %>%
         mutate(theme_long=paste0(stringr::str_to_title(theme),'<br>(<i>q</i>=',qnum,')')) %>%
         distinct(theme, .keep_all=T)
     }
   )
   df_headline<- bind_rows(themedf_list)

   return(df_headline)
}


generate_headline_chart <- function(df_headline, browser_height) {

  #browser()
  highchart() %>%
    hc_add_series(
      name='',
      id='',
      type='bar',
      showInLegend=F,
      data=df_headline,
      mapping=hcaes(x = factor(theme_long), y = average_rank, low=10,
                    color=color, name=theme)
      #,
      # dataLabels=list(
      #   enabled=T,
      #   format='{point.qnum}',
      #   color = "white",
      #   align = 'left',
      #   #x = 60,
      #   style = list(textOutline = T)
      # )
      #pointWidth=browser_height/19
    ) %>%
    hc_xAxis(
      type='category',
      title=list(enabled=F),
      labels = list(
        align='left',
        reserveSpace=T,
        style=list(
          fontSize='2vh',
          color='#9b9b9b',
          fontFamily = "Arial",
          fontWeight='300'
        )
        # formatter=paste0(
        #   JS(
        #     "
        #       function() {
        #         return this.value + this.qnum;
        #       }
        #     "
        #   )
        # )
      )
    ) %>%
    hc_yAxis(
      title =list(enabled=F),
      gridLineWidth=0,
      tickInterval=1,
      reversed=T,
      min=1,
      max=10,
      #max=max_borough,
      labels=list(
        formatter=JS(
          "
          function() {
            if (this.value==1)
              return this.value + 'st';
            else if ([4,5,6,7,8,9].includes(this.value))
              return this.value + 'th';
            else if (this.value==2)
              return this.value + 'nd';
            else if (this.value==3)
              return this.value + 'rd';
            else
              return ''
          }
          "
        ),
        style=list(
          fontSize='2vh',
          color='#9b9b9b',
          fontFamily = "Arial",
          fontWeight='300'
        )
      )
    ) %>%
    hc_credits(
      enabled=T,
      useHTML=T,
      #'Chart: <a href="https://data.london.gov.uk/social-evidence-base/" target="_blank" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence (Social Policy) </a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>',
      text='Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24, DCMS</a>',
      href="",
      position=list(
        align='left',
        x=10,
        y=-10
      ),
      style =list(
        fontSize='1.7vh',
        color='#9b9b9b'
      )
    ) %>%
    hc_tooltip(
      valueSuffix= '',
      borderWidth=2.6,
      style=list(fontSize='1.35vh'),
      shape='callout',
      #shared=T,
      useHTML = TRUE,
      headerFormat = "<span style='font-size:1.6vh;'>{point.point.theme}</span><br>" ,
      pointFormat="<span style='color: {point.color}'>\u25CF</span> Average rank: {point.y}"
      #formatter =
      #JS(paste0()
      #)
      #

      #point.point.options.num_resp
      #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
    ) %>%
    hc_exporting(
      enabled=T,
      showExportInProgress=T,
      buttons=list(
        contextButton=list(
          menuItems=c(
            'viewFullscreen', 'separator',  'downloadPNG', 'downloadJPEG', 'downloadSVG', 'separator', 'downloadCSV'
          ),
          symbol="url(data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA1MTIgNTEyIj48IS0tIUZvbnQgQXdlc29tZSBGcmVlIDYuNy4yIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlL2ZyZWUgQ29weXJpZ2h0IDIwMjUgRm9udGljb25zLCBJbmMuLS0+PHBhdGggZmlsbD0iIzliOWI5YiIgZD0iTTI4OCAzMmMwLTE3LjctMTQuMy0zMi0zMi0zMnMtMzIgMTQuMy0zMiAzMmwwIDI0Mi43LTczLjQtNzMuNGMtMTIuNS0xMi41LTMyLjgtMTIuNS00NS4zIDBzLTEyLjUgMzIuOCAwIDQ1LjNsMTI4IDEyOGMxMi41IDEyLjUgMzIuOCAxMi41IDQ1LjMgMGwxMjgtMTI4YzEyLjUtMTIuNSAxMi41LTMyLjggMC00NS4zcy0zMi44LTEyLjUtNDUuMyAwTDI4OCAyNzQuNyAyODggMzJ6TTY0IDM1MmMtMzUuMyAwLTY0IDI4LjctNjQgNjRsMCAzMmMwIDM1LjMgMjguNyA2NCA2NCA2NGwzODQgMGMzNS4zIDAgNjQtMjguNyA2NC02NGwwLTMyYzAtMzUuMy0yOC43LTY0LTY0LTY0bC0xMDEuNSAwLTQ1LjMgNDUuM2MtMjUgMjUtNjUuNSAyNS05MC41IDBMMTY1LjUgMzUyIDY0IDM1MnptMzY4IDU2YTI0IDI0IDAgMSAxIDAgNDggMjQgMjQgMCAxIDEgMC00OHoiLz48L3N2Zz4=)",
          height=40,
          width=48,
          symbolSize=36,
          symbolX=50,
          symbolY=15,
          
          symbolStrokeWidth=2
          
        )
      ),
      # sourceWidth=1600,
      # sourceHeight= 1400,
      sourceWidth=900,
      sourceHeight= 700,
      scale=3,
      # width=1000,
      # height=800,
      chartOptions = list(
        title=list(
          text='Headline rank scores for London by DCMS sector',
          align='left',
          margin=35,
          style = list(
            #fontSize ="3.8vh",
            fontSize =25 ,color = '#353d42',
            fontFamily = "Arial", fontWeight = "450"
          )
        ),
        subtitle=list(
          text="Headline rank scores represent London's average (mean) ranking across selected survey questions relative to the other 8 regions in England; the number of questions, <span style='font-style:normal !important'>q</span>, used to construct this average is shown below the axis label of each data point",
          useHTML=T,
          align='left',
          margin=35,
          style = list(
            #fontSize ="3.8vh",
            fontSize =14.5 ,color = '#353d42',
            fontFamily = "Arial", fontWeight = "250",
            fontStyle='italic'
          )
          
        )
      )
    ) %>%
    hc_legend(
      enabled=F
    ) %>%
    hc_chart(
      spacingBottom= browser_height/12, # needs to be adjusted
      spacingTop=20,
      backgroundColor='#ffffff',
      marginRight=browser_height/12
    ) #%>%
    #hc_plotOptions(
      # dataLabels=list(
      #   enabled=T,
      #   format='{point.qnum}',
      #   color = "white",
      #   style = list(textOutline = T)
      # )
   # )
}



generate_drilldown_chart <- function(question, df_list, theme, browser_height) {
  
 
  # if (theme=='sport') {
  #   browser()
  # }

  question <- as.numeric(question)
  df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp, color, drilldown_central, num_resp)  %>% mutate(id=region)
  df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp_lb, prop_resp_ub, drilldown_error, num_resp) %>% mutate(id=region)
  df_region_summary <- df_list[[question ]][['region']][['summary']]
  
    #rename(fname=num_resp)
  subtitle <-  df_list[[question ]][['region']][['title']]
  # if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
  #   df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
  #     select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp)
  # }
  # else {
  #   df_borough <- data.frame()
  # }
  #browser()
  
  if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
    
    df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
      select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp)
    
    chart <- 
      highchart() %>%
      hc_add_series(
        name='Central estimate',
        id='region-central',
        type='bar', 
        showInLegend=F,
        data=df_region_central, 
        mapping=hcaes(x = region, y = round(prop_resp,1), 
              color=color, drilldown=drilldown_central),
        pointWidth=browser_height/19
      ) %>%
      hc_add_series(
        name='Lower-Upper estimate',
        id='region-error',
        type='errorbar',
        showInLegend=F,
        data=df_region_error,
        mapping=hcaes(x=region, low=round(prop_resp_lb,1),
              high=round(prop_resp_ub,1), drilldown=drilldown_error)#,
      ) %>%
      hc_xAxis(
        type='category',
        title=list(enabled=F),
        labels = list(
          align='left',
          reserveSpace=T,
          style=list(
            fontSize='2vh',
            color='#9b9b9b',
            fontFamily = "Arial",
            fontWeight='300'
          ),
          formatter = JS(
            "function() {
                if (this.value =='London' ) {
                console.log(this.value)
                  return '<span style=\"color: black; font-weight:600; text-decoration:underline\">' + this.value + '</span>'; 
                }
                return this.value;
              }"
          )
        ),
        scrollbar=list(
          enabled=T,
          showFull=F,
          margin=30,
          barBackgroundColor=df_region_central$color[df_region_central$color!='#cccccc'],
          #barBorderColor=df_region_central$color[df_region_central$color!='#cccccc'],
          barBorderColor='#f2f2f2',
          barBorderRadius=0,
          barBorderWidth=1,
          buttonBorderColor='#f2f2f2',
          buttonBorderWidth=1,
          buttonBackgroundColor=df_region_central$color[df_region_central$color!='#cccccc'],
          buttonArrowColor='#ffffff',
          rifleColor='#ffffff',
          zIndex=9
          #,
          #
        ),
        min=0,
        max=8
      ) %>%
      hc_yAxis(
        title =list(enabled=F),
        gridLineWidth=0,
        tickInterval=10,
        floor=0,
        ceiling=100,
        #max=max_borough,
        labels=list(
          format="{value}%",
          style=list(
            fontSize='2vh',
            color='#9b9b9b',
            fontFamily = "Arial",
            fontWeight='300'
          )
        )
        ,
        plotLines=list(
          list(
            value=df_region_summary$prop_resp[1],
            color='#d82222',
            zIndex=99,
            label=list(
              text= 'England',
              verticalAlign='top',
              textAlign='center',
              rotation=0,
              y=-4,
              style=list(
                color='#d82222',
                fontWeight='normal',
                fontSize='1.35vh'
              )
            )
          )
        )
        ,
        plotBands=list(
          list(
            from=0,
            to=0,
            # from=0,
            # to=0,
            zIndex=98, 
            color='#d822221F'
          )
        )
        # ,
        # plotBands=list(
        #   list(
        #     id='region-plotBand',
        #     from=mean(df_region_error$prop_resp_lb),
        #     to=mean(df_region_error$prop_resp_ub),
        #     color='#eeeeee4D',
        #     zIndex=98
        #   )
        # )
      ) %>%
      hc_drilldown(
        allowPointDrilldown=F,
        breadcrumbs=list(
          showFullPath=F,
          useHTML=T,
          format= '&#x25c0; Back to Regions',
          style=list(
            fontSize='2.2vh'
          ),
          position=list(
            align='right',
            y=-47
          )
        ),
        activeAxisLabelStyle = list(
          textDecoration='none',
          color='#9b9b9b',
          fontWeight='normal'
        ),
        # TODO COULD TRY THIS SHIT HERE
        # TODO not working!!!
        #https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/breadcrumbs/format
        series = list(
          list(
            id='london-central',
            name='Central estimate',
            type='bar',
            #keys=c('x', 'y', 'color', 'drilldown', 'n'),
            showInLegend=F,
            pointWidth=browser_height/19,
            #pointWidth=(browser_height/19)*0.272,
            data = list_parse2(
              data.frame(
                borough=df_borough$borough,
                prop_resp=round(df_borough$prop_resp,1),
                num_resp=df_borough$num_resp
              )
              #d
            )
            # ,
            # tooltip = list(
            #   headerFormat = "<span style='font-size:1.6vh;'> {point.key} (n={point.point.options.prop_resp})</span><br>"
            # )
            #,
            # tooltip=list(
            #   headerFormat = "<span style='font-size:1.6vh;'> {point.key} (n={point.fname})</span><br>"
            # )
            #,
            # tooltip=list(
            #   headerFormat = "<span style='font-size:1.6vh;'> {point.key} (n={point.point.num_resp})</span><br>",
            #   shared=T
            # )
          ),
          list(
            id='london-error',
            name='Lower-Upper estimate',
            type='errorbar',
            showInLegend=F,
            data=list_parse2(
              #d2
              data.frame(
                borough=df_borough$borough,
                prop_resp_lb=round(df_borough$prop_resp_lb,1),
                prop_resp_ub=round(df_borough$prop_resp_ub,1)#,
                #'number'=as.numeric(df_borough$num_resp)
              )
            )
            #,
            # tooltip=list(
            #   headerFormat = "<span style='font-size:1.6vh;'> {point.key} (n={point.fname})</span><br>"
            # )
            #,
            # tooltip=list(
            #   headerFormat = "<span style='font-size:1.6vh;'> {point.key} (n={point.point.name})</span><br>",
            #   shared=T
            # )
          )
        )
      ) %>%
      hc_plotOptions(  #48-45
        # bar = list(
        #   pointWidth=browser_height/19
        # ),
        errorbar=list(
          stemWidth= 1,
          whiskerWidth=1,
          whiskerLength= browser_height/19.5
        )
        #,
        # series=list(
        #   events=list(
        #     # get drilldown level
        #     drilldown=JS(
        #       "function() {Shiny.onInputChange('currLevel', [this.series[0].options._levelNumber]);}"
        #     )
        #   )
        # )
      ) %>%
      # hc_title(
      #   text=subtitle,
      #   align='left',
      #   margin=60,
      #   style = list(
      #     fontSize ="3.8vh",color = '#353d42',
      #     fontFamily = "Arial", fontWeight = "450"
      #   )
      # ) %>%
      # hc_subtitle(
      #   text="Click to drilldown into London by Borough",
      #   align='left',
      #   style = list(
      #     fontSize ="2.4vh",color = '#353d42', 
      #     fontFamily = "Arial", fontWeight = "250",
      #     fontStyle='italic'
      #   )
      # ) %>%
      hc_credits(
        enabled=T,
        useHTML=T,
        #'Chart: <a href="https://data.london.gov.uk/social-evidence-base/" target="_blank" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence (Social Policy) </a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>',
        text='Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24, DCMS</a>',
        href="",
        position=list(
          align='left',
          x=10,
          y=-10 
        ),
        style =list(
          fontSize='1.7vh',
          color='#9b9b9b'
        )
      ) %>%
      hc_tooltip(
        valueSuffix= '%',
        borderWidth=2.6,
        style=list(fontSize='1.35vh'),
        shape='callout',
        shared=T,
        useHTML = TRUE,
        headerFormat = "<span style='font-size:1.6vh;'> {point.key}</span><br>"  
        #formatter = 
          #JS(paste0()
        #)
        # 
         
        #point.point.options.num_resp
        #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
      ) %>%
      hc_exporting(
        enabled=T,
        showExportInProgress=T,
        buttons=list(
          contextButton=list(
            menuItems=c(
              'viewFullscreen', 'separator',  'downloadPNG', 'downloadJPEG', 'downloadSVG', 'separator', 'downloadCSV'
            ),
            symbol="url(data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA1MTIgNTEyIj48IS0tIUZvbnQgQXdlc29tZSBGcmVlIDYuNy4yIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlL2ZyZWUgQ29weXJpZ2h0IDIwMjUgRm9udGljb25zLCBJbmMuLS0+PHBhdGggZmlsbD0iIzliOWI5YiIgZD0iTTI4OCAzMmMwLTE3LjctMTQuMy0zMi0zMi0zMnMtMzIgMTQuMy0zMiAzMmwwIDI0Mi43LTczLjQtNzMuNGMtMTIuNS0xMi41LTMyLjgtMTIuNS00NS4zIDBzLTEyLjUgMzIuOCAwIDQ1LjNsMTI4IDEyOGMxMi41IDEyLjUgMzIuOCAxMi41IDQ1LjMgMGwxMjgtMTI4YzEyLjUtMTIuNSAxMi41LTMyLjggMC00NS4zcy0zMi44LTEyLjUtNDUuMyAwTDI4OCAyNzQuNyAyODggMzJ6TTY0IDM1MmMtMzUuMyAwLTY0IDI4LjctNjQgNjRsMCAzMmMwIDM1LjMgMjguNyA2NCA2NCA2NGwzODQgMGMzNS4zIDAgNjQtMjguNyA2NC02NGwwLTMyYzAtMzUuMy0yOC43LTY0LTY0LTY0bC0xMDEuNSAwLTQ1LjMgNDUuM2MtMjUgMjUtNjUuNSAyNS05MC41IDBMMTY1LjUgMzUyIDY0IDM1MnptMzY4IDU2YTI0IDI0IDAgMSAxIDAgNDggMjQgMjQgMCAxIDEgMC00OHoiLz48L3N2Zz4=)",
            height=40,
            width=48,
            symbolSize=36,
            symbolX=50,
            symbolY=20,
            symbolStrokeWidth=2
          )
        ),
        # sourceWidth=1600,
        # sourceHeight= 1400,
        sourceWidth=900,
        sourceHeight= 700,
        scale=3,
        # width=1000,
        # height=800,
        chartOptions = list(
          title=list(
            text=subtitle,
            align='left',
            margin=35,
            style = list(
              #fontSize ="3.8vh",
              fontSize =25 ,color = '#353d42',
              fontFamily = "Arial", fontWeight = "450"
            )
          )
        )
      ) %>%
      hc_legend(
        enabled=F
      ) %>%
      hc_chart(
        spacingBottom= browser_height/12, # needs to be adjusted
        spacingTop=20,
        backgroundColor='#ffffff',
        animation=list(
          duration=1000
        ),
        #backgroundColor='#fafafa',
        events=list(
          
          # load=JS(
          #   "
          #   function() {
          #   levDrillDown = -1;
          #   levDrillUp=0;
          #   "
          #   ),
          
          drilldown=JS(
            paste0(
              # "
              # function(e) {
              # Shiny.onInputChange('",theme,"_currLevel', [this.series[0].options._levelNumber]);
              # }
              # "
              "
              function() {
              Shiny.onInputChange('",theme,"_currLevel',  0);
              }
              "
  
            
            
            
            # paste0(
            #   "
            #   function() {
            #     this.yAxis[0].removePlotLine('region-plotLine');
            #     this.yAxis[0].addPlotLine(
            #       {
            #         id:'borough-plotLine',
            #         value:",mean(df_borough$prop_resp),",
            #         color:'#d82222',
            #         zIndex:99
            #       }
            #     );
            #     Shiny.onInputChange('currLevel', [this.series[0].options._levelNumber]);
            #   }
            #   "
            )
          )
          ,
          drillup=JS(
            # paste0(
            #   "
            #   function() {
            #   Shiny.onInputChange('currLevel', [this.series[0].options._levelNumber]);
            #   }
            #   "
            paste0(
              "
              function(e) {
              Shiny.onInputChange('",theme,"_currLevel',  1);
              }
  
              "
  
              # "
              # function() {
              # this.yAxis[0].removePlotLine('borough-plotLine');
              #   this.yAxis[0].addPlotLine(
              #     {
              #       id:'region-plotLine',
              #       value:",mean(df_region_central$prop_resp),",
              #       color:'#d82222',
              #       zIndex:99
              #     }
              #   );
              #   Shiny.onInputChange('currLevel', [this.series[0].options._levelNumber]);
              # }
              # "
            )
          )
          #,
          # drillupAll=JS(
          #   # paste0(
          #   #   "
          #   #   function() {
          #   #   Shiny.onInputChange('currLevel', [this.series[0].options._levelNumber]);
          #   #   }
          #   #   "
          #   paste0(
          #     "
          #     function(e) {
          #     Shiny.onInputChange('",theme,"_currLevel', [this.series[0].options._levelNumber]);
          #     }
          # 
          #     "
          #     
          #     # "
          #     # function() {
          #     # this.yAxis[0].removePlotLine('borough-plotLine');
          #     #   this.yAxis[0].addPlotLine(
          #     #     {
          #     #       id:'region-plotLine',
          #     #       value:",mean(df_region_central$prop_resp),",
          #     #       color:'#d82222',
          #     #       zIndex:99
          #     #     }
          #     #   );
          #     #   Shiny.onInputChange('currLevel', [this.series[0].options._levelNumber]);
          #     # }
          #     # "
          #   )
          # )
        )
      ) %>%
      hc_add_event_point(event = "mouseOver")
    
  }
  
  else {
    df_borough <- data.frame()
    chart <- 
      highchart() %>%
      hc_add_series(
        name='Central estimate',
        id='region-central',
        type='bar', 
        showInLegend=F,
        data=df_region_central, 
        mapping=hcaes(x = region, y = round(prop_resp,1), 
                      color=color),
        pointWidth=browser_height/19
      ) %>%
      hc_add_series(
        name='Lower-Upper estimate',
        id='region-error',
        type='errorbar',
        showInLegend=F,
        data=df_region_error,
        mapping=hcaes(x=region, low=round(prop_resp_lb,1),
                      high=round(prop_resp_ub,1))#,
      ) %>%
      hc_xAxis(
        type='category',
        title=list(enabled=F),
        labels = list(
          align='left',
          reserveSpace=T,
          style=list(
            fontSize='2vh',
            color='#9b9b9b',
            fontFamily = "Arial",
            fontWeight='300'
          )
        ),
        scrollbar=list(
          enabled=T,
          showFull=F,
          zIndex=0
        ),
        min=0,
        max=8
      ) %>%
      hc_yAxis(
        title =list(enabled=F),
        gridLineWidth=0,
        tickInterval=10,
        #max=max_borough,
        labels=list(
          format="{value}%",
          style=list(
            fontSize='2vh',
            color='#9b9b9b',
            fontFamily = "Arial",
            fontWeight='300'
          )
        )
        ,
        plotLines=list(
          list(
            value=df_region_summary$prop_resp[1],
            color='#d82222',
            zIndex=99,
            label=list(
              text= 'England',
              verticalAlign='top',
              textAlign='center',
              rotation=0,
              y=-4,
              style=list(
                color='#d82222',
                fontWeight='normal',
                fontSize='1.35vh'
              )
            )
          )
        )
        ,
        plotBands=list(
          list(
            from=0,
            to=0,
            # from=0,
            # to=0,
            zIndex=98, 
            color='#d822221F'
          )
        )
        # ,
        # plotBands=list(
        #   list(
        #     id='region-plotBand',
        #     from=mean(df_region_error$prop_resp_lb),
        #     to=mean(df_region_error$prop_resp_ub),
        #     color='#eeeeee4D',
        #     zIndex=98
        #   )
        # )
      ) %>%
      hc_plotOptions(  #48-45
        # bar = list(
        #   pointWidth=browser_height/19
        # ),
        errorbar=list(
          stemWidth= 1,
          whiskerWidth=1,
          whiskerLength= browser_height/19.5
        )
        #,
        # series=list(
        #   events=list(
        #     # get drilldown level
        #     drilldown=JS(
        #       "function() {Shiny.onInputChange('currLevel', [this.series[0].options._levelNumber]);}"
        #     )
        #   )
        # )
      ) %>%
      # hc_title(
      #   text=subtitle,
      #   align='left',
      #   margin=60,
      #   style = list(
      #     fontSize ="3.8vh",color = '#353d42',
      #     fontFamily = "Arial", fontWeight = "450"
      #   )
      # ) %>%
      # hc_subtitle(
      #   text="Click to drilldown into London by Borough",
    #   align='left',
    #   style = list(
    #     fontSize ="2.4vh",color = '#353d42', 
    #     fontFamily = "Arial", fontWeight = "250",
    #     fontStyle='italic'
    #   )
    # ) %>%
    hc_credits(
      enabled=T,
      useHTML=T,
      #'Chart: <a href="https://data.london.gov.uk/social-evidence-base/" target="_blank" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence (Social Policy) </a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>',
      text='Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24, DCMS</a>',
      href="",
      position=list(
        align='left',
        x=10,
        y=-10 
      ),
      style =list(
        fontSize='1.7vh',
        color='#9b9b9b'
      )
    ) %>%
      hc_tooltip(
        valueSuffix= '%',
        borderWidth=2.6,
        style=list(fontSize='1.35vh'),
        shape='callout',
        shared=T,
        useHTML = TRUE,
        headerFormat = "<span style='font-size:1.6vh;'> {point.key}</span><br>"  
        #formatter = 
        #JS(paste0()
        #)
        # 
        
        #point.point.options.num_resp
        #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
      ) %>%
      hc_exporting(
        enabled=T,
        showExportInProgress=T,
        buttons=list(
          contextButton=list(
            menuItems=c(
              'viewFullscreen',  'separator',  'downloadPNG', 'downloadJPEG', 'downloadSVG', 'separator', 'downloadCSV'
            ),
            symbol="url(data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA1MTIgNTEyIj48IS0tIUZvbnQgQXdlc29tZSBGcmVlIDYuNy4yIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlL2ZyZWUgQ29weXJpZ2h0IDIwMjUgRm9udGljb25zLCBJbmMuLS0+PHBhdGggZmlsbD0iIzliOWI5YiIgZD0iTTI4OCAzMmMwLTE3LjctMTQuMy0zMi0zMi0zMnMtMzIgMTQuMy0zMiAzMmwwIDI0Mi43LTczLjQtNzMuNGMtMTIuNS0xMi41LTMyLjgtMTIuNS00NS4zIDBzLTEyLjUgMzIuOCAwIDQ1LjNsMTI4IDEyOGMxMi41IDEyLjUgMzIuOCAxMi41IDQ1LjMgMGwxMjgtMTI4YzEyLjUtMTIuNSAxMi41LTMyLjggMC00NS4zcy0zMi44LTEyLjUtNDUuMyAwTDI4OCAyNzQuNyAyODggMzJ6TTY0IDM1MmMtMzUuMyAwLTY0IDI4LjctNjQgNjRsMCAzMmMwIDM1LjMgMjguNyA2NCA2NCA2NGwzODQgMGMzNS4zIDAgNjQtMjguNyA2NC02NGwwLTMyYzAtMzUuMy0yOC43LTY0LTY0LTY0bC0xMDEuNSAwLTQ1LjMgNDUuM2MtMjUgMjUtNjUuNSAyNS05MC41IDBMMTY1LjUgMzUyIDY0IDM1MnptMzY4IDU2YTI0IDI0IDAgMSAxIDAgNDggMjQgMjQgMCAxIDEgMC00OHoiLz48L3N2Zz4=)",
            height=40,
            width=48,
            symbolSize=36,
            symbolX=50,
            symbolY=20,
            symbolStrokeWidth=2
            # theme=list(
            #   style=list(
            #     zIndex=999
            #   )
            # )
          )
        ),
        # sourceWidth=1600,
        # sourceHeight= 1400,
        sourceWidth=900,
        sourceHeight= 700,
        scale=3,
        # width=1000,
        # height=800,
        chartOptions = list(
          title=list(
            text=subtitle,
            align='left',
            margin=35,
            style = list(
              #fontSize ="3.8vh",
              fontSize =25 ,color = '#353d42',
              fontFamily = "Arial", fontWeight = "450"
            )
          )
        )
      ) %>%
      hc_legend(
        enabled=F
      ) %>%
      hc_chart(
        spacingBottom= browser_height/12, # needs to be adjusted
        spacingTop=20,
        backgroundColor='#ffffff',
        animation=list(
          duration=1000
        )
      ) %>%
      hc_add_event_point(event = "mouseOver")
    
  }

    
  #chart
  #browser()
  # TODO Add tooltip to plotline and plotband https://jsfiddle.net/BlackLabel/nx0uo1rk/
  # or dummy series using https://jsfiddle.net/BlackLabel/2Ln05yes/
  # Add the line(s) -or try
  # If not add England bar, optional add or remove ci lines maybe
  # then revisit fucking tooltip n on drilldown
  # bulleted text - bvasically done
  #print('yo')
  return(chart)
}


update_drilldown_chart <- function(question, df_list, chart, comp_ops, curr_level=NULL) {
  
  `%ni%` <- Negate(`%in%`)
  question <- as.numeric(question)
  df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp, num_resp, color, drilldown_central) %>% mutate(id=region)
  df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp_lb, prop_resp_ub, num_resp, drilldown_error)  %>% mutate(id=region)
  subtitle <-  df_list[[question ]][['region']][['title']]
  # if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
  #   
  # }
  # else {
  #   df_borough <- data.frame()
  # }
  #max_borough <- max(df_borough$prop_resp_ub)
  ldn_index <- which(df_region_central$region=='London')-1
  ldn_color <- unique(df_region_central$color[df_region_central$color!='#cccccc'])
  reg_index <- c(0:8)[c(0:8) !=ldn_index]
  
  
  #browser()
  if (is.null(curr_level)) {
    #browser()
    #browser()
    if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==F) {
      # if (question==12) {
      #   browser()
      # }
      df_borough <- data.frame()
      proxy <- highchartProxy(chart) %>%
      hcpxy_update_series(
        id='region-central',
        data=list_parse2(
          data.frame(
            region=df_region_central$region,
            prop_resp=round(df_region_central$prop_resp,1)#,
            #drilldown=rep(NA, 9)
          )
        )
      ) %>%
      hcpxy_update_series(
        id='region-error',
        data=list_parse2(
          data.frame( 
            region=df_region_error$region,
            prop_resp_lb=round(df_region_error$prop_resp_lb,1),
            prop_resp_ub=round(df_region_error$prop_resp_ub,1)#,
            #drilldown=rep(NA, 9)
          )
        )
      )  %>%
      hcpxy_update_point(
          id='region-central',
          id_point = ldn_index,
          color=ldn_color,
          #drilldown='london-central',
          marker=list(
            color=ldn_color,
            fillColor=ldn_color
          )
        ) %>%
        hcpxy_update_point(
          id='region-error',
          id_point = ldn_index#,
          #drilldown='london-error'
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[1],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[2],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[3],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[4],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[5],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[6],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[7],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[8],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update(
          xAxis=list(
            labels = list(
              style=list(
                color='#9b9b9b'
              )
            )
          ) 
        )
    }
    if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
      # if (question==12) {
      #   browser()
      # }
      df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
        select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp)
      proxy <- highchartProxy(chart) %>%
        hcpxy_update_series(
          id='region-central',
          data=list_parse2(
            data.frame(
              region=df_region_central$region,
              prop_resp=round(df_region_central$prop_resp,1),
              drilldown=rep('london-central', 9)
            )
          )
        ) %>%
        hcpxy_update_series(
          id='region-error',
          data=list_parse2(
            data.frame( 
              region=df_region_error$region,
              prop_resp_lb=round(df_region_error$prop_resp_lb,1),
              prop_resp_ub=round(df_region_error$prop_resp_ub,1),
              drilldown=rep('london-error', 9)
            )
          )
        )  %>%
        hcpxy_update_point(
          id='region-central',
          id_point = ldn_index,
          color=ldn_color,
          drilldown='london-central',
          marker=list(
            color=ldn_color,
            fillColor=ldn_color
          )
        ) %>%
        hcpxy_update_point(
          id='region-error',
          id_point = ldn_index,
          drilldown='london-error'
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[1],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[2],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[3],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[4],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[5],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[6],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[7],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
        hcpxy_update_point(
          id='region-central',
          id_point = reg_index[8],
          color='#cccccc',
          marker=list(
            color='#cccccc',
            fillColor='#cccccc'
          )
        ) %>%
      hcpxy_update(
        drilldown=list(
          series=list(
            list(
              id='london-central',
              name='Central estimate',
              type='bar',
              pointWidth=shinybrowser::get_height()/19.5,
              data=list_parse2(
                data.frame(
                  borough=df_borough$borough,
                  prop_resp=round(df_borough$prop_resp,1),
                  num_resp=df_borough$num_resp
                )
              )
            ),
            list(
              id='london-error',
              name='Lower-Upper estimate',
              type='errorbar',
              data=list_parse2(
                data.frame(
                  borough=df_borough$borough,
                  prop_resp_lb=round(df_borough$prop_resp_lb,1),
                  prop_resp_ub=round(df_borough$prop_resp_ub,1)
                  
                )
              )
            )
          )
        )
        #,
        # xAxis = list(
        #   type='category'
        # )
      )
    }
  }
  
  else if (curr_level==1) {
    df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
      select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp)
    #browser()
    proxy <- highchartProxy(chart) %>%
      # hcpxy_set_data(
      #   type='bar',
      #   data=df_region_central,
      #   mapping=hcaes(
      #     x=region,
      #     y=round(prop_resp,1),
      #     drilldown=drilldown_central
      #   ),
      #   updatePoints = F,
      #   redraw=T
      # ) %>%
      # hcpxy_set_data(
      #   type='errorbar',
      #   data=df_region_error,
      #   mapping=hcaes(
      #     x=factor(region),
      #     low=round(prop_resp_lb,1),
      #     high=round(prop_resp_ub,1),
      #     drilldown=drilldown_error,
      #     id=id
      #   ),
      #   updatePoints = T,
      #   redraw=F
      # ) %>%
    
    # mapping=hcaes(x = region, y = round(prop_resp,1), 
    #               color=color, drilldown=drilldown_central, id=id),
    
    
    
    
    
    
    # hcpxy_update_series(
    #   id='region-central',
    #   data=#=df_region_central$prop_resp
    #     list_parse2(
    #       data.frame(
    #         'region'=df_region_central$region,
    #         'prop_resp'=round(df_region_central$prop_resp,1),
    #         'color'=df_region_central$color,
    #         'drilldown'=df_region_central$drilldown_central,
    #         'num_resp'=df_region_central$num_resp, 'id'=df_region_central$id
    #       )
    #     )
    # ) %>%
    #   hcpxy_update_series(
    #     id='region-error',
    #     data=list_parse2(
    #       data.frame(
    #         'region'=df_region_error$region,
    #         'prop_resp_lb'=round(df_region_error$prop_resp_lb,1),
    #         'prop_resp_ub'=round(df_region_error$prop_resp_ub,1),
    #         'drilldown'=df_region_error$drilldown_error,
    #         'num_resp'=df_region_error$num_resp, id=df_region_error$id
    #       )
    #     )
    #   ) %>%
    # hcpxy_update_series(
    #   id='region-central',
    #   data=list_parse2(
    #     data.frame(
    #       region=df_region_central$region,
    #       prop_resp=round(df_region_central$prop_resp,1),
    #       drilldown=rep('london-central', 9)
    #     ))#,
    #   #,
    #   #drilldown=df_region_central$drilldown_central
    # ) %>%
    hcpxy_update_series(
      id='region-central',
      data=list_parse2(
        data.frame(
          region=df_region_central$region,
          prop_resp=round(df_region_central$prop_resp,1),
          drilldown=rep('london-central', 9)
        ))#,
      #,
      #drilldown=df_region_central$drilldown_central
    ) %>%
      hcpxy_update_series(
        id='region-error',
        data=list_parse2(
          data.frame(
            region=df_region_error$region,
            prop_resp_lb=round(df_region_error$prop_resp_lb,1),
            prop_resp_ub=round(df_region_error$prop_resp_ub,1),
            drilldown=rep('london-error', 9)
          )
        )
      ) %>%
      # hcpxy_set_data(
      #   type='bar',
      #   data=df_region_central,
      #   mapping=hcaes(
      #     x=region,
      #     y=round(prop_resp,1),
      #     drilldown=drilldown_central
      #   ),
      #   updatePoints = F,
      #   redraw=F
      # ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = ldn_index,
        color=ldn_color,
        drilldown='london-central',
        marker=list(
          color=ldn_color,
          fillColor=ldn_color
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[1],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[2],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[3],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[4],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[5],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[6],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[7],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update_point(
        id='region-central',
        id_point = reg_index[8],
        color='#cccccc',
        marker=list(
          color='#cccccc',
          fillColor='#cccccc'
        )
      ) %>%
      hcpxy_update(
        drilldown=list(
          series=list(
            list(
              id='london-central',
              name='Central estimate',
              type='bar',
              pointWidth=shinybrowser::get_height()/19.5,
              data=list_parse2(
                data.frame(
                  borough=df_borough$borough,
                  prop_resp=round(df_borough$prop_resp,1),
                  num_resp=df_borough$num_resp
                )
              )
            ),
            list(
              id='london-error',
              name='Lower-Upper estimate',
              type='errorbar',
              data=list_parse2(
                data.frame(
                  borough=df_borough$borough,
                  prop_resp_lb=round(df_borough$prop_resp_lb,1),
                  prop_resp_ub=round(df_borough$prop_resp_ub,1)
                )
              )
            )
          )
        )
      )
    # if ('mean'%in%comp_ops & 'error'%ni%comp_ops) {
    #   proxy <- proxy %>%
    #     hcpxy_update(
    #       yAxis=list(
    #         plotLines=list(
    #           list(
    #             value=mean(df_region_central$prop_resp),
    #             color='#d82222',
    #             zIndex=99,
    #             label=list(
    #               text= 'England',
    #               verticalAlign='top',
    #               textAlign='center',
    #               rotation=0,
    #               y=-4,
    #               style=list(
    #                 color='#d82222',
    #                 fontWeight='normal',
    #                 fontSize='1.35vh'
    #               )
    #             )
    #           )
    #         ),
    #         plotBands=list(
    #           list(
    #             from=mean(df_region_error$prop_resp_lb),
    #             to=mean(df_region_error$prop_resp_ub),
    #             color='#d8222200',
    #             zIndex=98,
    #             label=list(
    #               text= ''
    #             )
    #           )
    #         )
    #       )
    #     )
    # }
    # else if ('mean'%ni%comp_ops & 'error'%in%comp_ops) {
    #   proxy <- proxy %>%
    #     hcpxy_update(
    #       yAxis=list(
    #         plotLines=list(
    #           list(
    #             value=mean(df_region_central$prop_resp),
    #             color='#d8222200',
    #             zIndex=99,
    #             label=list(
    #               text= ''
    #             )
    #           )
    #         ),
    #         plotBands=list(
    #           list(
    #             from=mean(df_region_error$prop_resp_lb),
    #             to=mean(df_region_error$prop_resp_ub),
    #             color='#d822221F',
    #             zIndex=98,
    #             label=list(
    #               text= 'England',
    #               verticalAlign='top',
    #               textAlign='center',
    #               rotation=0,
    #               y=-4,
    #               style=list(
    #                 color='#d82222',
    #                 fontWeight='normal',
    #                 fontSize='1.35vh'
    #               )
    #             )
    #           )
    #         )
    #       )
    #     )
    # }
    # else if ('mean'%in%comp_ops & 'error'%in%comp_ops) {
    #   proxy <- proxy %>%
    #     hcpxy_update(
    #       yAxis=list(
    #         plotLines=list(
    #           list(
    #             value=mean(df_region_central$prop_resp),
    #             color='#d82222',
    #             zIndex=99,
    #             label=list(
    #               text= 'England',
    #               verticalAlign='top',
    #               textAlign='center',
    #               rotation=0,
    #               y=-4,
    #               style=list(
    #                 color='#d82222',
    #                 fontWeight='normal',
    #                 fontSize='1.35vh'
    #               )
    #             )
    #           )
    #         ),
    #         plotBands=list(
    #           list(
    #             from=mean(df_region_error$prop_resp_lb),
    #             to=mean(df_region_error$prop_resp_ub),
    #             color='#d822221F',
    #             zIndex=98,
    #             label=list(
    #               text= ''
    #             )
    #           )
    #         )
    #       )
    #     )
    # }
    # else {
    #   proxy <- proxy %>%
    #     hcpxy_update(
    #       yAxis=list(
    #         plotLines=list(
    #           list(
    #             value=mean(df_region_central$prop_resp),
    #             color='#d8222200',
    #             zIndex=99,
    #             label=list(
    #               text= ''
    #             )
    #           )
    #         ),
    #         plotBands=list(
    #           list(
    #             from=mean(df_region_error$prop_resp_lb),
    #             to=mean(df_region_error$prop_resp_ub),
    #             color='#d8222200',
    #             zIndex=98,
    #             label=list(
    #               text= ''
    #             )
    #           )
    #         )
    #       )
    #     )
    # }
  }
  
  else {  # THIS IS THE PROBLEM!!!
    df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
      select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp)
    proxy <- highchartProxy(chart) #%>%
      # hcpxy_set_data(
      #   type='bar',
      #   data=df_region_central,
      #   mapping=hcaes(
      #     x=region,
      #     y=round(prop_resp,1),
      #     drilldown=drilldown_central
      #   ),
      #   updatePoints = F,
      #   redraw=T
      #  ) %>%
      # hcpxy_set_data(
      #   type='errorbar',
      #   data=df_region_error,
      #   mapping=hcaes(
      #     x=factor(region),
      #     low=round(prop_resp_lb,1),
      #     high=round(prop_resp_ub,1),
      #     drilldown=drilldown_error,
      #     id=id
      #   ),
      #   updatePoints = T,
      #   redraw=F
      # ) #%>%
      # hcpxy_set_data(
      #   'errorbar',
      #   df_region_error,
      #   mapping=hcaes(
      #     region=factor(region),
      #     prop_resp_lb=round(prop_resp_lb,1),
      #     prop_resp_ub=round(prop_resp_ub,1),
      #     num_resp=num_resp, id=id
      #   ),
      #   updatePoints = T, 
      #   redraw=F
      # ) #%>%
    
    # 
    # hcpxy_set_data(
    #   type='errorbar',
    #   data=data.frame(
    #     borough=df_borough$borough,
    #     prop_resp_lb=round(df_borough$prop_resp_lb,1),
    #     prop_resp_ub=round(df_borough$prop_resp_ub,1)
    #   ),
    #   mapping=hcaes(
    #     x=borough, low=prop_resp_lb, high=prop_resp_ub
    #   ),
    #   redraw=F
    # ) %>%
    # hcpxy_redraw()
    
    # %>%
    # hcpxy_set_data(
    #   type='bar',
    #   data=data.frame(
    #     borough=df_borough$borough,
    #     prop_resp=round(df_borough$prop_resp,1),
    #     num_resp=df_borough$num_resp
    #   )
    #   ,
    #   mapping=hcaes(
    #     x = factor(borough), y = prop_resp
    #   )
    #   ,
    #   redraw=T
    # )# %>%
    #hcpxy_redraw()
    
    
    
    
    
    
    # hcpxy_update_series(
    #   id='region-central',
    #   data=#=df_region_central$prop_resp
    #     list_parse2(
    #       data.frame(
    #         'region'=df_region_central$region,
    #         'prop_resp'=round(df_region_central$prop_resp,1),
    #         'color'=df_region_central$color,
    #         'drilldown'=df_region_central$drilldown_central,
    #         'num_resp'=df_region_central$num_resp, 'id'=df_region_central$id
    #       )
    #     )
    # ) %>%
    #   hcpxy_update_series(
    #     id='region-error',
    #     data=list_parse2(
    #       data.frame(
    #         'region'=df_region_error$region,
    #         'prop_resp_lb'=round(df_region_error$prop_resp_lb,1),
    #         'prop_resp_ub'=round(df_region_error$prop_resp_ub,1),
    #         'drilldown'=df_region_error$drilldown_error,
    #         'num_resp'=df_region_error$num_resp, id=df_region_error$id
    #       )
    #     )
    #   ) 
    # hcpxy_set_data(
    #   type='bar',
    #   data=df_region_central,
    #   mapping=hcaes(
    #     x=region,
    #     y=round(prop_resp,1),
    #     drilldown=drilldown_central
    #   ),
    #   updatePoints = T,
    #   redraw=F
    # ) %>%
    
    
    
    
    
    
    
    
    
    
    
    
      # hcpxy_update_series(
      #   id='region-central',
      #   data=list_parse2(
      #     data.frame(
      #       region=df_region_central$region,
      #       prop_resp=round(df_region_central$prop_resp,1),
      #       drilldown=rep('london-central', 9)
      #     ))#,
      #   #,
      #   #drilldown=df_region_central$drilldown_central
      # ) %>%
      # hcpxy_update_series(
      #   id='region-error',
      #   data=list_parse2(
      #     data.frame(
      #       region=df_region_error$region,
      #       prop_resp_lb=round(df_region_error$prop_resp_lb,1),
      #       prop_resp_ub=round(df_region_error$prop_resp_ub,1),
      #       drilldown=df_region_error$drilldown_error
      #     )
      #   )
      # ) %>%
      # hcpxy_update(
      #   drilldown=list(
      #     series=list(
      #       list(
      #         id='london-central',
      #         name='Central estimate',
      #         type='bar',
      #         pointWidth=shinybrowser::get_height()/19.5,
      #         data=list_parse2(
      #           data.frame(
      #             borough=df_borough$borough,
      #             prop_resp=round(df_borough$prop_resp,1),
      #             num_resp=df_borough$num_resp
      #           )
      #         )
      #       ),
      #       list(
      #         id='london-error',
      #         name='Lower-Upper estimate',
      #         type='errorbar',
      #         data=list_parse2(
      #           data.frame(
      #             borough=df_borough$borough,
      #             prop_resp_lb=round(df_borough$prop_resp_lb,1),
      #             prop_resp_ub=round(df_borough$prop_resp_ub,1),
      #             num_resp=df_borough$num_resp
      # 
      #           )
      #         )
      #       )
      #     )
      #   )#,
      # #   xAxis = list(
      # #     type='category'
      # #   )
      # )
      # 
      #   hcpxy_update_series(
      #     id='london-central',
      #     data=list_parse2(
      #       data.frame(
      #         borough=df_borough$borough,
      #         prop_resp=round(df_borough$prop_resp,1),
      #         num_resp=df_borough$num_resp
      #       )
      #     )
      #     #,
      #     #drilldown=df_region_central$drilldown_central
      #   ) %>%
      #   hcpxy_update_series(
      #     id='london-error',
      #     data=list_parse2(
      #       data.frame(
      #         borough=df_borough$borough,
      #         prop_resp_lb=round(df_borough$prop_resp_lb,1),
      #         prop_resp_ub=round(df_borough$prop_resp_ub,1),
      #         num_resp=df_borough$num_resp
      #         
      #       )
      #     )
      #   )# %>%
      
      
      



    
  }
  
  #proxy <- proxy %>% hcpxy_redraw()
  #browser()
  return(proxy)
  
}


update_drilldown_chart_summary <- function(question, df_list, chart, comp_ops, curr_level=NULL) {
  
  #browser()
  `%ni%` <- Negate(`%in%`)
  question <- as.numeric(question)
  df_region_summary <- df_list[[question ]][['region']][['summary']]
  
  
  # df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
  #   select(region, prop_resp, num_resp, color, drilldown_central) %>% mutate(id=region)
  # df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
  #   select(region, prop_resp_lb, prop_resp_ub, num_resp, drilldown_error)  %>% mutate(id=region)
  # subtitle <-  df_list[[question ]][['region']][['title']]
  df_borough <- df_list[[question ]][['borough']][['dataframe']]
  # #max_borough <- max(df_borough$prop_resp_ub)
  # ldn_index <- which(df_region_central$region=='London')-1
  # ldn_color <- unique(df_region_central$color[df_region_central$color!='#cccccc'])
  # reg_index <- c(0:8)[c(0:8) !=ldn_index]
  
  if (is.null(curr_level)) {
    #browser()
    proxy <- highchartProxy(chart) 
    if ('mean'%in%comp_ops & 'error'%ni%comp_ops) {
      proxy <- proxy %>%
      hcpxy_update(
        yAxis=list(
          plotLines=list(
            list(
              value=df_region_summary$prop_resp[1],
              color='#d82222',
              zIndex=99,
              label=list(
                text= 'England',
                verticalAlign='top',
                textAlign='center',
                rotation=0,
                y=-4,
                style=list(
                  color='#d82222',
                  fontWeight='normal',
                  fontSize='1.35vh'
                )
              )
            )
          ),
          plotBands=list(
            list(
              from=df_region_summary$prop_resp_lb[1],
              to=df_region_summary$prop_resp_ub[1],
              color='#d8222200',
              zIndex=98,
              label=list(
                text= ''
              )
            )
          )
        )
      )
    }
    else if ('mean'%ni%comp_ops & 'error'%in%comp_ops) {
      proxy <- proxy %>%
      hcpxy_update(
        yAxis=list(
          plotLines=list(
            list(
              value=df_region_summary$prop_resp[1],
              color='#d8222200',
              zIndex=99,
              label=list(
                text= ''
              )
            )
          ),
          plotBands=list(
            list(
              from=df_region_summary$prop_resp_lb[1],
              to=df_region_summary$prop_resp_ub[1],
              color='#d822221F',
              zIndex=98,
              label=list(
                text= 'England',
                verticalAlign='top',
                textAlign='center',
                rotation=0,
                y=-4,
                style=list(
                  color='#d82222',
                  fontWeight='normal',
                  fontSize='1.35vh'
                )
              )
            )
          )
        )
      )
    }
    else if ('mean'%in%comp_ops & 'error'%in%comp_ops) {
      proxy <- proxy %>%
      hcpxy_update(
        yAxis=list(
          plotLines=list(
            list(
              value=df_region_summary$prop_resp[1],
              color='#d82222',
              zIndex=99,
              label=list(
                text= 'England',
                verticalAlign='top',
                textAlign='center',
                rotation=0,
                y=-4,
                style=list(
                  color='#d82222',
                  fontWeight='normal',
                  fontSize='1.35vh'
                )
              )
            )
          ),
          plotBands=list(
            list(
              from=df_region_summary$prop_resp_lb[1],
              to=df_region_summary$prop_resp_ub[1],
              color='#d822221F',
              zIndex=98,
              label=list(
                text= ''
              )
            )
          )
        )
      )
    }
    else {
      proxy <- proxy %>%
      hcpxy_update(
        yAxis=list(
          plotLines=list(
            list(
              value=df_region_summary$prop_resp[1],
              color='#d8222200',
              zIndex=99,
              label=list(
                text= ''
              )
            )
          ),
          plotBands=list(
            list(
              from=df_region_summary$prop_resp_lb[1],
              to=df_region_summary$prop_resp_ub[1],
              color='#d8222200',
              zIndex=98,
              label=list(
                text= ''
              )
            )
          )
        )
      )
    }
 }
  
  else if (curr_level==1) {
    #browser()
    proxy <- highchartProxy(chart) 
      if ('mean'%in%comp_ops & 'error'%ni%comp_ops) {
        proxy <- proxy %>%
          hcpxy_update(
            yAxis=list(
              plotLines=list(
                list(
                  value=df_region_summary$prop_resp[1],
                  color='#d82222',
                  zIndex=99,
                  label=list(
                    text= 'England',
                    verticalAlign='top',
                    textAlign='center',
                    rotation=0,
                    y=-4,
                    style=list(
                      color='#d82222',
                      fontWeight='normal',
                      fontSize='1.35vh'
                    )
                  )
                )
              ),
              plotBands=list(
                list(
                  from=df_region_summary$prop_resp_lb[1],
                  to=df_region_summary$prop_resp_ub[1],
                  color='#d8222200',
                  zIndex=98,
                  label=list(
                    text= ''
                  )
                )
              )
            )
          )
      }
      else if ('mean'%ni%comp_ops & 'error'%in%comp_ops) {
        proxy <- proxy %>%
          hcpxy_update(
            yAxis=list(
              plotLines=list(
                list(
                  value=df_region_summary$prop_resp[1],
                  color='#d8222200',
                  zIndex=99,
                  label=list(
                    text= ''
                  )
                )
              ),
              plotBands=list(
                list(
                  from=df_region_summary$prop_resp_lb[1],
                  to=df_region_summary$prop_resp_ub[1],
                  color='#d822221F',
                  zIndex=98,
                  label=list(
                    text= 'England',
                    verticalAlign='top',
                    textAlign='center',
                    rotation=0,
                    y=-4,
                    style=list(
                      color='#d82222',
                      fontWeight='normal',
                      fontSize='1.35vh'
                    )
                  )
                )
              )
            )
          )
      }
      else if ('mean'%in%comp_ops & 'error'%in%comp_ops) {
        proxy <- proxy %>%
          hcpxy_update(
            yAxis=list(
              plotLines=list(
                list(
                  value=df_region_summary$prop_resp[1],
                  color='#d82222',
                  zIndex=99,
                  label=list(
                    text= 'England',
                    verticalAlign='top',
                    textAlign='center',
                    rotation=0,
                    y=-4,
                    style=list(
                      color='#d82222',
                      fontWeight='normal',
                      fontSize='1.35vh'
                    )
                  )
                )
              ),
              plotBands=list(
                list(
                  from=df_region_summary$prop_resp_lb[1],
                  to=df_region_summary$prop_resp_ub[1],
                  color='#d822221F',
                  zIndex=98,
                  label=list(
                    text= ''
                  )
                )
              )
            )
          )
      }
      else {
        proxy <- proxy %>%
          hcpxy_update(
            yAxis=list(
              plotLines=list(
                list(
                  value=df_region_summary$prop_resp[1],
                  color='#d8222200',
                  zIndex=99,
                  label=list(
                    text= ''
                  )
                )
              ),
              plotBands=list(
                list(
                  from=df_region_summary$prop_resp_lb[1],
                  to=df_region_summary$prop_resp_ub[1],
                  color='#d8222200',
                  zIndex=98,
                  label=list(
                    text= ''
                  )
                )
              )
            )
          )
      }
  }
  
  else {
    proxy <- highchartProxy(chart)
    if ('mean'%in%comp_ops & 'error'%ni%comp_ops) {
        proxy <- proxy %>%
          hcpxy_update(
            yAxis=list(
              plotLines=list(
                list(
                  value=mean(df_borough$prop_resp),
                  color='#d82222',
                  zIndex=99,
                  label=list(
                    text= 'London',
                    verticalAlign='top',
                    textAlign='center',
                    rotation=0,
                    y=-4,
                    style=list(
                      color='#d82222',
                      fontWeight='normal',
                      fontSize='1.35vh'
                    )
                  )
                )
              ),
              plotBands=list(
                list(
                  from=mean(df_borough$prop_resp_lb),
                  to=mean(df_borough$prop_resp_ub),
                  color='#d8222200',
                  zIndex=98,
                  label=list(
                    text= ''
                  )
                )
              )
            )
          )
      }
    else if ('mean'%ni%comp_ops & 'error'%in%comp_ops) {
      proxy <- proxy %>%
        hcpxy_update(
          yAxis=list(
            plotLines=list(
              list(
                value=mean(df_borough$prop_resp),
                color='#d8222200',
                zIndex=99,
                label=list(
                  text= ''
                )
              )
            ),
            plotBands=list(
              list(
                from=mean(df_borough$prop_resp_lb),
                to=mean(df_borough$prop_resp_ub),
                color='#d822221F',
                zIndex=98,
                label=list(
                  text= 'London',
                  verticalAlign='top',
                  textAlign='center',
                  rotation=0,
                  y=-4,
                  style=list(
                    color='#d82222',
                    fontWeight='normal',
                    fontSize='1.35vh'
                  )
                )
              )
            )
          )
        )
    }
    else if ('mean'%in%comp_ops & 'error'%in%comp_ops) {
      proxy <- proxy %>%
        hcpxy_update(
          yAxis=list(
            plotLines=list(
              list(
                value=mean(df_borough$prop_resp),
                color='#d82222',
                zIndex=99,
                label=list(
                  text= 'London',
                  verticalAlign='top',
                  textAlign='center',
                  rotation=0,
                  y=-4,
                  style=list(
                    color='#d82222',
                    fontWeight='normal',
                    fontSize='1.35vh'
                  )
                )
              )
            ),
            plotBands=list(
              list(
                from=mean(df_borough$prop_resp_lb),
                to=mean(df_borough$prop_resp_ub),
                color='#d822221F',
                zIndex=98,
                label=list(
                  text= ''
                )
              )
            )
          )
        )
    }
    else {
      proxy <- proxy %>%
        hcpxy_update(
          yAxis=list(
            plotLines=list(
              list(
                value=mean(df_borough$prop_resp),
                color='#d8222200',
                zIndex=99,
                label=list(
                  text= ''
                )
              )
            ),
            plotBands=list(
              list(
                from=mean(df_borough$prop_resp_lb),
                to=mean(df_borough$prop_resp_ub),
                color='#d8222200',
                zIndex=98,
                label=list(
                  text= ''
                )
              )
            )
          )
        )
    }
  
  
  }
  
  #browser()
  return(proxy)

}
      
      


# update_drilldown_chart_errorbar <- function(question, df_list, chart) {
#   
#   question <- as.numeric(question)
#   df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
#     select(region, prop_resp, num_resp, color, drilldown_central)
#   df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
#     select(region, prop_resp_lb, prop_resp_ub, num_resp, drilldown_error)
#   subtitle <-  df_list[[question ]][['region']][['title']]
#   df_borough <- df_list[[question ]][['borough']][['dataframe']] 
#   #browser()
#   highchartProxy(chart) %>%
#   hcpxy_set_data(
#     type='errorbar',
#     data=data.frame(
#       borough=df_borough$borough,
#       prop_resp_lb=round(df_borough$prop_resp_lb,1),
#       prop_resp_ub=round(df_borough$prop_resp_ub,1)
#     ),
#     mapping=hcaes(
#       x=borough, low=prop_resp_lb, high=prop_resp_ub
#     ),
#     redraw=F
#    ) %>%
#     hcpxy_set_data(
#       type='bar',
#       data=data.frame(
#         borough=df_borough$borough,
#         prop_resp=round(df_borough$prop_resp,1),
#         num_resp=df_borough$num_resp
#       )
#       ,
#       mapping=hcaes(
#         x=borough, y=prop_resp
#       )
#       ,
#       redraw=F
#     ) %>%
#     hcpxy_redraw()
#   
# }
# 


generate_drilldown_map <- function(question, df_list, theme, question_list, bounds_region, bounds_borough) {
  
  # question <- 11
  # theme <- 'Sport'
  # question_list <- QUESTION_LIST
  #browser()
  question <- as.numeric(question)
  subtitle <-  df_list[[question ]][['region']][['title']]
  if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
    df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
      select(region, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp, color) %>%
      mutate(
        value = round(prop_resp,1), drilldown=case_when(region=='London'~'london-drilldown',T~''),
        prop_resp_lb = round(prop_resp_lb,1), prop_resp_ub=round(prop_resp_ub,1)
      )
   
    df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
      select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp) %>%
      mutate(
        value = round(prop_resp,1), prop_resp_lb = round(prop_resp_lb,1), prop_resp_ub=round(prop_resp_ub,1)
      )
  }
  else {
    
    df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
      select(region, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp, color) %>%
      mutate(
        value = round(prop_resp,1), prop_resp_lb = round(prop_resp_lb,1), prop_resp_ub=round(prop_resp_ub,1)
      )
  }
  
  
  # df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
  #   select(region, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp, color) %>%
  #   mutate(
  #     value = prop_resp,
  #     drilldown=case_when(region=='London'~'london-drilldown',T~'')
  #   )
  # subtitle <-  df_list[[question ]][['region']][['title']]
  # #browser()
  # if (question_list$borough$code[question]!="") {
  #   df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
  #     select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp) %>%
  #     mutate(value = prop_resp)
  # }
  #max_borough <- max(df_borough$prop_resp_ub)
  
  
  #pal <- gla_pal(gla_theme = "default", palette_type = "quantitative", palette_name='core', n = 20, main_colours=question_list[['region']][['color']][question])
  
  #browser()
  # mid_col <- unique(df_region$color[df_region$color!='#cccccc'])
  # min_col <- tinter::tinter(mid_col, direction='tints', steps=5)[1]
  # max_col <- tinter::tinter(mid_col, direction='shades', steps=5)[5]
  # pal <- colorRampPalette(c(min_col, mid_col, max_col))
  #pal_vec <- pal(11)
  max_col <- unique(df_region$color[df_region$color!='#cccccc'])
  min_col <- tinter::tinter(max_col, direction='tints', steps=9)[1]
  mid_col <- tinter::tinter(max_col, direction='tints', steps=9)[5]
  pal <- colorRampPalette(c(min_col, mid_col, max_col))
  pal_vec <- pal(11)
  
  # image(seq(1, 1000), 1, matrix(seq(1,1000), 1000,1), 
  #       col = greekIslandsColours(1000),
  #       axes = FALSE, ann = FALSE)
  

  
  #min <- min(df_region$prop_resp)
  #max <- max(df_region$prop_resp)
  
  #browser()
  
  df_region_rest <- df_region %>% filter(region != 'London') %>% select(-color)
  df_region_ldn <- df_region %>% filter(region == 'London') %>% select(-color)
  bounds_region_ldn <- list('type'=bounds_region$type, 'features'=list(bounds_region$features[[7]]))
  
  # Rename to East of England (from Eastern)
  bounds_region$features[[6]]$properties$EER13NM <- 'East of England'

  map <- highchart(type='map') %>%
    hc_add_dependency("modules/drilldown.js") %>%
    hc_add_dependency("modules/highmaps.js") %>%
    # hc_add_series_map(bounds_pfa, df_pfa_nonselect, name="Number of stop-searches", value = "numberOfSearches", joinBy=c("pfa16nm", "pfaName"),
    #                   borderColor='#FAFAFA', borderWidth=.1, dataLabels = list(enabled = TRUE, format = "{point.pfaName}",
    #                                                                            style=list(fontSize='10'))) %>%
    # hc_add_series_map(bounds_pfa_select, df_pfa_select, name="Number of stop-searches", value = "numberOfSearches", joinBy=c("pfa16nm", "pfaName"),
    #                   borderColor='#323232', borderWidth=2, dataLabels = list(enabled = F, format = "{point.pfaName}",
    #                                                                           style=list(fontSize='9'))) %>%
    # 
    # 
    #hc_chart(spacingBottom= 50) %>%
    hc_add_series(id='regions', mapData=bounds_region, data=list_parse(df_region_rest), joinBy=c("EER13NM", "region"), name="Regions", borderColor='#FAFAFA', borderWidth=0.1,
                  point=list(      
                  events = list(
                      mouseOver = JS(
                        paste0("function() { 
                         Shiny.onInputChange('",theme,"_map_mouseOver', this.EER13NM);
                         
                         
                         }")
                     ))
                    )) %>%
    # hc_plotOptions(
    #   series = list(
    #     point=list(
    #       events = list(
    #         mouseOver = JS(
    #           paste0("function() { 
    #                      Shiny.onInputChange('arts_map_mouseOver', this.name);
    #                      
    #                      
    #                      }")
    #         )
    #       )
    hc_add_series(id='regions_ldn', mapData=bounds_region_ldn, data=list_parse(df_region_ldn), joinBy=c("EER13NM", "region"), name="London", borderColor='black', borderWidth=1.8,
                  point=list(      
                    events = list(
                      mouseOver = JS(
                        paste0("function() { 
                         Shiny.onInputChange('",theme,"_map_mouseOver', this.EER13NM);
                         
                         
                         }")
                      ))
                  )) %>% 
    
    hc_colorAxis(
      stops = list(
        list(0.1, pal_vec[1]),
        list(0.5, pal_vec[6]),
        list(0.9, pal_vec[11])
      )
    ) %>%
    #   minColor = pal[20], #pal[20],#'#eff4f9'
    #   maxColor = pal[1] #pal[1] # '#252C35'
    # ) %>%
    # hc_title(
    #   text=subtitle,
    #   align='left',
    #   margin=60,
    #   style = list(
    #     fontSize ="3.8vh",color = '#353d42', 
    #     fontFamily = "Arial", fontWeight = "450"
    #   )
    # ) %>%
    # hc_subtitle(
    #   text="Click to drilldown into London by Borough",
    #   align='left',
    #   style = list(
    #     fontSize ="2.4vh",color = '#353d42', 
    #     fontFamily = "Arial", fontWeight = "250",
    #     fontStyle='italic'
    #   )
    # ) %>%
    hc_credits(
      enabled=T,
      useHTML=T,
      #Chart: <a href="https://data.london.gov.uk/social-evidence-base/" target="_blank" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence (Social Policy) </a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>',
      text='Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" target="_blank" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24, DCMS</a>',
      href="",
      position=list(
        align='left',
        x=10,
        y=-10 
      ),
      style =list(
        fontSize='1.7vh',
        color='#9b9b9b'
      )
    ) %>%
      hc_legend(
        valueDecimals = 0,
        format = "{value}%"
      ) %>%
      hc_tooltip(
        #valueSuffix= '%',
        borderWidth=2.6,
        shared=T,
        style=list(fontSize='1.35vh'),
        shape='callout',
        useHTML = TRUE,
        headerFormat = "<span style='font-size:1.6vh;'><span style='color:{point.color}'>\u25CF</span> {point.key} (n={point.point.num_resp})</span>", #<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.EER13NM}</span><br>
        pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'> {point.EER13NM}</span><br>Central estimate: <b>{point.value}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
      ) %>%
      hc_mapView(
        projection=list(
          name='WebMercator'
        )
      ) %>%
    hc_exporting(
      enabled=T,
      showExportInProgress=T,
      buttons=list(
        contextButton=list(
          menuItems=c(
            'viewFullscreen', 'separator',  'downloadPNG', 'downloadJPEG', 'downloadSVG'
          ),
          symbol="url(data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA1MTIgNTEyIj48IS0tIUZvbnQgQXdlc29tZSBGcmVlIDYuNy4yIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlL2ZyZWUgQ29weXJpZ2h0IDIwMjUgRm9udGljb25zLCBJbmMuLS0+PHBhdGggZmlsbD0iIzliOWI5YiIgZD0iTTI4OCAzMmMwLTE3LjctMTQuMy0zMi0zMi0zMnMtMzIgMTQuMy0zMiAzMmwwIDI0Mi43LTczLjQtNzMuNGMtMTIuNS0xMi41LTMyLjgtMTIuNS00NS4zIDBzLTEyLjUgMzIuOCAwIDQ1LjNsMTI4IDEyOGMxMi41IDEyLjUgMzIuOCAxMi41IDQ1LjMgMGwxMjgtMTI4YzEyLjUtMTIuNSAxMi41LTMyLjggMC00NS4zcy0zMi44LTEyLjUtNDUuMyAwTDI4OCAyNzQuNyAyODggMzJ6TTY0IDM1MmMtMzUuMyAwLTY0IDI4LjctNjQgNjRsMCAzMmMwIDM1LjMgMjguNyA2NCA2NCA2NGwzODQgMGMzNS4zIDAgNjQtMjguNyA2NC02NGwwLTMyYzAtMzUuMy0yOC43LTY0LTY0LTY0bC0xMDEuNSAwLTQ1LjMgNDUuM2MtMjUgMjUtNjUuNSAyNS05MC41IDBMMTY1LjUgMzUyIDY0IDM1MnptMzY4IDU2YTI0IDI0IDAgMSAxIDAgNDggMjQgMjQgMCAxIDEgMC00OHoiLz48L3N2Zz4=)",
          height=40,
          width=48,
          symbolSize=36,
          symbolX=42,
          symbolY=45,
          symbolStrokeWidth=2
        )
      ),
      # sourceWidth=1600,
      # sourceHeight= 1400,
      sourceWidth=900,
      sourceHeight= 700,
      scale=3,
      # width=1000,
      # height=800,
      chartOptions = list(
        title=list(
          text=subtitle,
          align='left',
          margin=35,
          style = list(
            #fontSize ="3.8vh",
            fontSize =25 ,color = '#353d42',
            fontFamily = "Arial", fontWeight = "450"
          )
        )
      )
    ) %>%
    hc_chart(
      spacingBottom= 35,
      backgroundColor='#ffffff'
        ,
      events=list(
        drilldown=JS(
          paste0(
            "
            function() {
            Shiny.onInputChange('",theme,"_currLevelMap', 0);
            }
            " 
          )
        )
        ,
        drillup=JS(
          paste0(
            "
            function() {
            Shiny.onInputChange('",theme,"_currLevelMap', 1);
            }
            
            "
          )
        )#,
      #   click=JS(
      #     paste0(
      #       "
      #         function(e) {
      #           Shiny.onInputChange('",theme,"_map_click', e.point.name);
      #         }
      #         "
      #     )
      #   )
      #   
      )
    )
        #e.properties.EER13NM
        #e.point.EER13NM
        
        #- just need fucking right specification
      #)
    #) # WORKING
  #https://shiny.posit.co/r/articles/build/communicating-with-js/
    #hc_add_event_series(event = "mouseOver")
    #https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/maps/plotoptions/series-events-click/
  #https://stackoverflow.com/questions/52066731/highcharter-change-highlight-color-on-hover/52075310#52075310
    #hc_add_event_point(event = "mouseOver", series='series')

  
  if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
    
     map <- map %>%
       #,
       #   zoom=5.7#,
       #   #center=rev(c(51.51279, -0.09184))
       #  ) %>%
       hc_drilldown(
         breadcrumbs=list(
           showFullPath=F,
           useHTML=T,
           format= '&#x25c0; Back to Regions',
           style=list(
             fontSize='2.2vh'
           ),
           position=list(
             align='right',
             y=-47,
             x=-20
           )
         ),
         activeAxisLabelStyle = list(
           color='black'
         ),
         series=list(
           list(
             id='london-drilldown',
             mapData=bounds_borough$features,
             data=list_parse(df_borough),
             joinBy=c("name", "borough"),
             name="Boroughs",
             borderColor='#FAFAFA',
             borderWidth=0.1
           )
         ),
         # mapZooming=list(
         #   enabled=T
         # )
         mapZooming=list(enabled=T)
         
         
         # activeDataLabelStyle = list(
         #   color =  'red',
         #   textDecoration = 'none'
         # )
       )
    
  }
  #browser()
  #print ('yo')
  return(map)
}


update_drilldown_map <- function(question, df_list, map, question_list=NULL, bounds_region=NULL, bounds_borough=NULL) {
  

  question <- as.numeric(question)
  subtitle <-  df_list[[question ]][['region']][['title']]
  if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
    df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
      select(region, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp, color) %>%
      mutate(
        value = round(prop_resp,1), drilldown=case_when(region=='London'~'london-drilldown',T~''),
        prop_resp_lb = round(prop_resp_lb,1), prop_resp_ub=round(prop_resp_ub,1)
      )
    
    df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
      select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp) %>%
      mutate(
        value = round(prop_resp,1), prop_resp_lb = round(prop_resp_lb,1), prop_resp_ub=round(prop_resp_ub,1)
      )
  }
  else {
    df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
      select(region, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp, color) %>%
      mutate(
        value = round(prop_resp,1), prop_resp_lb = round(prop_resp_lb,1), prop_resp_ub=round(prop_resp_ub,1)
      )
  }
  df_region_rest <- df_region %>% filter(region != 'London') %>% select(-color)
  df_region_ldn <- df_region %>% filter(region == 'London') %>% select(-color)
  max_col <- unique(df_region$color[df_region$color!='#cccccc'])
  min_col <- tinter::tinter(max_col, direction='tints', steps=9)[1]
  mid_col <- tinter::tinter(max_col, direction='tints', steps=9)[5]
  pal <- colorRampPalette(c(min_col, mid_col, max_col))
  pal_vec <- pal(11)
  
#browser()
  proxy <- highchartProxy(map) %>%
    hcpxy_update_series(
      id='regions',
      data=list_parse(df_region_rest)
    ) %>%
    hcpxy_update_series(
      id='regions_ldn',
      data=list_parse(df_region_ldn)#,
    ) %>%
    hcpxy_update(
      colorAxis=list(
        stops = list(
          list(0.1, pal_vec[1]),
          list(0.5, pal_vec[6]),
          list(0.9, pal_vec[11])
        )
      )
    )
  if (rlang::has_name( df_list[[question]][['borough']], 'dataframe')==T) {
    proxy <- proxy %>%
    hcpxy_update(
      drilldown=list(
        series=list(
          list(
            id='london-drilldown',
            mapData=bounds_borough$features,
            data=list_parse(df_borough),
            joinBy=c("name", "borough"),
            borderColor='#FAFAFA',
            borderWidth=0.1
          )
        )
      )
    )
  }
  
  return(proxy)
}

generate_countUp <- function(count_to, count_from, theme) {
  
  #print(count_to)
  #print(as.numeric(count_to))
  count_to <- as.numeric(count_to)
  #ops <- list()
  # browser()
  countup(
    count = count_to,
    start_at = count_from,
    duration = 1.5,
    suffix='%',
    decimalPlaces=1,
    decimal='.',
    start = TRUE,
    width = NULL,
    height = NULL,
    elementId = paste0('countUp-',theme)
  )
  
}




# generate_region_text <- function(question, df_list, drill_level=NULL, reactive__region_name=NULL,  reactive__region_val=NULL, reactive__region_dif=NULL, reactive__region_rank=NULL) {
#   
# 
# 
#   question <- as.numeric(question)
#   df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
#     select(region, prop_resp, color, drilldown_central)
#   question_text <- paste(
#     tolower(substr(df_list[[question ]][['region']][['title']], 1, 1)), 
#     substr(df_list[[question ]][['region']][['title']], 2, nchar(df_list[[question ]][['region']][['title']])), sep="")
#   
#   #browser()
#   eng_mean <- round(mean(df_region$prop_resp),1)
#   eng_ldn_dif_text <- 
#     ifelse(reactive__region_dif>0, paste0(abs(reactive__region_dif), ' points more than'),
#       ifelse(reactive__region_dif<0, paste0(abs(reactive__region_dif), ' points less than'),
#         'the same as'
#       )
#     )
# 
#   
#   headline <- paste0(
#     #HTML(paste0('<span style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;">',df_region$prop_resp[df_region$region=='London'],'%</span><br><span style="color:#ffffff; font-size:2.8vw;  line-height:2.8vw;">of Londoners</span><br>')),
#     HTML(
#       '
#       <div class="section-text-title-l1">
#         <p>of Londoners</p>
#       </div>
#       '
#     ),
#     HTML(
#       paste(
#         '
#         <div class="section-text-title-l2">
#           <p>',strwrap(gsub(' \\(%)','', paste0(question_text,'</p><br>')), width=100),'</p>
#         </div>
#         ',
#         collapse = "<br>"
#         
#       )
#     ),
#     
#     
#     
#     #HTML(paste('<span style="color: #ffffff;font-size:1.4vw;line-height:1.4vw;">',strwrap(gsub(' \\(%)','', paste0(question_text,'</span><br>')), width=100),collapse = "<br>")),
#     HTML('<hr width="90%" color="#ffffff" />')
#   )
# 
#   list <- HTML(
#     paste0(
#       "
#       <div style='height:-1vh'></div>
#       <span style='color:#ffffff;font-size:1.15vw; line-height:1.25vw;'>
#       <ul>
#       <li>The regional average in ",reactive__region_name," is ",reactive__region_val,"%, which is ",eng_ldn_dif_text," the England average of ",eng_mean,"%</li>
#       <li>This ranks ",reactive__region_name," ",reactive__region_rank," out of England's 9 regions</li>
#       </ul>
#       </span>
#       "
#     )
#   )
#   if(is.null(reactive__region_name)) {
#     return(HTML(headline))
#   }
#   # Else show loader
#   else {
#     return(HTML(paste0(headline,list)))
#   }
# 
# }
# 

generate_region_headline_text <- function(question, df_list) {
  
  question <- as.numeric(question)
  question_text <- paste(
    tolower(substr(df_list[[question ]][['region']][['title']], 1, 1)), 
    substr(df_list[[question ]][['region']][['title']], 2, nchar(df_list[[question ]][['region']][['title']])), sep=""
  )
  #browser()
  headline <- paste0(
    HTML(
      '
      <div class="section-text-title-l1">
        <p>of Londoners</p>
      </div>
      '
    ),
    HTML(
      paste(
        '
        <div class="section-text-title-l2">
          <p>',strwrap(gsub(' \\(%)','', paste0(question_text,'</p><br>')), width=100),'</p>
        </div>
        ',
        collapse = "<br>"
        
      )
    ),
    # HTML('<span style="color:#ffffff; font-size:2.8vw;  line-height:2.8vw;">of Londoners</span><br>'),
    # HTML(paste('<span style="color: #ffffff;font-size:1.4vw;line-height:1.4vw;">',strwrap(gsub(' \\(%)','', paste0(question_text,'</span><br>')), width=100),collapse = "<br>")),
    
    HTML(
      '
      <hr class="section-text-title-seperator" />
      '
    )
  )
  return(HTML(headline))
  
}


generate_region_summary_text <- function(question, df_list, drill_level=NULL, reactive__region_name=NULL,  reactive__region_val=NULL, reactive__england_val=NULL, reactive__region_dif=NULL, reactive__region_rank=NULL) {
  #browser()
  question <- as.numeric(question)
  # df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
  #   select(region, prop_resp, color, drilldown_central)
  #eng_mean <- round(mean(df_region$prop_resp),1)
  #browser()
  eng_ldn_dif_text <- 
    ifelse(reactive__region_dif>0, paste0(abs(reactive__region_dif), ' points more than'),
      ifelse(reactive__region_dif<0, paste0(abs(reactive__region_dif), ' points less than'),'the same as'
      )
    )
  list <- HTML(
    paste0(
      "
      <span class='section-text-summary'>
      <ul>
      <li>The regional average in ",reactive__region_name," is ",reactive__region_val,"%, which is ",eng_ldn_dif_text," the England average of ",reactive__england_val,"%</li>
      <li>This ranks ",reactive__region_name," ",reactive__region_rank," out of England's 9 regions</li>
      </ul>
      </span>
      "
    )
  )
  if(!is.null(reactive__region_name)) {
    return(HTML(list))
  }
  else {
    return('')
  }
  
}

# TODO move 1dp ranking outside of processing, and so add add 1dpp only in chart output and ranking output itself



generate_borough_text <- function(question, df_list) {
  
  question <- as.numeric(question)
  df_borough <- df_list[[question ]][['borough']][['dataframe']] 
  
  # if (question==6) {
  #   browser()
  # }


  #browser()
  high1_name <- as.character(df_borough$borough[which(df_borough$prop_resp==max(df_borough$prop_resp))])
  high2_name <- as.character(df_borough$borough[which(df_borough$prop_resp==max(df_borough$prop_resp[df_borough$prop_resp!=max(df_borough$prop_resp)]))])
  low1_name <- as.character(df_borough$borough[which(df_borough$prop_resp==min(df_borough$prop_resp))])
  low2_name <- as.character(df_borough$borough[which(df_borough$prop_resp==min(df_borough$prop_resp[df_borough$prop_resp!=min(df_borough$prop_resp)]))])
  high1_val <- round(max(df_borough$prop_resp),1)
  high2_val <- round(max(df_borough$prop_resp[df_borough$prop_resp!=max(df_borough$prop_resp)]),1)
  low1_val <- round(min(df_borough$prop_resp),1)
  low2_val <- round(min(df_borough$prop_resp[df_borough$prop_resp!=min(df_borough$prop_resp)]),1)
  
  list <- HTML(
    paste0(
      "
      <span class='section-text-summary'>
      <ul>
      <li>The two boroughs that score the highest are ", high1_name, " and ", high2_name ,", at ", high1_val,"% and ",high2_val,"% respectively</li>
      <li>The two boroughs that score the lowest are ", low1_name, " and ", low2_name ,", at ", low1_val,"% and ",low2_val,"% respectively</li>
      </ul>
      </span>
      "
    )
  )      
  # if (question==1) {
  #   browser()
  # }
  # if (as.numeric(question)==1) {
  #   browser()
  # }
  print('yo')
  return(list)
  

}

# Bullet x% of Londoners question
# This is x points higher/lower than the national average of x %
# X and Z are the Boroughs that scores the highest, with x% and z% respectively
# I and J are the Boroughs that score the lowest, with i% and j% respectively



# gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1), main_colours='green')
# '#d82222'
# '#5ea15d'

