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

generate_region_frame <- function(survey_path, release_year, question) {
  
  if (file.exists(survey_path)) {
    df <- read_ods(survey_path, sheet=paste0('Table_', question[['code']]), skip=3)
  }
  else {
    stop(
      'Aborting procedure - survey data does not exist in user specified path'
    )
  }
  #title <- paste0('<span style="color:',pal[1],'">', question[['theme']],'</span>')
  subtitle <- paste0(sub(".*: ", "", gsub(r"{\s*\[[^\)]+\]}","", paste0(question[['theme']],': ',gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])))))
  # Define colour palette
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1), main_colours=question[['color']])
  df_region <- df %>% 
    select(
      level=Question,
      region=`Response Breakdown`,
      prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
      prop_resp_lb=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Lower estimate'),
      prop_resp_ub=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Upper estimate'),
      num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
    ) %>%
    filter(grepl('ITL1', level)) %>%
    mutate(color = case_when(region=='London'~pal[1], T~pal[2])) %>%
    mutate(across(contains('resp'),~ round(as.numeric(as.character(.x)),1))) %>%
    mutate(region = fct_reorder(region, -prop_resp)) %>%
    arrange(region) %>%
    mutate(code = question[['code']]) %>%
    mutate(num_resp = format(round(as.numeric(num_resp), 0), nsmall=0, big.mark=","))
  
  # create drilldown_central and drilldown_error as london-central and london-error, or "" 
  # entirely where Question_lisrt drilldown is 0
  
  
  return(list('dataframe'=df_region, 'title'=subtitle))
  
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
    mutate(across(contains('resp'),~ round(.x,1))) %>%
    mutate(borough = fct_reorder(borough, -prop_resp)) %>%
    arrange(borough) %>%
    mutate(code = question[['code']]) %>% 
    mutate(num_resp = format(round(as.numeric(num_resp), 0), nsmall=0, big.mark=","))
    #mutate(drilldown=case_when(region=='London'~'london',T~''))
  
  return(list('dataframe'=df_borough))
  
  
  
}



generate_drilldown_chart <- function(question, df_list, theme, browser_height) {
  
  # #
  # question <- SPORT_QUESTIONS[2]
  # theme <- 'Sport'
  # browser_height <- 950
  
  
  question <- as.numeric(question)
  df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp, num_resp, color, drilldown_central)
  df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp_lb, prop_resp_ub, num_resp, drilldown_error)
  subtitle <-  df_list[[question ]][['region']][['title']]
  df_borough <- df_list[[question ]][['borough']][['dataframe']] 
  
  
  #max_borough <- max(df_borough$prop_resp_ub)
  #browser()
  chart <- highchart() %>%
    hc_add_series(
      name='Central estimate',
      id='region-central',
      type='bar', 
      showInLegend=F,
      data=df_region_central, 
      hcaes(x = factor(region), y = round(prop_resp,1), 
            color=color, drilldown=drilldown_central)
    ) %>%
    hc_add_series(
      name='Lower-Upper estimate',
      id='region-error',
      type='errorbar',
      showInLegend=F,
      data=df_region_error,
      hcaes(x=factor(region), low=prop_resp_lb, 
            high=prop_resp_ub, drilldown=drilldown_error)
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
        color='black'
      ),
      # TODO not working!!!
      # tooltip = list(
      #   headerFormat = "<span style='font-size:1.6vh;'> {point.key} (n={point.point.options.num_resp})</span><br>"
      #   
      # ),
      #https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/breadcrumbs/format
      series = list(
        list(
          id='london-central',
          name='Central estimate',
          type='bar',
          showInLegend=F,
          data = list_parse2(
            data.frame(
              borough=df_borough$borough,
              prop_resp=df_borough$prop_resp,
              num_resp=df_borough$num_resp
            )
          )
        ),
        list(
          id='london-error',
          name='Lower-Upper estimate',
          type='errorbar',
          showInLegend=F,
          data=list_parse2(
            data.frame(
              borough=df_borough$borough,
              prop_resp_lb=df_borough$prop_resp_lb,
              prop_resp_ub=df_borough$prop_resp_ub
            )

          )
        )
      )
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
        showFull=F
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
          value=-10,
          color='#d82222',
          zIndex=99
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
      bar = list(
        pointWidth=browser_height/19
      ),
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
      text='Chart: <a href="https://data.london.gov.uk/social-evidence-base/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence (Social Policy) </a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
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
      headerFormat = "<span style='font-size:1.6vh;'> {point.key} (n={point.point.options.num_resp})</span><br>"
      #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
    ) %>%
    hc_exporting(
      enabled=T
    ) %>%
    hc_legend(
      enabled=F
    ) %>%
    hc_chart(
      spacingBottom= browser_height/12, # needs to be adjusted
      spacingTop=20,
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
    )
  #chart
  
  # TODO Add tooltip to plotline and plotband https://jsfiddle.net/BlackLabel/nx0uo1rk/
  # or dummy series using https://jsfiddle.net/BlackLabel/2Ln05yes/
  # Add the line(s) -or try
  # If not add England bar, optional add or remove ci lines maybe
  # then revisit fucking tooltip n on drilldown
  # bulleted text - bvasically done
  
  return(chart)
}


update_drilldown_chart <- function(question, df_list, chart) {
  
  question <- as.numeric(question)
  df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp, color, drilldown_central)
  df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp_lb, prop_resp_ub, drilldown_error)
  subtitle <-  df_list[[question ]][['region']][['title']]
  df_borough <- df_list[[question ]][['borough']][['dataframe']] 
  #max_borough <- max(df_borough$prop_resp_ub)
  
  highchartProxy(chart) %>%
    hcpxy_update_series(
      id='region-central',
      data = df_region_central$prop_resp
    ) %>%
    hcpxy_update_series(
      id='region-error',
      data=list_parse2(
        data.frame(
          prop_resp_lb=df_region_error$prop_resp_lb,
          prop_resp_ub=df_region_error$prop_resp_ub
        )
      )
    ) %>%
    hcpxy_update(
      drilldown=list(
        series=list(
          list(
            id='london-central',
            type='column',
            data=list_parse2(
              data.frame(
                borough=df_borough$borough,
                prop_resp=df_borough$prop_resp
              )
            )
          ),
          list(
            id='london-error',
            type='errorbar',
            data=list_parse2(
              data.frame(
                borough=df_borough$borough,
                prop_resp_lb=df_borough$prop_resp_lb,
                prop_resp_ub=df_borough$prop_resp_ub
                
              )
            )
            
          )
        )
      )
    )
}



generate_drilldown_map <- function(question, df_list, theme, question_list, bounds_region, bounds_borough) {
  
  # question <- 11
  # theme <- 'Sport'
  # question_list <- QUESTION_LIST
  #browser()
  question <- as.numeric(question)
  df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp, color) %>%
    mutate(
      value = prop_resp,
      drilldown=case_when(region=='London'~'london-drilldown',T~'')
    )
  subtitle <-  df_list[[question ]][['region']][['title']]
  #browser()
  if (question_list$borough$code[question]!="") {
    df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
      select(borough, prop_resp,  prop_resp_lb, prop_resp_ub, num_resp) %>%
      mutate(value = prop_resp)
  }
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
    hc_add_series(id='regions', mapData=bounds_region, data=list_parse(df_region_rest), joinBy=c("EER13NM", "region"), name="{point.EER13NM}", borderColor='#FAFAFA', borderWidth=0.1) %>%
    hc_add_series(id='regions_ldn', mapData=bounds_region_ldn, data=list_parse(df_region_ldn), joinBy=c("EER13NM", "region"), name="{point.EER13NM}", borderColor='black', borderWidth=1.8) %>% 
    
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
      text='Chart: <a href="https://data.london.gov.uk/social-evidence-base/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence (Social Policy) </a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
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
      enabled=T#,
      # navigation=list(
      #   buttonOptions=list(
      #     align='center'
      #   )
      # )
      # buttons=list(
      #   exportButton=list(
      #     align='right',
      #     x=-300
      #   )
      # )
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
        )
      )
    )
  
  if (question_list$borough$code[question]!="") {
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
             name="{point.borough}",
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


update_drilldown_map <- function(question, df_list, map) {
  
  question <- as.numeric(question)
  df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp,  prop_resp_lb, prop_resp_ub) %>%
    mutate(
      value = prop_resp,
      drilldown=case_when(region=='London'~'london-drilldown',T~'')
    )
  subtitle <-  df_list[[question ]][['region']][['title']]
  df_borough <- df_list[[question ]][['borough']][['dataframe']] %>%
    select(borough, prop_resp,  prop_resp_lb, prop_resp_ub) %>%
    mutate(value = prop_resp)
  #max_borough <- max(df_borough$prop_resp_ub)
  pal <- gla_pal(gla_theme = "default", palette_type = "quantitative", palette_name='core', n = 20, main_colours='blue')
  #min <- min(df_region$prop_resp)
  #max <- max(df_region$prop_resp)
  
  highchartProxy(map) %>%
    hcpxy_update_series(
      id='regions',
      data = df_region$value
    ) %>%
    hcpxy_update(
      drilldown=list(
        series=list(
          list(
            id='london-drilldown',
            data=list_parse2(
              data.frame(
                value=df_borough$value
              )
            )
          )
        )
      )
    )
}

generate_region_text <- function(question, df_list) {
  
  question <- as.numeric(question)
  df_region <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp, color, drilldown_central)
  question_text <- tolower(df_list[[question ]][['region']][['title']])
  
  lnd_mean <- round(df_region$prop_resp[df_region$region=='London'],1)
  eng_mean <- round(mean(df_region$prop_resp),1)
  eng_ldn_dif <- round(eng_mean - lnd_mean,1)
  eng_ldn_dif_text <- 
    ifelse(eng_ldn_dif>0, paste0(eng_ldn_dif, ' points more than'),
      ifelse(eng_ldn_dif<0, paste0(eng_ldn_dif, ' points less than'),
        'the same as'
      )
    )
  df_region$rank <- scales::ordinal(rank(df_region$prop_resp, ties.method='min'))
  ldn_rank <- df_region$rank[df_region$region=='London']
  
  headline <- paste0(
    HTML(paste0('<span style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;">',df_region$prop_resp[df_region$region=='London'],'%</span><br><span style="color:#ffffff; font-size:2.8vw;  line-height:2.8vw;">of Londoners</span><br>')),
    HTML(paste('<span style="color: #ffffff;font-size:1.4vw;line-height:1.4vw;">',strwrap(gsub(' \\(%)','', paste0(question_text,'</span><br>')), width=85),collapse = "<br>")),
    HTML('<hr width="90%" color="#ffffff" />')
  )

  list <- HTML(
    paste0(
      "
      <div style='height:-1vh'></div>
      <span style='color:#ffffff;font-size:1.15vw; line-height:1.15vw;'>
      <ul>
      <li>The regional average in England is ",eng_mean,"%, which is ",eng_ldn_dif_text," the London average</li>
      <li>This ranks London ",ldn_rank," out of the 9 regions in England</li>
      </ul>
      </span>
      "
    )
  )
  return(HTML(paste0(headline,list)))
}


generate_borough_text <- function(question, df_list) {
  
  question <- as.numeric(question)
  df_borough <- df_list[[question ]][['borough']][['dataframe']] 
  
  high1_name <- as.character(df_borough$borough[which(df_borough$prop_resp==max(df_borough$prop_resp))])
  high2_name <- as.character(df_borough$borough[which(df_borough$prop_resp==max(df_borough$prop_resp[df_borough$prop_resp!=max(df_borough$prop_resp)]))])
  low1_name <- as.character(df_borough$borough[which(df_borough$prop_resp==min(df_borough$prop_resp))])
  low2_name <- as.character(df_borough$borough[which(df_borough$prop_resp==min(df_borough$prop_resp[df_borough$prop_resp!=min(df_borough$prop_resp)]))])
  high1_val <- max(df_borough$prop_resp)
  high2_val <- max(df_borough$prop_resp[df_borough$prop_resp!=max(df_borough$prop_resp)])
  low1_val <- min(df_borough$prop_resp)
  low2_val <- min(df_borough$prop_resp[df_borough$prop_resp!=min(df_borough$prop_resp)])
  
  list <- HTML(
    paste0(
      "
      <span style='color:#ffffff;font-size:1.15vw; line-height:1.15vw;'>
      <ul>
      <li>The two boroughs that score the highest are ", high1_name, " and ", high2_name ,", at ", high2_val,"% and ",high2_val,"% respectively</li>
      <li>The two boroughs that score the lowest are ", low1_name, " and ", low2_name ,", at ", low1_val,"% and ",low2_val,"% respectively</li>
      </ul>
      </span>
      "
    )
  )                                
  return(list)
  

}

# Bullet x% of Londoners question
# This is x points higher/lower than the national average of x %
# X and Z are the Boroughs that scores the highest, with x% and z% respectively
# I and J are the Boroughs that score the lowest, with i% and j% respectively



# gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1), main_colours='green')
# '#d82222'
# '#5ea15d'

