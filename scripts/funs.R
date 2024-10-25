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
  subtitle <- paste0(sub(".*: ", "", gsub(r"{\s*\[[^\)]+\]}","", paste0(question[['theme']],': ',gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])))),' (%)')
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
    mutate(across(contains('resp'),~ round(as.numeric(as.character(.x,1))))) %>%
    mutate(region = fct_reorder(region, -prop_resp)) %>%
    arrange(region) %>%
    mutate(code = question[['code']])
  
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
    mutate(code = question[['code']]) #%>% 
    #mutate(drilldown=case_when(region=='London'~'london',T~''))
  
  return(list('dataframe'=df_borough))
  
  
  
}



generate_drilldown_plot <- function(question, df_list) {
  question <- as.numeric(question)
  df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp, color, drilldown_central)
  df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp_lb, prop_resp_ub, drilldown_error)
  subtitle <-  df_list[[question ]][['region']][['title']]
  df_borough <- df_list[[question ]][['borough']][['dataframe']] 
  max_borough <- max(df_borough$prop_resp_ub)
  
  plot <- highchart() %>%
    hc_chart(
      spacingBottom= 50,
      backgroundColor='white'
      
    ) %>%
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
          align='right'
        )
      ),
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
              prop_resp=df_borough$prop_resp
            )
          )
        ),
        list(
          id='london-error',
          mame='Lower-Upper estimate',
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
      max=max_borough,
      labels=list(
        format="{value}%",
        style=list(
          fontSize='2vh',
          color='#9b9b9b',
          fontFamily = "Arial",
          fontWeight='300'
        )
      )
    ) %>%
    hc_plotOptions(  #48-45
      bar = list(
        pointWidth=32
      ),
      errorbar=list(
        stemWidth= 1,
        whiskerWidth=1,
        whiskerLength= 30
      )
    ) %>%
    hc_title(
      text=subtitle,
      align='left',
      style = list(
        fontSize ="3.8vh",color = '#353d42', 
        fontFamily = "Arial", fontWeight = "450"
      )
    ) %>%
    hc_subtitle(
      text="Click to drilldown into London by Borough",
      align='left',
      style = list(
        fontSize ="2.4vh",color = '#353d42', 
        fontFamily = "Arial", fontWeight = "250",
        fontStyle='italic'
      )
    ) %>%
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
      useHTML = TRUE
      #headerFormat = ""#,
      #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
    ) %>%
    hc_exporting(
      enabled=T
    )
  return(plot)
}


update_drilldown_plot <- function(question, df_list, plot) {
  
  question <- as.numeric(question)
  df_region_central <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp, color, drilldown_central)
  df_region_error <- df_list[[question ]][['region']][['dataframe']] %>% 
    select(region, prop_resp_lb, prop_resp_ub, drilldown_error)
  subtitle <-  df_list[[question ]][['region']][['title']]
  df_borough <- df_list[[question ]][['borough']][['dataframe']] 
  max_borough <- max(df_borough$prop_resp_ub)
  
  highchartProxy(plot) %>%
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
  
  return(plot)
}