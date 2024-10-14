#'[Script]#'*process-2_2_2_6.R*
#'[Project]#'*economic_fairness_gla (https://github.com/mawtee/economic-fairness-gla)*
#'[Author]#'*M. Tibbles*
#'[Last Update]#'*02/10/2024*
#'[Description]#'*This blah *
#'[____________________________________________________________________________]


# Scrape and write apprenticeships data to file
#===============================================================================

scrape_and_write_survey_data <- function(release_year) {
  
  #' @description 
  #' Scrapes latest Apprenticeships data from DFE site and writes to file
  #' 
  #' @details  
  #' 
  #' @param url String URL to DFE Apprenticeships data page. Defined in global `UPDATE__URL`
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


generate_regional_plot <- function(sheet_letter, sheet_number) {
  
  sheet_letter <- 'A'
  sheet_number <- 2
  
  df <- read_ods(
    "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods", 
    sheet=paste0('Table_', sheet_letter, sheet_number), skip=3
  )
  title <- gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])
  subtitle <- "Percentage of respondents"
  if (nchar(gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))>1) {
    subtitle <- paste0(subtitle," who ", gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))
  }
  
  df_plot <- df %>% 
    select(
      level=Question,
      region=`Response Breakdown`,
      prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
      num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
    ) %>%
    filter(grepl('ITL1', level)) %>%
    mutate(region = fct_reorder(region, prop_resp)) %>%
    mutate(flag = case_when(region=='London'~1, T~0))

  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  plot <- ggplot(df_plot, mapping=aes(x=factor(region), y=prop_resp, fill=factor(flag))) +
    ggla_horizbar(width=1.8 * mm_to_pt) +
    coord_flip(ylim=c(50,100)) +
    scale_fill_manual(values=rev(pal)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = paste(strwrap(title, width = 70), collapse = "\n"),
      subtitle = paste(strwrap(subtitle, width = 100), collapse = "\n"),
      caption = "Chart: GLA City Intelligence. Source: Participaton Survey 2023/24, DCMS."
    ) +
    theme(
      legend.position='none',
      axis.text.y=element_text(margin = margin(r = 10*mm_to_pt))
    )
  return(plot)
}




generate_borough_plot <- function(sheet_letter, sheet_number) {
  
  sheet_letter <- 'A'
  sheet_number <- 3
  
  df <- read_ods(
    "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods", 
    sheet=paste0('Table_', sheet_letter, sheet_number), skip=4
  )
  title <- gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])
  subtitle <- "Percentage of respondents"
  if (nchar(gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))>1) {
    subtitle <- paste0(subtitle," who ", gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))
  }
  

  
  df_plot <- df %>% 
    select(
      region=`ITL1 name`,
      borough=`Response Breakdown`,
      prop_resp=paste0('Percentage of respondents ',gsub('_', '/', release_year)),
      num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
    ) %>%
    filter(grepl('London', region)) %>%
    mutate(borough = fct_reorder(borough, prop_resp)) 
  
  df_plot_bounds <- sf::st_read(
    "C:/Users/Matt/Downloads/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp"
  ) %>%
    right_join(df_plot, by=c('NAME'='borough'))
  
  
  pal <- gla_pal(gla_theme = "default", palette_type = "quantitative", palette_name='core', n = 10)
  theme_set(theme_gla(gla_theme = "default"))
  
  plot <- ggplot(df_plot_bounds, mapping=aes(fill=prop_resp)) + 
    ggla_sf(
      color='white'
    ) +
    scale_fill_gradientn(colors=rev(pal), labels = function(x) paste0(x, "%")) +
    labs(
      title = paste(strwrap(title, width = 70), collapse = "\n"),
      subtitle = paste(strwrap(subtitle, width = 90), collapse = "\n"),
      caption = "Chart: GLA City Intelligence. Source: Participaton Survey 2023/24, DCMS."
    ) +
    #coord_sf(clip='on') +
    theme(
      #legend.direction='horizontal',
      #plot.margin = unit(c(1,1,1,-5), "cm")
      legend.position='left',
      legend.justification='top',
      plot.title.position='plot',
      legend.text=element_text(size=22*mm_to_pt),
      legend.key.height=unit(8, 'mm')
      #axis.text.y=element_text(margin = margin(r = 10*mm_to_pt))
    )
  plot
  return(plot)
}


#772x587
