#'[Script]#'*process-2_2_2_6.R*
#'[Project]#'*economic_fairness_gla (https://github.com/mawtee/economic-fairness-gla)*
#'[Author]#'*M. Tibbles*
#'[Last Update]#'*02/10/2024*
#'[Description]#'*This blah *
#'[____________________________________________________________________________]



library(tidyverse)
library(rvest)
library(readxl)
library(readODS)
library(tidyverse)
library(gglaplot)
library(geojsonio)

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
  release_year <- '2023_24'
  
  df <- read_ods(
    "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods", 
    sheet=paste0('Table_', sheet_letter, sheet_number), skip=3
  )
  title <- gsub(r"{\s*\([^\)]+\)}","",df[1, 'Question'])
  subtitle <- "Percentage of respondents"
  if (nchar(gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))>1) {
    subtitle <- paste0(subtitle," who ", gsub("\\(([^()]*)\\)|.", "\\1", df[1, 'Question'], perl=T))
  }
  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  df_plot <- df %>% 
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
    mutate(across(contains('resp'),~ round(.x,1))) %>%
    mutate(region = fct_reorder(region, -prop_resp)) %>%
    arrange(region) 


  gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1), main_colours='purple')
  
   plot <- highchart() %>%
     hc_chart(spacingBottom= 50) %>%
     hc_add_series(
       type='bar', 
       name='Central estimate',
       showInLegend=F,
       data=df_plot, 
       hcaes(x = region, y = round(prop_resp,1), color=color)
     ) %>%
     hc_add_series(
       type='errorbar',
       name='Lower-Upper estimate',
       data=df_plot,
       hcaes(x=region, low=prop_resp_lb, high=prop_resp_ub)#,
       #color='#943fa6'
       # tooltip=list(
       #   pointFormat='(error range: {point.low}-{point.high}°C)<br/>'
       # )
     ) %>%
     hc_xAxis(
       categories=levels(df_plot$region),
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
       )
     ) %>%
     hc_yAxis(
       title =list(enabled=F),
       gridLineWidth=0,
       tickInterval=10,
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
     hc_title(
       text=title,
       align='left',
       style = list(
         fontSize ="3.2vh",color = "#333333", 
         fontFamily = "Arial", fontWeight = "600"
       )
     ) %>%
     hc_subtitle(
       text=subtitle,
       align='left',
       style = list(
         fontSize ="2.4vh",color = "#333333", 
         fontFamily = "Arial", fontWeight = "350"
       )
     ) %>%
     hc_credits(
       enabled=T,
       useHTML=T,
       text='Chart: <a href="https://data.london.gov.uk/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence</a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
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
     hc_plotOptions(
       bar = list(
         pointWidth=72
       ),
       errorbar=list(
         stemWidth= 1,
         whiskerWidth=1,
         whiskerLength= 50
       )
     ) %>%
     hc_tooltip(
       valueSuffix= '%',
       borderWidth=2.5,
       shared=T,
       style=list(fontSize='1.4vh')
     ) 

   
 
 
 
 # 
 # 
 # 
 # 
 #   hchart(
 #   df_plot,type = "bar",
 #   hcaes(x = factor(region), y = round(prop_resp,1), color=fill),
 #   name = "Central estimate",
 #   showInLegend=F
 #   ) %>%
 #   hc_xAxis(
 #     categories=rev(levels(df_plot$region)),
 #     title=list(enabled=F),
 #     labels = list(
 #       align='left',
 #       reserveSpace=T,
 #       style=list(
 #         fontSize='1.75vh',
 #         color='#9b9b9b',
 #         fontFamily = "Arial"
 #       )
 #     )
 #   ) %>%
 #   hc_yAxis(
 #     title =list(enabled=F),
 #     gridLineWidth=0,
 #     labels=list(
 #       format="{value}%",
 #       style=list(
 #         fontSize='1.75vh',
 #         color='#9b9b9b',
 #         fontFamily = "Arial"
 #       )
 #     )
 #   ) %>%
 #   hc_title(
 #     text=title,
 #     align='left',
 #     style = list(
 #       fontSize ="3.2vh",color = "#333333", 
 #       fontFamily = "Arial", fontWeight = "600"
 #     )
 #   ) %>%
 #   hc_subtitle(
 #     text=subtitle,
 #     align='left',
 #     style = list(
 #       fontSize ="2.2vh",color = "#333333", 
 #       fontFamily = "Arial", fontWeight = "350"
 #     )
 #   ) %>%
 #   hc_plotOptions(
 #     bar = list(
 #       pointWidth=68
 #     ),
 #     errorbar=list(
 #       stemWidth=1
 #     )
 #   ) %>%
 #   hc_tooltip(
 #     valueSuffix= '%',
 #     borderWidth=3
 #   ) 
 # plot
 # 
 # 
 # # crosshairs = TRUE,
 # # shared=T,
 # # 
 # # sort = TRUE,
 # # table = TRUE
 # # 
 # 
 # 
 # %>%
 #   hc_colors(pal)
 # 
 # plot
 #  
 #  
 #  highchart() %>%
 #  hc_add_series(df_plot, type='bar', hcaes(x=factor(region), y=prop_resp)) %>%
 #    hc_xAxis(categories=rev(levels(df_plot$region)))
 #  
 #  
 #  theme_set(theme_gla(gla_theme = "default"))
 #  
 #  plot <- ggplot(df_plot, mapping=aes(x=factor(region), y=prop_resp, fill=factor(flag))) +
 #    ggla_horizbar(width=1.8 * mm_to_pt) +
 #    coord_flip(ylim=c(50,100)) +
 #    scale_fill_manual(values=rev(pal)) +
 #    scale_y_continuous(labels = function(x) paste0(x, "%")) +
 #    labs(
 #      title = paste(strwrap(title, width = 70), collapse = "\n"),
 #      subtitle = paste(strwrap(subtitle, width = 100), collapse = "\n"),
 #      caption = "Chart: GLA City Intelligence. Source: Participaton Survey 2023/24, DCMS."
 #    ) +
 #    theme(
 #      legend.position='none',
 #      axis.text.y=element_text(margin = margin(r = 10*mm_to_pt))
 #    )
 #  
  
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
      prop_resp_lb=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Lower estimate'),
      prop_resp_ub=paste0('Percentage of respondents ',gsub('_', '/', release_year), ' Upper estimate'),
      num_resp=paste0(gsub('_', '/', release_year), ' No. of respondents')
    ) %>%
    filter(grepl('London', region)) %>%
    mutate(borough = fct_reorder(borough, prop_resp)) %>%
    mutate(across(contains('resp'),~ round(.x,1))) 
  
  pal <- gla_pal(gla_theme = "default", palette_type = "quantitative", palette_name='core', n = 20, main_colours='blue')
  min <- min(df_plot$prop_resp)
  max <- max(df_plot$prop_resp)
    #sort(df_plot$prop_resp, TRUE)[2]+(.05*max(df_plot$prop_resp))
  
  bounds <- geojsonio::geojson_read( "C:/Users/Matt/Downloads/london_421.geojson", what = "list")
  bounds_select <- list('type'=bounds$type, 'features'=list(bounds$features[[1]]))
  
  df_plot$value <- df_plot$prop_resp # to make a choropleth
  
 plot <- highchart(type='map') %>%
    hc_chart(spacingBottom= 50) %>%
    # hc_title(
    #   text = "Number of stop-searches",
    #   align = "left",style = list(
    #     fontSize ="28px",color = "#333333", 
    #     fontFamily = "Arial", fontWeight = "400" 
    #   )
    # ) %>%
    hc_subtitle(text = "") %>%
    #hc_add_series_map(bounds, df_plot, value = "prop_resp", joinBy=c("name", "borough")) %>%
    hc_add_series(mapData=bounds, data=list_parse(df_plot), joinBy=c("name", "borough"), name="{point.name}") %>%
    hc_colorAxis(
      minColor = pal[20],
      maxColor = pal[1],
      min=min,
      max=max
    ) %>%
    hc_title(
      text=title,
      align='left',
      style = list(
        fontSize ="3.2vh",color = "#333333", 
        fontFamily = "Arial", fontWeight = "600"
      )
    ) %>%
    hc_subtitle(
      text=subtitle,
      align='left',
      style = list(
        fontSize ="2.4vh",color = "#333333", 
        fontFamily = "Arial", fontWeight = "350"
      )
    ) %>%
    hc_credits(
      enabled=T,
      useHTML=T,
      text='Chart: <a href="https://data.london.gov.uk/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence</a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
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
      valueSuffix= '%',
      borderWidth=3,
      shared=T,
      style=list(fontSize='1.3vh'),
      shape='callout',
      useHTML = TRUE, 
      headerFormat = "",
      pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
    ) 
  
# 
#   
#   
#   
#   df_plot_bounds <- sf::st_read(
#     "C:/Users/Matt/Downloads/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp"
#   ) %>%
#     right_join(df_plot, by=c('NAME'='borough'))
#   
#   
#   pal <- gla_pal(gla_theme = "default", palette_type = "quantitative", palette_name='core', n = 10)
#   theme_set(theme_gla(gla_theme = "default"))
#   
#   plot <- ggplot(df_plot_bounds, mapping=aes(fill=prop_resp)) + 
#     ggla_sf(
#       color='white'
#     ) +
#     scale_fill_gradientn(colors=rev(pal), labels = function(x) paste0(x, "%")) +
#     labs(
#       title = paste(strwrap(title, width = 70), collapse = "\n"),
#       subtitle = paste(strwrap(subtitle, width = 90), collapse = "\n"),
#       caption = "Chart: GLA City Intelligence. Source: Participaton Survey 2023/24, DCMS."
#     ) +
#     #coord_sf(clip='on') +
#     theme(
#       #legend.direction='horizontal',
#       #plot.margin = unit(c(1,1,1,-5), "cm")
#       legend.position='left',
#       legend.justification='top',
#       plot.title.position='plot',
#       legend.text=element_text(size=22*mm_to_pt),
#       legend.key.height=unit(8, 'mm')
#       #axis.text.y=element_text(margin = margin(r = 10*mm_to_pt))
#     )
#   plot
  return(plot)
}


#772x587
