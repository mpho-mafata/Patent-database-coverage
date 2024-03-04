# PTMT coverage
{
  # patent type coverage
  {
    rm(list = ls())
    library("RPostgres")
    library("glue")
    library("tidyverse")
    library("hrbrthemes")
    library("stringr")
    library("viridis")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    table1 = 'PTMT_applications_type'
    schema1 = 'mpho'
    query1 <-
      dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
    type_applications_coverage <- dbFetch(query1)
    type_applications_coverage$Utility_Patents <-
      as.numeric(str_replace_all(
        type_applications_coverage$Utility_Patents,
        fixed(" "),
        ""
      )) / 1000
    type_applications_coverage$Design_Patents <-
      as.numeric(str_replace_all(type_applications_coverage$Design_Patents, fixed(" "), "")) /
      1000
    type_applications_coverage$Plant_Patents <-
      as.numeric(str_replace_all(type_applications_coverage$Plant_Patents, fixed(" "), "")) /
      1000
    type_applications_coverage$Total_Patents <-
      as.numeric(str_replace_all(type_applications_coverage$Total_Patents , fixed(" "), "")) /
      1000
    
    table2 = 'PTMT_grant_type'
    schema2 = 'mpho'
    query2 <-
      dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
    type_grants_coverage <- dbFetch(query2)
    type_grants_coverage$Utility_Patents <-
      as.numeric(str_replace_all(type_grants_coverage$Utility_Patents, fixed(" "), "")) /
      1000
    type_grants_coverage$Design_Patents <-
      as.numeric(str_replace_all(type_grants_coverage$Design_Patents, fixed(" "), "")) /
      1000
    type_grants_coverage$Plant_Patents <-
      as.numeric(str_replace_all(type_grants_coverage$Plant_Patents, fixed(" "), "")) /
      1000
    type_grants_coverage$Reissue_Patents <-
      as.numeric(str_replace_all(type_grants_coverage$Reissue_Patents, fixed(" "), "")) /
      1000
    type_grants_coverage$Total_Patents <-
      as.numeric(str_replace_all(type_grants_coverage$Total_Patents , fixed(" "), "")) /
      1000
    
    dataset <- list ('type_applications_coverage',
                     'type_grants_coverage')
    application_type <- list('Utility_Patents',
                             'Design_Patents',
                             'Plant_Patents',
                             'Reissue_Patents')
    type_applications_coverage_line <-
      type_applications_coverage %>% pivot_longer(
        cols = c('Utility_Patents',
                 'Design_Patents',
                 'Plant_Patents'),
        names_to = 'patent_type',
        values_to = 'n_patents'
      )
    type_applications_coverage_line <-
      type_applications_coverage_line %>% mutate(patent_type = fct_reorder2(patent_type, Year, n_patents))
    
    type_grants_coverage_line <-
      type_grants_coverage %>% pivot_longer(
        cols = c(
          'Utility_Patents',
          'Design_Patents',
          'Plant_Patents',
          'Reissue_Patents'
        ),
        names_to = 'patent_type',
        values_to = 'n_patents'
      )
    type_grants_coverage_line <-
      type_grants_coverage_line %>% mutate(patent_type = fct_reorder2(patent_type, Year, n_patents))
    
    dataset_lines <- list ('type_applications_coverage_line',
                           'type_grants_coverage_line')
    for (dataset_graph in dataset_lines) {
      dataset_graph_linegraph <- ggplot(
        get(dataset_graph),
        aes(
          x = Year,
          y = n_patents,
          group = patent_type,
          colour = patent_type,
          fill = patent_type
        )
      ) +
        geom_line(linewidth = 0.25) +
        geom_point(size = 0.8, shape = 21) +
        theme_ipsum(base_family = "Arial", axis_text_size = 18) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 0.5,
            vjust = 0.5
          ),
          axis.title.x = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.title.y = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          legend.position = c(0.16, 0.7),
          legend.box.background = element_rect(fill = 'white', colour = 'black'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)
        ) +
        scale_colour_viridis(
          aesthetics = c("fill", "colour"),
          discrete = TRUE,
          option = "H",
          direction = 1,
          begin = 1,
          end = 0
        ) +
        scale_x_continuous(breaks = seq(
          from = 1963,
          to = 2020,
          by = 3
        ),
        minor_breaks = 1) +
        scale_y_continuous(breaks = seq(
          from = 0,
          to = 700,
          by = 100
        )) +
        labs(
          x = "Year",
          y = glue("Number of applications/grants (thousands)"),
          fill = "Application/Grant type",
          colour = "Application/Grant type"
        )
      
      ggsave(
        glue("PTMT_{dataset_graph}.jpg"),
        plot = dataset_graph_linegraph,
        width = 25,
        height = 15,
        units = 'cm'
      )
      browseURL(glue("PTMT_{dataset_graph}.jpg"))
    }
  }
  
  # geographical coverage maps
  {
    rm(list = ls())
    library("writexl")
    library("tidyverse")
    library("dplyr")
    library("countrycode")
    library("plotly")
    library("htmlwidgets")
    library("RPostgres")
    library("glue")
    library("hrbrthemes")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    table1 = 'PTMT_country'
    schema1 = 'mpho'
    query1 <-
      dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
    geographic_coverage <- dbFetch(query1)
    
    # create datasets for different time windows
    geographic_coverage$region_r <-
      str_trim(str_to_lower(geographic_coverage$region_r))
    geographic_coverage <-
      geographic_coverage %>% filter (region_r != "")
    geographic_coverage$mil_total <-
      as.numeric(apply(geographic_coverage[, 40:55], 1, sum, na.rm = TRUE)) # 2000 to 2015
    geographic_coverage$pre_mil_total <-
      as.numeric(apply(geographic_coverage[, 3:39], 1, sum, na.rm = TRUE)) # 1963 to 1999
    geographic_coverage <-
      geographic_coverage %>% dplyr::select(region_r, mil_total, pre_mil_total, Total)
    geographic_coverage[geographic_coverage == 0] <- NA
    geographic_coverage$region_r[geographic_coverage$region_r == "macedonia"] <-
      "north macedonia"
    geographic_coverage$region_r[geographic_coverage$region_r == "viet nam"] <-
      "vietnam"
    
    mapdata <-
      map_data("world") #get the polygon latitudes and longitudes
    mapdata$region_r <- str_to_lower(mapdata$region)
    mapdata <-
      left_join(mapdata, geographic_coverage, by = "region_r")
    
    time_windows <- list ("Total")
    for (time_window in time_windows)
    {
      my_breaks_time_window <-
        as.numeric(round(exp(seq(
          log(min(geographic_coverage[[time_window]], na.rm = TRUE)),
          log(max(geographic_coverage[[time_window]],  na.rm = TRUE)),
          length = 10
        )), digits = 0))
      
      map_time_window <- ggplot(mapdata) +
        geom_polygon(
          aes(
            x = long,
            y = lat,
            group = region,
            subgroup = group,
            fill = get(time_window)
          ),
          colour = "black",
          alpha = 0.9,
          linewidth = 0.1,
          show.legend = TRUE
        ) + scale_fill_gradient2(
          name = "Number of utility grants",
          guide = "legend",
          trans = "log",
          space = "Lab",
          na.value = "white",
          low = "#ff3d3c",
          mid = "#3b407b",
          high = "#89ba17",
          midpoint = log(quantile(geographic_coverage[[time_window]], 0.75,  na.rm = TRUE)),
          breaks = my_breaks_time_window
        ) +  theme(
          legend.key.size = unit(0.5, 'cm'),
          legend.key.width = unit(0.5, 'cm'),
          legend.key.height = unit(0.5, 'cm'),
          legend.title = element_text(size = 6),
          legend.position = c(0.1, 0.45),
          legend.text = element_text(size = 6),
          legend.direction = "vertical",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank()
        )
      ggsave(
        glue("PTMT_geo_{time_window}.jpeg"),
        plot = map_time_window,
        width = 20,
        height = 10,
        units = "cm",
        dpi = 900
      )
      browseURL(glue("PTMT_geo_{time_window}.jpeg"))
    }
  }
}

# PATSTAT coverage
{
  # patent type coverage
  {
    rm(list = ls())
    library("tidyverse")
    library("plotly")
    library("htmlwidgets")
    library("RPostgres")
    library("hrbrthemes")
    library("glue")
    library("viridis")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    table1 = 'patstat_us_patent_type_appln_coverage'
    schema1 = 'mpho'
    query1 <-
      dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
    applications_coverage <- dbFetch(query1)
    applications_coverage$n_patents <-
      as.numeric(applications_coverage$n_applns)
    applications_coverage$ip_type <-
      trimws(applications_coverage$ip_type)
    applications_coverage$n_patents <-
      as.numeric(applications_coverage$n_applns)
    applications_coverage$year <-
      as.numeric(applications_coverage$appln_year)
    applications_coverage$n_patents <-
      as.numeric(applications_coverage$n_patents) / 1000
    applications_coverage <-
      applications_coverage %>% filter (year < 2020) %>% mutate(ip_type = fct_reorder2(ip_type, year, n_patents))
    # create an aggregate applications table for utility patents
    patstat_utility_applications <-
      applications_coverage %>% dplyr::filter(ip_type == "A") %>% dplyr::select(year, n_applns)
    
    
    table2 = 'patstat_us_patent_type_grants_coverage'
    schema2 = 'mpho'
    query2 <-
      dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
    grants_coverage <- dbFetch(query2)
    grants_coverage$n_patents <-
      as.numeric(grants_coverage$n_grants)
    grants_coverage$ip_type <- trimws(grants_coverage$ip_type)
    grants_coverage$year <-
      as.numeric(grants_coverage$grant_year)
    grants_coverage$n_patents <-
      as.numeric(grants_coverage$n_patents) / 1000
    grants_coverage <-
      grants_coverage %>% filter (year < 2020) %>% mutate(ip_type = fct_reorder2(ip_type, year, n_patents))
    # create an aggregate grants table
    patstat_utility_grants <- grants_coverage %>%
      dplyr::filter(ip_type == "A" |
                      ip_type == "B1" | ip_type == "B2") %>%
      dplyr::select(year, n_grants)
    patstat_utility_grants <- patstat_utility_grants %>%
      group_by(year) %>%
      mutate(n_patents = sum(n_grants))
    
    patstat_types <- c('applications_coverage', 'grants_coverage')
    for (patstat_type in patstat_types) {
      my_breaks <- seq(
        from = 0,
        to = ceiling(max(get(patstat_type)$n_patents)),
        by = round(ceiling(max(
          get(patstat_type)$n_patents
        )) / 12,
        digits = 0)
      )
      
      my_minor_breaks <-
        round(ceiling(max(get(patstat_type)$n_patents)) / 12, digits = 0)
      
      patstat_type_linegraph <- ggplot(
        get(patstat_type),
        aes(
          x = year,
          y = n_patents,
          colour = ip_type,
          fill = ip_type,
          group = ip_type
        )
      ) +
        geom_line(linewidth = 0.2) +
        geom_point(size = 0.2, shape = 21) +
        theme_ipsum(base_family = "Arial", axis_text_size = 18) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 0.5,
            vjust = 0.5
          ),
          axis.title.x = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.title.y = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.ticks.length.x = unit(0, "cm"),
          axis.ticks.length.y = unit(0, "cm"),
          legend.direction = "horizontal",
          legend.position = c(0.3, 0.55),
          legend.box.background = element_rect(
            fill = 'white',
            colour = 'black',
            linewidth = 0.1
          ),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)
        ) +
        scale_colour_viridis(
          aesthetics = c("fill", "colour"),
          discrete = TRUE,
          option = "H",
          direction = 1,
          begin = 1,
          end = 0
        ) +
        scale_x_continuous(breaks = seq(
          from = 1790,
          to = 2022,
          by = 10
        ),
        minor_breaks = 5) +
        scale_y_continuous(breaks = my_breaks, minor_breaks = my_minor_breaks) +
        labs(
          x = "Year",
          y = glue("Number of applications/grants (thousands)"),
          fill = "Application/Publication type",
          colour = "Application/Publication type"
        )
      ggsave(
        glue("patstat_type_{patstat_type}.jpg"),
        plot = patstat_type_linegraph,
        width = 30,
        height = 15,
        units = 'cm'
      )
      browseURL(glue("patstat_type_{patstat_type}.jpg"))
    }
  }
  
  # geographic coverage maps
  {
    rm(list = ls())
    library("RPostgres")
    library("glue")
    library("tidyverse")
    library("plotly")
    library("htmlwidgets")
    library("countrycode")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    table3 = 'patstat_us_geographic_coverage'
    schema3 = 'mpho'
    query3 <-
      dbSendQuery(connec, glue('select * from "{schema3}"."{table3}";'))
    patstat_geographic_coverage <- dbFetch(query3)
    patstat_geographic_coverage$ip_type <-
      trimws(patstat_geographic_coverage$ip_type)
    patstat_geographic_coverage$country_full_name <-
      countrycode(patstat_geographic_coverage$person_ctry_code,
                  "iso2c",
                  "country.name")
    
    # add the country names for the country codes that were not matched by countrycode library but are recognised
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "AP"] <-
      "African Regional Industrial Property Organization"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "BX"] <-
      "Benelux Trademarks and Designs Office"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "EP"] <-
      "European Patent Organization"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "GC"] <-
      "Patent Office of the Cooperation Council for the Arab States of the Gulf (GCC)"
    patstat_geographic_coverage$person_ctry_code[patstat_geographic_coverage$person_ctry_code == "TS"] <-
      "TD"
    patstat_geographic_coverage$person_ctry_code[patstat_geographic_coverage$person_ctry_code == "UK"] <-
      "GB"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "WO"] <-
      "World Intellectual Property Organization"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Congo - Brazzaville"] <-
      "Republic of Congo" #this must change to DRC
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Bosnia & Herzegovina"] <-
      "bosnia and herzegovina"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "United States"] <-
      "usa"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "eswatini"] <-
      "swaziland"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "côte d’ivoire"] <-
      "ivory coast"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "United Kingdom"] <-
      "uk"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "czechia"] <-
      "czech republic"
    patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "myanmar (burma)"] <-
      "myanmar"
    
    
    # change the country names to lower case to better match them for mapping
    patstat_geographic_coverage$country_full_name <-
      str_to_lower(patstat_geographic_coverage$country_full_name)
    colnames(patstat_geographic_coverage)[colnames(patstat_geographic_coverage) ==
                                            "country_full_name"] <- "region"
    
    # compile the dataframe for visualization
    patstat_geo_map_utility_aggregate <-
      patstat_geographic_coverage %>% filter (ip_type == "A" |
                                                ip_type == "B1" |
                                                ip_type == "B2") %>% dplyr::select(region, grant_year, n_grants)
    patstat_geo_map_utility_aggregate <-
      patstat_geo_map_utility_aggregate %>% group_by(region) %>% mutate(total = sum(n_grants))
    patstat_geo_map_utility_aggregate <-
      patstat_geo_map_utility_aggregate %>% select(region, total) %>% filter(!duplicated(region))
    
    mapdata <- map_data("world") #get the latitudes and longitudes
    mapdata$region <- str_to_lower(mapdata$region)
    
    iptypes <- c("patstat_geo_map_utility_aggregate")
    for (iptype in iptypes)
    {
      # make time_windows a list from each iptype
      time_windows <- t(t(colnames(get(iptype)[, c(-1)])))
      
      mapdata_iptype <-
        left_join(mapdata, get(iptype), by = "region")
      for (time_window in time_windows)
      {
        my_breaks_time_window <-
          as.numeric(round(exp(seq(
            log(min(get(iptype)[[time_window]], na.rm = TRUE)),
            log(max(get(iptype)[[time_window]],  na.rm = TRUE)),
            length = 10
          )), digits = 0))
        
        iptype_time_window <- ggplot(mapdata_iptype) +
          geom_polygon(
            aes(
              x = long,
              y = lat,
              group = region,
              subgroup = group,
              fill = get(time_window)
            ),
            colour = "black",
            alpha = 0.9,
            linewidth = 0.1,
            show.legend = TRUE
          ) + scale_fill_gradient2(
            name = "Number of patents",
            guide = "legend",
            trans = "log",
            space = "Lab",
            na.value = "white",
            low = "#ff3d3c",
            mid = "#3b407b",
            high = "#89ba17",
            midpoint = log(quantile(get(iptype)[[time_window]], 0.75,  na.rm = TRUE)),
            breaks = my_breaks_time_window
          ) +  theme(
            legend.key.size = unit(1, 'cm'),
            legend.key.width = unit(0.7, 'cm'),
            legend.key.height = unit(0.8, 'cm'),
            legend.title = element_text(size = 9),
            legend.position = c(0.1, 0.45),
            legend.text = element_text(size = 8),
            legend.direction = "vertical",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            rect = element_blank()
          )
        ggsave(
          glue("{iptype}_{time_window}.jpeg"),
          plot = iptype_time_window,
          width = 12.5,
          height = 6,
          dpi = 900
        )
        browseURL(glue("{iptype}_{time_window}.jpeg"))
        # saveWidget(ggplotly(iptype_time_window), file = glue("{iptype}_{time_window}.html"))
        # browseURL(glue("{iptype}_{time_window}.html"))
      }
    }
  }
}

# PatentsView coverage
{
  # patent type coverage
  {
    rm(list = ls())
    library("tidyverse")
    library("plotly")
    library("htmlwidgets")
    library("RPostgres")
    library("hrbrthemes")
    library("glue")
    library("viridis")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    # Fetch required data
    table1 = 'patentsview_patent_type_applications_coverage'
    schema1 = 'mpho'
    query1 <-
      dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
    applications_coverage <- dbFetch(query1)
    applications_coverage$n_applns <-
      as.numeric(applications_coverage$n_applns) / 1000
    applications_coverage <-
      applications_coverage %>% filter (appln_year >= 1970 &
                                          appln_year <= 2020 & appln_type != 'NA')
    applications_coverage <-
      applications_coverage %>% select (appln_year, appln_type, n_applns) %>% group_by(appln_year, appln_type) %>% summarise(n_patents = sum(n_applns))
    colnames(applications_coverage)[colnames(applications_coverage) == "appln_type"] <-
      "ip_type"
    colnames(applications_coverage)[colnames(applications_coverage) == "appln_year"] <-
      "year"
    
    table2 = 'patentsview_patent_type_grants_coverage'
    schema2 = 'mpho'
    query2 <-
      dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
    grants_coverage <- dbFetch(query2)
    colnames(grants_coverage)[colnames(grants_coverage) == "wipo_kind"] =
      "ip_type"
    grants_coverage$n_patents <-
      as.numeric(grants_coverage$n_patents) / 1000
    grants_coverage <-
      grants_coverage %>% filter (year >= 1900 & year <= 2020)
    
    patent_types <- c('applications_coverage', 'grants_coverage')
    for (patent_type in patent_types) {
      my_breaks <- seq(
        from = 0,
        to = ceiling(max(get(patent_type)$n_patents)),
        by = round(ceiling(max(
          get(patent_type)$n_patents
        )) / 14,
        digits = 0)
      )
      my_minor_breaks <-
        round(ceiling(max(get(patent_type)$n_patents)) / 14, digits = 0)
      
      patent_type_linegraph <- ggplot(
        get(patent_type),
        aes(
          x = year,
          y = n_patents,
          colour = reorder(ip_type,-n_patents),
          fill = reorder(ip_type,-n_patents),
          group = reorder(ip_type,-n_patents)
        )
      ) +
        geom_line(linewidth = 0.2) +
        geom_point(size = 0.2, shape = 21) +
        theme_ipsum(base_family = "Arial", axis_text_size = 18) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 0.5,
            vjust = 0.5
          ),
          axis.title.x = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.title.y = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.ticks.length.x = unit(0, "cm"),
          axis.ticks.length.y = unit(0, "cm"),
          # legend.key.height = unit(0.5, "cm"),
          # legend.key.width = unit(0.3, "cm"),
          legend.direction = "horizontal",
          legend.position = c(0.3, 0.62),
          legend.box.background = element_rect(
            fill = 'white',
            colour = 'black',
            linewidth = 0.1
          ),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)
        ) +
        scale_colour_viridis(
          aesthetics = c("fill", "colour"),
          discrete = TRUE,
          option = "H",
          direction = 1,
          begin = 1,
          end = 0
        ) +
        scale_x_continuous(breaks = seq(
          from = min(get(patent_type)$year),
          to = max(get(patent_type)$year),
          by = 2
        ),
        minor_breaks = 2) +
        scale_y_continuous(breaks = my_breaks, minor_breaks = my_minor_breaks) +
        labs(
          x = "Year",
          y = glue("Number of grants (thousands)"),
          colour = "Publication type",
          fill = "Publication type"
        )
      ggsave(
        glue("patentsview_type_{patent_type}.jpg"),
        plot = patent_type_linegraph,
        width = 30,
        height = 15,
        units = 'cm'
      )
      browseURL(glue("patentsview_type_{patent_type}.jpg"))
      # saveWidget(ggplotly(patent_type_linegraph), file = glue("patentsview_type_{patent_type}.html"))
      # browseURL(glue("patentsview_type_{patent_type}.html"))
    }
    
  }
  
  # geographic coverage maps
  {
    rm(list = ls())
    library("RPostgres")
    library("glue")
    library("tidyverse")
    library("plotly")
    library("htmlwidgets")
    library("countrycode")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    table3 = 'patentsview_geographic_coverage'
    schema3 = 'mpho'
    query3 <-
      dbSendQuery(connec, glue('select * from "{schema3}"."{table3}";'))
    geographic_coverage <- dbFetch(query3)
    geographic_coverage$country_full_name <-
      countrycode(geographic_coverage$person_ctry_code,
                  "iso2c",
                  "country.name")
    colnames(geographic_coverage)[colnames(geographic_coverage) == "country_full_name"] =
      "region"
    colnames(geographic_coverage)[colnames(geographic_coverage) == "n_grants"] =
      "n_patents"
    colnames(geographic_coverage)[colnames(geographic_coverage) == "ip_wipo"] =
      "ip_type"
    geographic_coverage$region <-
      str_to_lower(geographic_coverage$region)
    geographic_coverage$person_ctry_code <-
      str_to_lower(geographic_coverage$person_ctry_code)
    geographic_coverage$n_patents <-
      as.numeric(geographic_coverage$n_patents)
    
    # add the country names for the country codes that were not matched by countrycode library but are recognised
    # change the person_ctry_codes to match the actual codes
    geographic_coverage$country_full_name[geographic_coverage$person_ctry_code == "canada"] <-
      "ca"
    # change some country and country code names to match the mapdata region names
    geographic_coverage[geographic_coverage == "united states"] <-
      "usa"
    geographic_coverage[geographic_coverage == "united kingdom"] <-
      "uk"
    geographic_coverage[geographic_coverage == "congo - brazzaville"] <-
      "democratic republic of the congo"
    geographic_coverage[geographic_coverage == "eswatini"] <-
      "swaziland"
    geographic_coverage[geographic_coverage == "british virgin islands"] <-
      "virgin islands"
    geographic_coverage[geographic_coverage == "u.s. virgin islands"] <-
      "virgin islands"
    geographic_coverage[geographic_coverage == "bosnia & herzegovina"] <-
      "bosnia and herzegovina"
    geographic_coverage <-
      geographic_coverage %>% group_by(region) %>% summarise(total = sum(n_patents, na.rm = TRUE))
    geographic_coverage[geographic_coverage == 0] <- NA
    
    mapdata <- map_data("world")
    mapdata$region <- str_to_lower(mapdata$region)
    
    iptypes <- c("geographic_coverage")
    for (iptype in iptypes)
    {
      # make time_windows a list from each IP type
      time_windows <- t(t(colnames(get(iptype)[, c(-1)])))
      
      mapdata_iptype <-
        left_join(mapdata, get(iptype), by = "region")
      for (time_window in time_windows)
      {
        my_breaks_time_window <-
          as.numeric(round(exp(seq(
            log(min(get(iptype)[[time_window]], na.rm = TRUE)),
            log(max(get(iptype)[[time_window]],  na.rm = TRUE)),
            length = 10
          )), digits = 0))
        
        iptype_time_window <- ggplot(mapdata_iptype) +
          geom_polygon(
            aes(
              x = long,
              y = lat,
              group = region,
              subgroup = group,
              fill = get(time_window)
            ),
            colour = "black",
            alpha = 0.9,
            linewidth = 0.1,
            show.legend = TRUE
          ) + scale_fill_gradient2(
            name = "Number of patents",
            guide = "legend",
            trans = "log",
            space = "Lab",
            na.value = "white",
            low = "#ff3d3c",
            mid = "#3b407b",
            high = "#89ba17",
            midpoint = log(quantile(get(iptype)[[time_window]], 0.75,  na.rm = TRUE)),
            breaks = my_breaks_time_window
          ) +  theme(
            legend.key.size = unit(1, 'cm'),
            legend.key.width = unit(0.7, 'cm'),
            legend.key.height = unit(0.8, 'cm'),
            legend.title = element_text(size = 9),
            legend.position = c(0.1, 0.45),
            legend.text = element_text(size = 8),
            legend.direction = "vertical",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            rect = element_blank()
          )
        ggsave(
          glue("patentsview_{iptype}_{time_window}.jpeg"),
          plot = iptype_time_window,
          width = 12.5,
          height = 6,
          dpi = 600
        )
        browseURL(glue("patentsview_{iptype}_{time_window}.jpeg"))
      }
      
    }
    
  }
}

# Dataset comparisons
{
  # patent type coverage
  {
    rm(list = ls())
    library("tidyverse")
    library("plotly")
    library("htmlwidgets")
    library("RPostgres")
    library("hrbrthemes")
    library("glue")
    library("viridis")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    # PTMT data
    table1 = 'PTMT_applications_type'
    schema1 = 'mpho'
    query1 <-
      dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
    ptmt_applications <- dbFetch(query1)
    ptmt_applications$Utility_Patents <-
      as.numeric(str_replace_all(ptmt_applications$Utility_Patents, fixed(" "), ""))
    ptmt_applications <-
      ptmt_applications %>% dplyr::select(Year, Utility_Patents)
    ptmt_applications <-
      ptmt_applications %>% dplyr::filter(Year >= 1963 & Year <= 2020)
    colnames(ptmt_applications)[colnames(ptmt_applications) == "Utility_Patents"] =
      "ptmt_applications"
    colnames(ptmt_applications)[colnames(ptmt_applications) == "Year"] =
      "year"
    
    table2 = 'PTMT_grant_type'
    schema2 = 'mpho'
    query2 <-
      dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
    ptmt_grants <- dbFetch(query2)
    ptmt_grants$Utility_Patents <-
      as.numeric(str_replace_all(ptmt_grants$Utility_Patents, fixed(" "), ""))
    ptmt_grants <-
      ptmt_grants %>% dplyr::select(Year, Utility_Patents) %>% dplyr::filter(Year >=
                                                                               1963 & Year <= 2020)
    colnames(ptmt_grants)[colnames(ptmt_grants) == "Utility_Patents"] =
      "ptmt_grants"
    colnames(ptmt_grants)[colnames(ptmt_grants) == "Year"] = "year"
    ptmt_grants$year <- as.numeric(ptmt_grants$year)
    
    # PATSTAT data
    table3 = 'patstat_us_patent_type_appln_coverage'
    schema3 = 'mpho'
    query3 <-
      dbSendQuery(connec, glue('select * from "{schema3}"."{table3}";'))
    patstat_applications <- dbFetch(query3)
    patstat_applications$n_patents <-
      as.numeric(patstat_applications$n_applns)
    patstat_applications$ip_type <-
      trimws(patstat_applications$ip_type)
    patstat_applications$year <-
      as.numeric(patstat_applications$appln_year)
    
    # create an aggregate applications table for utility patents from all datasets
    patstat_applications <-
      patstat_applications %>%
      dplyr::filter(ip_type == "A" & year >= 1963 & year <= 2020) %>%
      dplyr::select(year, n_patents)
    colnames(patstat_applications)[colnames(patstat_applications) == "n_patents"] =
      "patstat_applications"
    
    table4 = 'patstat_us_patent_type_grants_coverage'
    schema4 = 'mpho'
    query4 <-
      dbSendQuery(connec, glue('select * from "{schema4}"."{table4}";'))
    patstat_grants <- dbFetch(query4)
    patstat_grants$n_grants <- as.numeric(patstat_grants$n_grants)
    patstat_grants$ip_type <- trimws(patstat_grants$ip_type)
    patstat_grants$year <- as.numeric(patstat_grants$grant_year)
    # create an aggregate grants table
    patstat_grants <-
      patstat_grants %>% dplyr::filter(ip_type == "A" |
                                         ip_type == "B1" |
                                         ip_type == "B2") %>% dplyr::select(year, n_grants)
    patstat_grants <-
      patstat_grants %>% group_by(year) %>% mutate(n_grants = sum(n_grants)) %>% filter(!duplicated(year) &
                                                                                          year >= 1963 & year <= 2020)
    colnames(patstat_grants)[colnames(patstat_grants) == "n_grants"] =
      "patstat_grants"
    
    # PATENSTVIEW data
    table5 = 'patentsview_patent_type_applications_coverage'
    schema5 = 'mpho'
    query5 <-
      dbSendQuery(connec, glue('select * from "{schema5}"."{table5}";'))
    patentsview_applications <- dbFetch(query5)
    patentsview_applications$n_applns <-
      as.numeric(patentsview_applications$n_applns)
    patentsview_applications <-
      patentsview_applications %>% filter (appln_type == 'utility application' &
                                             appln_year >= 1963 & appln_year <= 2020)
    patentsview_applications <-
      patentsview_applications %>% select (appln_year, n_applns) %>% group_by(appln_year) %>% summarise(patentsview_applications = sum(n_applns))
    colnames(patentsview_applications)[colnames(patentsview_applications) ==
                                         "appln_year"] <- "year"
    
    table6 = 'patentsview_patent_type_grants_coverage'
    schema6 = 'mpho'
    query6 <-
      dbSendQuery(connec, glue('select * from "{schema6}"."{table6}";'))
    patentsview_grants <- dbFetch(query6)
    patentsview_grants <- patentsview_grants %>%
      dplyr::filter(wipo_kind == "A" |
                      wipo_kind == "B1" | wipo_kind == "B2") %>%
      select (year, n_patents)
    patentsview_grants <- patentsview_grants %>%
      group_by(year) %>%
      summarise(patentsview_grants = sum(n_patents)) %>%
      dplyr::filter(year >= 1963 & year <= 2020)
    patentsview_grants$patentsview_grants <-
      as.numeric(patentsview_grants$patentsview_grants)
    
    patent_types <- list(
      ptmt_applications,
      patstat_applications,
      patentsview_applications,
      ptmt_grants,
      patstat_grants,
      patentsview_grants
    )
    patent_types <- patent_types %>% reduce(full_join, by = "year")
    
    # calculate correlation coefficients
    patent_types %>%
      mutate(corr_coeff = cor(patstat_grants, patentsview_grants))
    
    comparison_correlations <-
      cor(as.data.frame(patent_types[, c(2:7)]),
          use = "pairwise.complete.obs",
          method = "pearson")
    
    # create summary statistics
    patent_types$appln_dev_from_ptmt_patstat <-
      (patent_types$patstat_applications - patent_types$ptmt_applications)
    patent_types$appln_percent_dev_from_ptmt_patstat <-
      ((
        patent_types$patstat_applications - patent_types$ptmt_applications
      ) / (patent_types$ptmt_applications)
      ) * 100
    patent_types$appln_dev_from_ptmt_patentsview <-
      (patent_types$patentsview_applications - patent_types$ptmt_applications)
    patent_types$appln_percent_dev_from_ptmt_patentsview <-
      ((
        patent_types$patentsview_applications - patent_types$ptmt_applications
      ) / (patent_types$ptmt_applications)
      ) * 100
    patent_types$grants_dev_from_ptmt_patstat <-
      (patent_types$patstat_grants - patent_types$ptmt_grants)
    patent_types$grants_percent_dev_from_ptmt_patstat <-
      ((patent_types$patstat_grants - patent_types$ptmt_grants) / (patent_types$ptmt_grants)
      ) * 100
    patent_types$grants_dev_from_ptmt_patentsview <-
      (patent_types$patentsview_grants - patent_types$ptmt_grants)
    patent_types$grants_percent_dev_from_ptmt_patentsview <-
      ((
        patent_types$patentsview_grants - patent_types$ptmt_grants
      ) / (patent_types$ptmt_grants)
      ) * 100
    
    # plot the line graphs
    patent_types <-
      patent_types %>% pivot_longer(!year,
                                    names_to = "ip_type",
                                    values_to = "n_patents")
    patent_applications <-
      patent_types %>% filter(
        ip_type == "ptmt_applications" |
          ip_type == "patstat_applications" |
          ip_type == "patentsview_applications"
      )
    patent_grants <-
      patent_types %>% filter(
        ip_type == "ptmt_grants" |
          ip_type == "patstat_grants" |
          ip_type == "patentsview_grants"
      )
    applications_deviations <-
      patent_types %>% filter(
        ip_type == "appln_dev_from_ptmt_patstat" |
          ip_type == "appln_dev_from_ptmt_patentsview"
      )
    grants_deviations <-
      patent_types %>% filter(
        ip_type == "grants_dev_from_ptmt_patstat" |
          ip_type == "grants_dev_from_ptmt_patentsview"
      )
    
    applications_percent_deviations <-
      patent_types %>% filter(
        ip_type == "appln_percent_dev_from_ptmt_patstat" |
          ip_type == "appln_percent_dev_from_ptmt_patentsview"
      )
    grants_percent_deviations <-
      patent_types %>% filter(
        ip_type == "grants_percent_dev_from_ptmt_patstat" |
          ip_type == "grants_percent_dev_from_ptmt_patentsview"
      )
    
    patent_data <- c(
      "patent_applications",
      "patent_grants",
      "applications_deviations",
      "grants_deviations"
      # "applications_percent_deviations",
      # "grants_percent_deviations"
    )
    plot_list <- list()
    for (i in 1:length(patent_data)) {
      patent_linegraph_i <- ggplot(
        get(patent_data[[i]]),
        aes(
          x = year,
          y = n_patents,
          colour = reorder(ip_type,-n_patents),
          fill = reorder(ip_type,-n_patents),
          group = reorder(ip_type,-n_patents)
        )
      ) +
        geom_line(linewidth = 0.5) +
        geom_point(size = 0.5, shape = 21) +
        theme_ipsum(base_family = "Arial", axis_text_size = 18) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 0.5,
            vjust = 0.5
          ),
          axis.title.x = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.title.y = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.ticks.length.x = unit(0, "cm"),
          axis.ticks.length.y = unit(0, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.1, "cm"),
          legend.direction = "vertical",
          legend.position = c(0.30, 0.51),
          legend.box.background = element_rect(
            fill = 'white',
            colour = 'black',
            linewidth = 0.1
          ),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)
        ) +
        scale_colour_viridis(
          aesthetics = c("fill", "colour"),
          discrete = TRUE,
          option = "H",
          direction = 1,
          begin = 1,
          end = 0
        ) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = seq(
          from = 1963,
          to = 2020,
          by = 5
        ),
        minor_breaks = 1) +
        labs(
          x = "Year",
          y = NULL,
          colour = "data source",
          fill = "data source"
        )
      ggsave(
        glue("comparison_utility_{patent_data[[i]]}.svg"),
        plot = patent_linegraph_i,
        width = 30,
        height = 15,
        units = 'cm',
        dpi = 720
      )
      plot_list[[i]] <- patent_linegraph_i
    }
    grid_plot <-
      gridExtra::grid.arrange(grobs = plot_list,
                              ncol = 2,
                              padding = unit(0.5, "line"))
    ggsave(
      "comparison_grid_plot.jpg",
      plot = grid_plot ,
      width = 42,
      height = 20,
      units = 'cm',
      dpi = 720
    )
    browseURL("comparison_grid_plot.jpg")
    
  }
  
  # geographic coverage
  {
    # multiple factor analysis (MFA)
    {
      rm(list = ls())
      library("tidyverse")
      library("dplyr")
      library("plotly")
      library("htmlwidgets")
      library("RPostgres")
      library("glue")
      library("hrbrthemes")
      library("viridis")
      library("countrycode")
      library("viridis")
      library("FactoMineR")
      library("factoextra")
      # Script settings for connecting to postgreSQL
      credentials <-
        readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
      crest_dbname =  credentials$crest_dbname[c(2)]
      crest_host = credentials$crest_host[c(2)]
      crest_port = 5432
      crest_user = credentials$crest_user[c(2)]
      crest_password = credentials$crest_password[c(2)]
      drv <- RPostgres::Postgres()
      connec <- dbConnect(
        drv,
        dbname = crest_dbname,
        host = crest_host,
        port = crest_port,
        user = crest_user,
        password = crest_password
      )
      table1 = 'PTMT_country'
      schema1 = 'mpho'
      query1 <-
        dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
      ptmt_geographic_coverage <- dbFetch(query1)
      ptmt_geographic_coverage <- ptmt_geographic_coverage[, c(-2)]
      ptmt_geographic_coverage$region <-
        str_trim(str_to_lower(ptmt_geographic_coverage$region))
      ptmt_geographic_coverage$region[ptmt_geographic_coverage$region == "macedonia"] <-
        "north macedonia"
      ptmt_geographic_coverage$region[ptmt_geographic_coverage$region == "viet nam"] <-
        "vietnam"
      
      # filter the years to match with PatentsView
      ptmt_geographic_coverage <-
        ptmt_geographic_coverage[, c(-2:-14, -55)]
      row.names(ptmt_geographic_coverage) <-
        ptmt_geographic_coverage$region
      ptmt_geographic_coverage <- ptmt_geographic_coverage[, c(-1)]
      ptmt_geographic_coverage <-
        as.data.frame(t(ptmt_geographic_coverage))
      ptmt_geographic_coverage$grant_year <-
        as.numeric(row.names(ptmt_geographic_coverage))
      
      # Patstat data
      table2 = 'patstat_us_geographic_coverage'
      schema2 = 'mpho'
      query2 <-
        dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
      patstat_geographic_coverage <- dbFetch(query2)
      patstat_geographic_coverage$ip_type <-
        trimws(patstat_geographic_coverage$ip_type)
      patstat_geographic_coverage$n_grants <-
        as.numeric(patstat_geographic_coverage$n_grants)
      patstat_geographic_coverage$country_full_name <-
        countrycode(patstat_geographic_coverage$person_ctry_code,
                    "iso2c",
                    "country.name")
      
      # add the country names for the country codes that were not matched but are recognised
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "AP"] <-
        "African Regional Industrial Property Organization"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "BX"] <-
        "Benelux Trademarks and Designs Office"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "EP"] <-
        "European Patent Organization"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "GC"] <-
        "Patent Office of the Cooperation Council for the Arab States of the Gulf (GCC)"
      patstat_geographic_coverage$person_ctry_code[patstat_geographic_coverage$person_ctry_code == "TS"] <-
        "TD"
      patstat_geographic_coverage$person_ctry_code[patstat_geographic_coverage$person_ctry_code == "UK"] <-
        "GB"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "WO"] <-
        "World Intellectual Property Organization"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Congo - Brazzaville"] <-
        "Republic of Congo"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Bosnia & Herzegovina"] <-
        "bosnia and herzegovina"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "United States"] <-
        "usa"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Eswatini"] <-
        "swaziland"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Côte d’Ivoire"] <-
        "ivory coast"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "United Kingdom"] <-
        "uk"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Czechia"] <-
        "czech republic"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Myanmar (Burma)"] <-
        "myanmar"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Hong Kong SAR China"] <-
        "China"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Vatican City"] <-
        "Vatican"
      
      # change the country names to lower case to better match them for mapping
      patstat_geographic_coverage$country_full_name <-
        str_to_lower(patstat_geographic_coverage$country_full_name)
      colnames(patstat_geographic_coverage)[colnames(patstat_geographic_coverage) ==
                                              "country_full_name"] <- "region"
      
      # compile the dataframe for visualization
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% filter (ip_type == "A" |
                                                  ip_type == "B1" | ip_type == "B2")
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% filter (grant_year >= 1976 &
                                                  grant_year <= 2015)
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% group_by(region, grant_year) %>% mutate(n_grants = sum(n_grants))
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% filter(region != '')
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% filter(!duplicated(region, grant_year))
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% select(grant_year, region, n_grants) %>% pivot_wider(
          names_from = region,
          values_from = n_grants,
          values_fill = 0
        )
      patstat_geographic_coverage <-
        as.data.frame(patstat_geographic_coverage)
      row.names(patstat_geographic_coverage) <-
        patstat_geographic_coverage$grant_year
      # patstat_geographic_coverage <- patstat_geographic_coverage[,c(-1)]
      
      # patentsView dataset
      table3 = 'patentsview_geographic_coverage'
      schema3 = 'mpho'
      query3 <-
        dbSendQuery(connec, glue('select * from "{schema3}"."{table3}";'))
      patentsview_geographic_coverage <- dbFetch(query3)
      
      # get the country names using the country codes
      patentsview_geographic_coverage$country_full_name <-
        countrycode(patentsview_geographic_coverage$person_ctry_code,
                    "iso2c",
                    "country.name")
      colnames(patentsview_geographic_coverage)[colnames(patentsview_geographic_coverage) == "country_full_name"] =
        "region"
      colnames(patentsview_geographic_coverage)[colnames(patentsview_geographic_coverage) == "n_grants"] =
        "n_patents"
      colnames(patentsview_geographic_coverage)[colnames(patentsview_geographic_coverage) == "ip_wipo"] =
        "ip_type"
      patentsview_geographic_coverage$region <-
        str_to_lower(patentsview_geographic_coverage$region)
      patentsview_geographic_coverage$person_ctry_code <-
        str_to_lower(patentsview_geographic_coverage$person_ctry_code)
      patentsview_geographic_coverage$n_patents <-
        as.numeric(patentsview_geographic_coverage$n_patents)
      
      # change the person_ctry_codes to match the actual codes
      patentsview_geographic_coverage$region[patentsview_geographic_coverage$person_ctry_code == "usa"] <-
        "united states"
      patentsview_geographic_coverage$region[patentsview_geographic_coverage$person_ctry_code == "canada"] <-
        "ca"
      
      # change some country and country code names to match the mapdata region names
      patentsview_geographic_coverage[patentsview_geographic_coverage == "united states"] <-
        "usa"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "united kingdom"] <-
        "uk"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "congo - brazzaville"] <-
        "democratic republic of the congo"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "eswatini"] <-
        "swaziland"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "british virgin islands"] <-
        "virgin islands" # need to distinguih these by sub-region
      patentsview_geographic_coverage[patentsview_geographic_coverage == "u.s. virgin islands"] <-
        "virgin islands"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "bosnia & herzegovina"] <-
        "bosnia and herzegovina"
      # filter out the utility patents
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>% filter (ip_type == 'A' |
                                                      ip_type == 'B1' | ip_type == 'B2')
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>% filter (grant_year >= 1976 &
                                                      grant_year <= 2015) %>% select(grant_year, region, n_patents)
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>% group_by(region, grant_year) %>% mutate(n_patents = sum(n_patents))
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>% filter(region != '')
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>% filter(!duplicated(region, grant_year))
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>%
        select(grant_year, region, n_patents) %>%
        pivot_wider(
          names_from = region,
          values_from = n_patents,
          values_fill = 0
        )
      patentsview_geographic_coverage <-
        as.data.frame(patentsview_geographic_coverage)
      row.names(patentsview_geographic_coverage) <-
        patentsview_geographic_coverage$grant_year
      
      # Calculate the RV coefficient  between dataframes
      ptmt_patstat_rv <- coeffRV(ptmt_geographic_coverage,
                                 patstat_geographic_coverage)
      ptmt_patentsview_rv <- coeffRV(ptmt_geographic_coverage,
                                     patentsview_geographic_coverage)
      patstat_patentsview_rv <-
        coeffRV(patstat_geographic_coverage,
                patentsview_geographic_coverage)
      
      # join data sets for MFA analysis
      mfa_geo <-
        full_join(ptmt_geographic_coverage,
                  patstat_geographic_coverage,
                  by = "grant_year")
      mfa_geo <-
        full_join(mfa_geo, patentsview_geographic_coverage, by = "grant_year")
      mfa_geo <- as.data.frame(mfa_geo)
      row.names(mfa_geo) <- mfa_geo$grant_year
      mfa_geo <- mfa_geo[,!names(mfa_geo) %in% c("grant_year")]
      
      # generate the MFA calculations
      mfa_geo_plot <- MFA(
        mfa_geo,
        # the group lengths are the lengths of the seperate datasets minus the grant year which was used to merge them
        group = c((length(ptmt_geographic_coverage)-1), 
                  (length(patstat_geographic_coverage)-1), 
                  ((length(patentsview_geographic_coverage)-1))),
        type = c(rep("s", 3)),
        ncp = 40,
        name.group = c("PTMT",
                       "Patstat",
                       "PatentsView")
      )
      # plot the MFA results seperately
      plot.MFA(mfa_geo_plot)
      plot(
        mfa_geo_plot,
        choix = "group",
        partial = "all",
        graph.type = "ggplot"
      )
      mfa_vars <-
        fviz_mfa_var(
          mfa_geo_plot,
          choice = "quanti.var",
          palette = "lancet",
          repel = FALSE,
          geom = c("point", "text"),
          legend = "left"
        )
      fviz_mfa_ind(
        mfa_geo_plot,
        partial = "all",
        col.partial = "group",
        palette = "lancet"
      )
      fviz_mfa_axes(mfa_geo_plot,
                    repel = FALSE)
      
      # unsupervised cluster analysis, hence the negative one
      cluster_geo_ind <- FactoMineR::HCPC(mfa_geo_plot, nb.clust = -1)
      cluster_plot_ind <-
        fviz_cluster(cluster_geo_ind) + theme_ipsum()
      
      cluster_geo_var <-
        FactoMineR::HCPC(mfa_geo_plot$quanti.var, nb.clust = -1)
      cluster_plot_var <-
        fviz_cluster(cluster_geo_var) + theme_ipsum()
      fviz_dend(cluster_geo_ind)
      fviz_dend(cluster_geo_var)
      
    }
    
    # additional scatterplot comparison
    {
      rm(list = ls())
      library("tidyverse")
      library("dplyr")
      library("plotly")
      library("htmlwidgets")
      library("RPostgres")
      library("glue")
      library("hrbrthemes")
      library("viridis")
      library("countrycode")
      library("viridis")
      library("ggrepel")
      # Script settings for connecting to postgreSQL
      credentials <-
        readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
      crest_dbname =  credentials$crest_dbname[c(2)]
      crest_host = credentials$crest_host[c(2)]
      crest_port = 5432
      crest_user = credentials$crest_user[c(2)]
      crest_password = credentials$crest_password[c(2)]
      drv <- RPostgres::Postgres()
      connec <- dbConnect(
        drv,
        dbname = crest_dbname,
        host = crest_host,
        port = crest_port,
        user = crest_user,
        password = crest_password
      )
      
      table1 = 'PTMT_country'
      schema1 = 'mpho'
      query1 <-
        dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
      ptmt_geographic_coverage <- dbFetch(query1)
      ptmt_geographic_coverage$region_r <-
        str_trim(str_to_lower(ptmt_geographic_coverage$region_r))
      ptmt_geographic_coverage$ptmt_total <-
        as.numeric(apply(ptmt_geographic_coverage[, 3:55], 1, sum, na.rm = TRUE))
      ptmt_geographic_coverage <-
        ptmt_geographic_coverage %>% select(region_r, ptmt_total)
      colnames(ptmt_geographic_coverage)[colnames(ptmt_geographic_coverage) ==
                                           "region_r"] <- "region"
      ptmt_geographic_coverage$region[ptmt_geographic_coverage$region == "macedonia"] <-
        "north macedonia"
      ptmt_geographic_coverage$region[ptmt_geographic_coverage$region == "viet nam"] <-
        "vietnam"
      
      table2 = 'patstat_us_geographic_coverage'
      schema2 = 'mpho'
      query2 <-
        dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
      patstat_geographic_coverage <- dbFetch(query2)
      patstat_geographic_coverage$ip_type <-
        trimws(patstat_geographic_coverage$ip_type)
      patstat_geographic_coverage$country_full_name <-
        countrycode(patstat_geographic_coverage$person_ctry_code,
                    "iso2c",
                    "country.name")
      
      # add the country names for the country codes that were not matched but are recognised
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "AP"] <-
        "African Regional Industrial Property Organization"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "BX"] <-
        "Benelux Trademarks and Designs Office"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "EP"] <-
        "European Patent Organization"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "GC"] <-
        "Patent Office of the Cooperation Council for the Arab States of the Gulf (GCC)"
      patstat_geographic_coverage$person_ctry_code[patstat_geographic_coverage$person_ctry_code == "TS"] <-
        "TD"
      patstat_geographic_coverage$person_ctry_code[patstat_geographic_coverage$person_ctry_code == "UK"] <-
        "GB"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$person_ctry_code == "WO"] <-
        "World Intellectual Property Organization"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Congo - Brazzaville"] <-
        "Republic of Congo"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Bosnia & Herzegovina"] <-
        "bosnia and herzegovina"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "United States"] <-
        "usa"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Eswatini"] <-
        "swaziland"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Côte d’Ivoire"] <-
        "ivory coast"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "United Kingdom"] <-
        "uk"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Czechia"] <-
        "czech republic"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Myanmar (Burma)"] <-
        "myanmar"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Hong Kong SAR China"] <-
        "China"
      patstat_geographic_coverage$country_full_name[patstat_geographic_coverage$country_full_name == "Vatican City"] <-
        "Vatican"
      
      # change the country names to lower case to better match them for mapping
      patstat_geographic_coverage$country_full_name <-
        str_to_lower(patstat_geographic_coverage$country_full_name)
      colnames(patstat_geographic_coverage)[colnames(patstat_geographic_coverage) ==
                                              "country_full_name"] <- "region"
      # compile the dataframe for visualization
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>%
        filter (ip_type == "A" |
                  ip_type == "B1" |
                  ip_type == "B2") %>%
        dplyr::select(region, grant_year, n_grants)
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% 
        group_by(region) %>% 
        mutate(patstat_total = sum(n_grants))
      patstat_geographic_coverage$patstat_total <-
        as.numeric(patstat_geographic_coverage$patstat_total)
      patstat_geographic_coverage <-
        patstat_geographic_coverage %>% 
        select(region, patstat_total) %>% 
        filter(!duplicated(region))
      
      # patentsView dataset
      table3 = 'patentsview_geographic_coverage'
      schema3 = 'mpho'
      query3 <-
        dbSendQuery(connec, glue('select * from "{schema3}"."{table3}";'))
      patentsview_geographic_coverage <- dbFetch(query3)
      # get the country names using the country codes
      patentsview_geographic_coverage$country_full_name <-
        countrycode(patentsview_geographic_coverage$person_ctry_code,
                    "iso2c",
                    "country.name")
      colnames(patentsview_geographic_coverage)[colnames(patentsview_geographic_coverage) == "country_full_name"] =
        "region"
      colnames(patentsview_geographic_coverage)[colnames(patentsview_geographic_coverage) == "n_grants"] =
        "n_patents"
      colnames(patentsview_geographic_coverage)[colnames(patentsview_geographic_coverage) == "ip_wipo"] =
        "ip_type"
      patentsview_geographic_coverage$region <-
        str_to_lower(patentsview_geographic_coverage$region)
      patentsview_geographic_coverage$person_ctry_code <-
        str_to_lower(patentsview_geographic_coverage$person_ctry_code)
      patentsview_geographic_coverage$n_patents <-
        as.numeric(patentsview_geographic_coverage$n_patents)
      # change the person_ctry_codes to match the actual codes
      patentsview_geographic_coverage$region[patentsview_geographic_coverage$person_ctry_code == "usa"] <-
        "united states"
      patentsview_geographic_coverage$region[patentsview_geographic_coverage$person_ctry_code == "canada"] <-
        "ca"
      # change some country and country code names to match the mapdata region names
      patentsview_geographic_coverage[patentsview_geographic_coverage == "united states"] <-
        "usa"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "united kingdom"] <-
        "uk"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "congo - brazzaville"] <-
        "democratic republic of the congo"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "eswatini"] <-
        "swaziland"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "british virgin islands"] <-
        "virgin islands" # need to distinguih these by sub-region
      patentsview_geographic_coverage[patentsview_geographic_coverage == "u.s. virgin islands"] <-
        "virgin islands"
      patentsview_geographic_coverage[patentsview_geographic_coverage == "bosnia & herzegovina"] <-
        "bosnia and herzegovina"
      # filter out the utility patents
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>% filter (ip_type == 'A' |
                                                      ip_type == 'B1' | ip_type == 'B2')
      patentsview_geographic_coverage <-
        patentsview_geographic_coverage %>% group_by(region) %>% summarise(patentsview_total = sum(n_patents, na.rm = TRUE))
      patentsview_geographic_coverage[patentsview_geographic_coverage == 0] <-
        NA
      
      # join data sets
      geographic_coverage_comparison <-
        full_join(ptmt_geographic_coverage,
                  patstat_geographic_coverage,
                  by = "region")
      geographic_coverage_comparison <-
        full_join(geographic_coverage_comparison,
                  patentsview_geographic_coverage,
                  by = "region")
      
      # calculate correlation coefficients
      geographic_coverage_comparison_correlations <-
        cor(
          as.data.frame(geographic_coverage_comparison[, c(-1)]),
          use = "pairwise.complete.obs",
          method = "pearson"
        )
      
      # create comparative variables for patstat
      geographic_coverage_comparison$patstat_difference <-
        as.numeric(
          geographic_coverage_comparison$patstat_total - geographic_coverage_comparison$ptmt_total
        )
      geographic_coverage_comparison$patstat_percentage_difference <-
        as.numeric((
          (
            geographic_coverage_comparison$patstat_total - geographic_coverage_comparison$ptmt_total
          ) / geographic_coverage_comparison$ptmt_total
        ) * 100)
      geographic_coverage_comparison$patstat_difference_fold <-
        as.numeric(
          (
            geographic_coverage_comparison$patstat_total - geographic_coverage_comparison$ptmt_total
          ) / geographic_coverage_comparison$ptmt_total
        )
      # create comparative variables for patentsview
      geographic_coverage_comparison$patentsview_difference <-
        as.numeric(
          geographic_coverage_comparison$patentsview_total - geographic_coverage_comparison$ptmt_total
        )
      geographic_coverage_comparison$patentsview_percentage_difference <-
        as.numeric((
          (
            geographic_coverage_comparison$patentsview_total - geographic_coverage_comparison$ptmt_total
          ) / geographic_coverage_comparison$ptmt_total
        ) * 100)
      geographic_coverage_comparison$patentsview_difference_fold <-
        as.numeric(
          (
            geographic_coverage_comparison$patentsview_total - geographic_coverage_comparison$ptmt_total
          ) / geographic_coverage_comparison$ptmt_total
        )
      
      # remove unidentified countries
      geographic_coverage_comparison <-
        geographic_coverage_comparison %>% filter(region != "")
      
      # major breaks for the plot axes
      my_breaks_x <- round(seq(
        from = min(
          geographic_coverage_comparison$patstat_difference_fold,
          na.rm = TRUE
        ),
        to = max(
          geographic_coverage_comparison$patstat_difference_fold,
          na.rm = TRUE
        ),
        length = 10
      ),
      digits = 0)
      my_breaks_y <- round(seq(
        from = min(
          geographic_coverage_comparison$patentsview_difference_fold,
          na.rm = TRUE
        ),
        to = max(
          geographic_coverage_comparison$patentsview_difference_fold,
          na.rm = TRUE
        ),
        length = 10
      ),
      digits = 0)
      
      # full scatterplot
      geo_cov_compare_scatter_full <-
        ggplot(
          geographic_coverage_comparison,
          aes(x = patstat_difference_fold,
              y = patentsview_difference_fold)
        ) +
        geom_point(colour =  "maroon") +
        geom_text_repel(
          label = geographic_coverage_comparison$region,
          max.overlaps = 100,
          size = 4
        ) +
        theme(
          axis.text.x = element_text(
            angle = 0,
            hjust = 1.0,
            vjust = 1.0,
            size = 20
          ),
          axis.text.y = element_text(
            angle = 0,
            hjust = 1.0,
            vjust = 1.0,
            size = 20
          ),
          axis.title.x = element_text(
            hjust = 0.5,
            vjust = -1.0,
            size = 20
          ),
          axis.title.y = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 20
          ),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_line(colour = "grey50", linewidth = 0.1),
          panel.border = element_rect(linetype = "solid", fill = NA),
          panel.ontop = TRUE
        ) +
        scale_y_continuous(breaks = my_breaks_y) +
        scale_x_continuous(breaks = my_breaks_x)
      
      #The plot with a zoom
      geo_cov_compare_scatter_zoom <-
        ggplot(
          geographic_coverage_comparison,
          aes(x = patstat_difference_fold,
              y = patentsview_difference_fold)
        ) +
        geom_point(colour = "maroon") +
        geom_text_repel(
          label = geographic_coverage_comparison$region,
          max.overlaps = 1000,
          size = 4
        ) +
        theme(
          axis.text.x = element_text(
            angle = 0,
            hjust = 1.0,
            vjust = 1.0,
            size = 20
          ),
          axis.text.y = element_text(
            angle = 0,
            hjust = 1.0,
            vjust = 1.0,
            size = 20
          ),
          axis.title.x = element_text(
            hjust = 0.5,
            vjust = -1.0,
            size = 20
          ),
          axis.title.y = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 20
          ),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_line(colour = "transparent", linewidth = 0),
          panel.border = element_rect(linetype = "solid", fill = NA),
          panel.ontop = TRUE
        ) +
        scale_y_continuous(breaks = my_breaks_y) +
        scale_x_continuous(breaks = my_breaks_x) +
        xlim (-1, 4.4) +
        ylim (-1, 4.7) +
        labs(x = NULL, y = NULL)
      
      geo_cov_compare_scatter <- geo_cov_compare_scatter_full +
        annotation_custom(
          ggplotGrob(geo_cov_compare_scatter_zoom),
          # dimension of the below rectangle that the zoom plot will end up in
          xmin = -4,
          xmax = 13,
          ymin = 12,
          ymax = 28
        ) +
        # the dimensions of the triangle around which the zoom will end up
        geom_rect(
          aes(
            xmin = -4,
            xmax = 13,
            ymin = 12,
            ymax = 28
          ),
          color = 'black',
          linetype = 'dashed',
          alpha = 0
        ) +
        geom_rect(
          aes(
            xmin = -4,
            xmax = 5,
            ymin = -4,
            ymax = 5
          ),
          color = 'black',
          linetype = 'dashed',
          alpha = 0
        ) +
        geom_path(aes(x, y, group = grp),
                  data = data.frame(
                    x = c(0, 1, 0, 1),
                    y = c(5, 12, 5, 12),
                    grp = c(1, 1, 2, 2)
                  ),
                  linetype = 'dashed')
      ggsave(
        "geographic_coverage_comparison_scatterplot.jpeg",
        plot = geo_cov_compare_scatter,
        width = 30,
        height = 20
        # dpi = 700
      )
      browseURL("geographic_coverage_comparison_scatterplot.jpeg")
      
    }
  }
  
  # ownership coverage
  {
    rm(list = ls())
    library("tidyverse")
    library("plotly")
    library("htmlwidgets")
    library("RPostgres")
    library("hrbrthemes")
    library("glue")
    library("viridis")
    library("ggrepel")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    # PTMT DATA
    table1 = 'PTMT_patent_owners'
    schema1 = 'mpho'
    query1 <-
      dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
    ptmt_owner_coverage <- dbFetch(query1)
    colnames(ptmt_owner_coverage)[colnames(ptmt_owner_coverage) == " First-Named Assignee "] <-
      "ptmt_owner"
    ptmt_owner_coverage$ptmt_owner <-
      trimws(ptmt_owner_coverage$ptmt_owner)
    ptmt_owner_coverage <- ptmt_owner_coverage %>% pivot_longer(cols = c(2:48),
                                                                names_to = 'year',
                                                                values_to = 'ptmt_n_patents')
    ptmt_owner_coverage <-
      ptmt_owner_coverage %>% filter(ptmt_owner != "INDIVIDUALLY OWNED PATENT")
    ptmt_owner_coverage <-
      ptmt_owner_coverage %>% select(year, ptmt_owner, ptmt_n_patents)
    ptmt_owner_coverage$ptmt_owner <-
      str_to_upper(ptmt_owner_coverage$ptmt_owner)
    ptmt_owner_coverage$year <- as.numeric(ptmt_owner_coverage$year)
    ptmt_owner_coverage$ptmt_n_patents <-
      as.numeric(ptmt_owner_coverage$ptmt_n_patents)
    ptmt_owner_coverage[ptmt_owner_coverage == 0] <- NA
    # create yearly totals
    # count the number of owners with a patents that year
    ptmt_owner_coverage <- ptmt_owner_coverage %>%
      group_by(year) %>%
      mutate(
        ptmt_year_total = sum(ptmt_n_patents,
                              na.rm = TRUE),
        ptmt_n_owners = sum(!is.na(ptmt_n_patents))
      )
    # select the year totals and owner totals
    ptmt_owner_coverage <- ptmt_owner_coverage %>%
      select(year, ptmt_year_total, ptmt_n_owners) %>%
      filter(!duplicated(year))
    
    
    # PATSTAT DATA
    # applicants
    table2 = 'patstat_us_owner_coverage'
    schema2 = 'mpho'
    query2 <-
      dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
    patstat_appln_coverage <- dbFetch(query2)
    # select only utility patent types
    patstat_appln_coverage <-
      patstat_appln_coverage %>% filter(ip_type == 'A ' |
                                          ip_type == 'B1' | ip_type == 'B2')
    colnames(patstat_appln_coverage)[colnames(patstat_appln_coverage) ==
                                       "person_name"] <- "patstat_appln"
    colnames(patstat_appln_coverage)[colnames(patstat_appln_coverage) ==
                                       "grant_year"] <- "year"
    colnames(patstat_appln_coverage)[colnames(patstat_appln_coverage) ==
                                       "n_grants"] <- "patstat_appln_n_patents"
    patstat_appln_coverage$patstat_appln <-
      str_to_upper(patstat_appln_coverage$patstat_appln)
    patstat_appln_coverage$year <-
      as.numeric(patstat_appln_coverage$year)
    # patstat_appln_coverage <- patstat_appln_coverage %>% filter( patstat_appln != "UNKNOWN" & patstat_appln != "" )
    patstat_appln_coverage$patstat_appln_n_patents <-
      as.numeric(patstat_appln_coverage$patstat_appln_n_patents)
    # sum over the patent types
    patstat_appln_coverage <-
      patstat_appln_coverage %>% group_by (year, patstat_appln) %>% mutate(patstat_appln_n_patents = sum(patstat_appln_n_patents))
    patstat_appln_coverage <-
      patstat_appln_coverage %>% select(year, patstat_appln, patstat_appln_n_patents) %>% filter(!duplicated(year))
    # filter for the year before creating the total count
    patstat_appln_coverage <-
      patstat_appln_coverage %>% filter(year >= 1969 &
                                          year <= 2015)
    patstat_appln_coverage <-
      patstat_appln_coverage %>% group_by(patstat_appln) %>% mutate(total = sum(patstat_appln_n_patents))
    patstat_appln_coverage[patstat_appln_coverage == 0] <- NA
    patstat_appln_coverage <-
      patstat_appln_coverage %>% select(year, patstat_appln, patstat_appln_n_patents)
    # create yearly totals
    # count the number of owners with a patents that year
    patstat_appln_coverage <-
      patstat_appln_coverage %>%
      group_by(year) %>%
      mutate(
        patstat_appln_year_total = sum(patstat_appln_n_patents,
                                       na.rm = TRUE),
        patstat_n_applns = sum(!is.na(patstat_appln_n_patents))
      )
    # select the year totals and owner totals
    patstat_appln_coverage <- patstat_appln_coverage %>%
      select(year, patstat_appln_year_total, patstat_n_applns) %>%
      filter(!duplicated(year))
    
    # owners
    table2 = 'patstat_us_owner_legal'
    schema2 = 'mpho'
    query2 <-
      dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
    patstat_owner_coverage <- dbFetch(query2)
    # select only utility patent types
    patstat_owner_coverage <-
      patstat_owner_coverage %>% filter(ip_type == 'A ' |
                                          ip_type == 'B1' | ip_type == 'B2')
    colnames(patstat_owner_coverage)[colnames(patstat_owner_coverage) ==
                                       "owner"] <- "patstat_owner"
    colnames(patstat_owner_coverage)[colnames(patstat_owner_coverage) ==
                                       "n_grants"] <- "patstat_n_patents"
    patstat_owner_coverage$patstat_owner <-
      str_to_upper(patstat_owner_coverage$patstat_owner)
    patstat_owner_coverage$year <-
      as.numeric(patstat_owner_coverage$year)
    patstat_owner_coverage <-
      patstat_owner_coverage %>% filter(patstat_owner != "UNKNOWN" &
                                          patstat_owner != "")
    patstat_owner_coverage$patstat_n_patents <-
      as.numeric(patstat_owner_coverage$patstat_n_patents)
    # sum over the patent types
    patstat_owner_coverage <-
      patstat_owner_coverage %>% group_by (year, patstat_owner) %>% mutate(patstat_n_patents = sum(patstat_n_patents))
    patstat_owner_coverage <-
      patstat_owner_coverage %>% select(year, patstat_owner, patstat_n_patents) %>% filter(!duplicated(year))
    # filter the years before calculating total
    patstat_owner_coverage <-
      patstat_owner_coverage %>% filter(year >= 1969 & year <= 2015)
    patstat_owner_coverage <-
      patstat_owner_coverage %>% group_by(patstat_owner) %>% mutate(total = sum(patstat_n_patents))
    patstat_owner_coverage <-
      patstat_owner_coverage %>% filter(total >= 100)
    patstat_owner_coverage[patstat_owner_coverage == 0] <- NA
    patstat_owner_coverage <-
      patstat_owner_coverage %>% select(year, patstat_owner, patstat_n_patents)
    # create yearly totals
    # count the number of owners with a patents that year
    patstat_owner_coverage <-
      patstat_owner_coverage %>%
      group_by(year) %>%
      mutate(
        patstat_n_owners = sum(!is.na(patstat_n_patents)),
        patstat_owner_year_total = sum(patstat_n_patents, na.rm = TRUE)
      )
    # select the year totals and owner totals
    patstat_owner_coverage <- patstat_owner_coverage %>%
      select(year, patstat_owner_year_total, patstat_n_owners) %>%
      filter(!duplicated(year))
    
    # PATENTSVIEW DATA
    table3 = 'patentsview_assignee_coverage'
    schema3 = 'mpho'
    query3 <-
      dbSendQuery(connec, glue('select * from "{schema3}"."{table3}";'))
    patentsview_owner_coverage <- dbFetch(query3)
    patentsview_owner_coverage <-
      patentsview_owner_coverage %>% filter(organization != "") %>% select(grant_year, organization, ip_type, n_grants)
    # change column names to match the other datasets
    colnames(patentsview_owner_coverage)[colnames(patentsview_owner_coverage) ==
                                           "grant_year"] <- "year"
    colnames(patentsview_owner_coverage)[colnames(patentsview_owner_coverage) ==
                                           "organization"] <- "patentsview_owner"
    colnames(patentsview_owner_coverage)[colnames(patentsview_owner_coverage) ==
                                           "n_grants"] <- "patentsview_n_patents"
    patentsview_owner_coverage$patentsview_n_patents <-
      as.numeric(patentsview_owner_coverage$patentsview_n_patents)
    # get only utility patent grants
    patentsview_owner_coverage <-
      patentsview_owner_coverage %>% filter(ip_type == "A" |
                                              ip_type == "B1" | ip_type == "B2")
    # changing to upper case (lower case would work too) will remove case-based duplicates
    patentsview_owner_coverage$patentsview_owner <-
      str_to_upper(patentsview_owner_coverage$patentsview_owner)
    patentsview_owner_coverage <-
      patentsview_owner_coverage  %>% select(year, patentsview_owner, patentsview_n_patents)
    # filter before calculating total
    patentsview_owner_coverage <-
      patentsview_owner_coverage %>% filter(year >= 1969 &
                                              year <= 2015)
    patentsview_owner_coverage <-
      patentsview_owner_coverage %>% group_by(patentsview_owner) %>% mutate(total = sum(patentsview_n_patents))
    patentsview_owner_coverage <-
      patentsview_owner_coverage %>% filter(total >= 100)
    patentsview_owner_coverage[patentsview_owner_coverage == 0] <-
      NA
    patentsview_owner_coverage <-
      patentsview_owner_coverage %>% select(year, patentsview_owner, patentsview_n_patents)
    # create yearly totals
    # count the number of owners with a patents that year
    patentsview_owner_coverage <-
      patentsview_owner_coverage %>% group_by(year) %>%
      mutate(
        patentsview_n_owners = sum(!is.na(patentsview_n_patents)),
        patentsview_year_total = sum(patentsview_n_patents, na.rm = TRUE)
      )
    # select the year totals and owner totals
    patentsview_owner_coverage <- patentsview_owner_coverage %>%
      select(year, patentsview_year_total, patentsview_n_owners) %>%
      filter(!duplicated(year))
    
    # combine the datasets into a comparison dataframe
    comparison_ownership <-
      full_join(ptmt_owner_coverage, patstat_owner_coverage, by = "year")
    comparison_ownership <-
      full_join(comparison_ownership, patentsview_owner_coverage, by = "year")
    comparison_ownership <-
      full_join(comparison_ownership, patstat_appln_coverage, by = "year")
    comparison_ownership[is.na(comparison_ownership) == TRUE] <- 0
    writexl::write_xlsx(
      x = as.data.frame(comparison_ownership),
      path = "comparison_ownership.xlsx",
      col_names = TRUE
    )
    # calculate correlation coefficients
    comparison_ownership_correlations <-
      cor(as.data.frame(comparison_ownership[, c(-1)]),
          use = "pairwise.complete.obs",
          method = "pearson")
    
    # create comparison values
    comparison_ownership$patstat_owner_diff <-
      comparison_ownership$patstat_n_owners - comparison_ownership$ptmt_n_owners
    comparison_ownership$patstat_owner_diff_perc <-
      (comparison_ownership$patstat_owner_diff / comparison_ownership$ptmt_n_owners) *
      100
    comparison_ownership$patstat_appln_diff <-
      comparison_ownership$patstat_n_applns - comparison_ownership$ptmt_n_owners
    comparison_ownership$patstat_appln_diff_perc <-
      (comparison_ownership$patstat_appln_diff / comparison_ownership$ptmt_n_owners) *
      100
    comparison_ownership$patentsview_diff <-
      comparison_ownership$patentsview_n_owners - comparison_ownership$ptmt_n_owners
    comparison_ownership$patentsview_diff_perc <-
      (comparison_ownership$patentsview_diff / comparison_ownership$ptmt_n_owners) *
      100
    # pivot and subset the data
    comparison_ownership <-
      comparison_ownership %>% pivot_longer(!year, names_to = "comparison", values_to = "comparison_value")
    # year totals and number of owners
    comparison_ownership_diff_perc <-
      comparison_ownership %>% filter(
        comparison == "patstat_owner_diff_perc" |
          comparison == "patstat_appln_diff_perc" |
          comparison == "patentsview_diff_perc"
      )
    comparison_ownership_year_totals <- comparison_ownership %>%
      filter(
        comparison == "ptmt_year_total" |
          comparison == "patstat_appln_year_total" |
          comparison == "patstat_owner_year_total" |
          comparison == "patentsview_year_total"
      )
    comparison_ownership_n_owners <- comparison_ownership  %>%
      filter(
        comparison == "ptmt_n_owners" |
          comparison == "patstat_n_owners" |
          comparison == "patstat_n_applns" |
          comparison == "patentsview_n_owners"
      )
    
    # linegraph
    ownership_datasets <- c("comparison_ownership_year_totals",
                            "comparison_ownership_n_owners")
    plot_list <- list()
    for (i in 1:length(ownership_datasets)) {
      comparison_ownership_linegraph_i <-
        ggplot(
          get(ownership_datasets[[i]]),
          aes(
            x = year,
            y = comparison_value,
            colour = reorder(comparison,-comparison_value),
            fill = reorder(comparison,-comparison_value),
            group = reorder(comparison,-comparison_value)
          )
        ) +
        geom_line(linewidth = 0.5) +
        geom_point(size = 2.5, shape = 21) +
        theme_ipsum(base_family = "Arial", axis_text_size = 18) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 0.5,
            vjust = 0.5
          ),
          axis.title.x = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.title.y = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 16
          ),
          axis.ticks.length.x = unit(0, "cm"),
          axis.ticks.length.y = unit(0, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.1, "cm"),
          legend.direction = "vertical",
          legend.position = c(0.2, 0.62),
          legend.box.background = element_rect(
            fill = 'white',
            colour = 'black',
            linewidth = 0.1
          ),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)
        ) +
        scale_colour_viridis(
          aesthetics = c("fill", "colour"),
          discrete = TRUE,
          option = "C",
          direction = 1,
          begin = 0,
          end = 0.7
        ) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = seq(
          from = 1969,
          to = 2015,
          by = 5
        ),
        minor_breaks = 1) +
        labs(
          x = "Year",
          y = NULL,
          colour = "data source",
          fill = "data source"
        )
      ggsave(
        glue("comparison_owners_{ownership_datasets[[i]]}.svg"),
        plot = comparison_ownership_linegraph_i,
        width = 30,
        height = 15,
        units = 'cm',
        dpi = 720
      )
      plot_list[[i]] <- comparison_ownership_linegraph_i
    }
    grid_plot <-
      gridExtra::grid.arrange(grobs = plot_list,
                              ncol = 1,
                              padding = unit(0.5, "line"))
    ggsave(
      "comparison_owners_grid_plot.svg",
      plot = grid_plot,
      width = 35,
      height = 30,
      units = 'cm',
      
      dpi = 720
    )
    browseURL("comparison_owners_grid_plot.svg")
  }
  
  # technology coverage
  {
    rm(list = ls())
    library("tidyverse")
    library("plotly")
    library("htmlwidgets")
    library("RPostgres")
    library("hrbrthemes")
    library("glue")
    library("viridis")
    library("ggrepel")
    library("stringr")
    # Script settings for connecting to postgreSQL
    credentials <-
      readxl::read_xlsx("/Users/mphomafata/Documents/Work_file/CREST Postdoc/my_credentials.xlsx")
    crest_dbname =  credentials$crest_dbname[c(2)]
    crest_host = credentials$crest_host[c(2)]
    crest_port = 5432
    crest_user = credentials$crest_user[c(2)]
    crest_password = credentials$crest_password[c(2)]
    drv <- RPostgres::Postgres()
    connec <- dbConnect(
      drv,
      dbname = crest_dbname,
      host = crest_host,
      port = crest_port,
      user = crest_user,
      password = crest_password
    )
    # PATSTAT COVERAGE
    table1 = 'patstat_us_technology_coverage'
    schema1 = 'mpho'
    query1 <-
      dbSendQuery(connec, glue('select * from "{schema1}"."{table1}";'))
    patstat_tech_coverage <- dbFetch(query1)
    patstat_tech_coverage$ip_type <-
      trimws(patstat_tech_coverage$ip_type)
    patstat_tech_coverage$n_grants <-
      as.numeric(patstat_tech_coverage$n_grants)
    patstat_tech_coverage$cpc_class <-
      str_replace_all(patstat_tech_coverage$cpc_class, fixed(" "), "")
    patstat_tech_coverage <-
      patstat_tech_coverage %>% filter(ip_type == "A" |
                                         ip_type == "B1" |
                                         ip_type == "B2") %>% select(grant_year,
                                                                     cpc_class,
                                                                     n_grants)
    colnames(patstat_tech_coverage)[colnames(patstat_tech_coverage) == "n_grants"] <-
      "patstat_n_patents"
    patstat_tech_coverage <-
      patstat_tech_coverage %>% group_by(grant_year,
                                         cpc_class) %>% mutate(patstat_n_patents = sum(patstat_n_patents))
    patstat_tech_coverage <-
      patstat_tech_coverage %>% filter(!duplicated(grant_year))
    
    # PATENTSVIEW COVERAGE
    table2 = 'patentsview_technology_coverage'
    schema2 = 'mpho'
    query2 <-
      dbSendQuery(connec, glue('select * from "{schema2}"."{table2}";'))
    patentsview_tech_coverage <- dbFetch(query2)
    patentsview_tech_coverage$ip_type <-
      trimws(patentsview_tech_coverage$ip_type)
    patentsview_tech_coverage$n_patents <-
      as.numeric(patentsview_tech_coverage$n_patents)
    patentsview_tech_coverage <-
      patentsview_tech_coverage %>%
      filter(ip_type == "A" |
               ip_type == "B1" |
               ip_type == "B2") %>%
      select(grant_year,
             level_1,
             n_patents)
    colnames(patentsview_tech_coverage)[colnames(patentsview_tech_coverage) ==
                                          "level_1"] <- "cpc_class"
    colnames(patentsview_tech_coverage)[colnames(patentsview_tech_coverage) ==
                                          "n_patents"] <- "patentsview_n_patents"
    patentsview_tech_coverage <-
      patentsview_tech_coverage %>%
      group_by(grant_year, cpc_class) %>%
      mutate(patentsview_n_patents = sum(patentsview_n_patents))
    patentsview_tech_coverage <- patentsview_tech_coverage %>%
      filter(!duplicated(grant_year))
    comparison_tech <- merge(
      x = patstat_tech_coverage,
      y = patentsview_tech_coverage,
      by = c("grant_year", "cpc_class")
    )
    
    # calculate correlation coefficients
    comparison_tech$corr_coeff <- comparison_tech %>%
      group_by(cpc_class) %>%
      mutate(corr_coeff = cor(patstat_n_patents, patentsview_n_patents))
    cor(
      x = comparison_tech$patstat_n_patents,
      y = comparison_tech$patentsview_n_patents,
      method = "pearson",
      use = "all.obs"
    )
    
    # three-dimensional scatterplot
      tech_cov_compare <- plot_ly(
        data = comparison_tech,
        type = 'scatter3d',
        x = ~ grant_year,
        y = ~ patstat_n_patents,
        z = ~ patentsview_n_patents,
        mode = "markers",
        size = log(comparison_tech$corr_coeff$corr_coeff),
        showlegend = TRUE,
        color = ~ cpc_class,
        colors = "Dark2",
        text = ~ cpc_class
      ) %>%
        layout(
          legend = list(title = list(text = 'CPC class')),
          scene = list(
            xaxis = list(title = 'Grant year'),
            yaxis = list(title = 'Number of Patstat grants'),
            zaxis = list(title = 'Number of PatentsView grants')
          )
        )
      saveWidget(tech_cov_compare,  "tech_cov_compare.html")
      browseURL("tech_cov_compare.html")
    
  }
}

# for reporting purposes
# get the versions of R, the loaded packages, and their versions
sessionInfo()
