library(plotly)
library(dplyr)
library(readr)
library(WDI)
library(htmlwidgets)  #interactive map labels

#Read in data from World Development Indicators
indicators <- c('population_growth' = 'SP.POP.GROW')

df <- WDI(country = "all",
          indicator = indicators,
          start = 1990,
          end = 2020,
          extra = TRUE) %>%
      select(-c('status', 'lastupdated', 'lending'))

# Create a variables with the text per country
df <- df %>% 
        mutate(hover = paste0(country, '\n',
                              'region: ', region, '\n',
                              'capital: ',capital,'\n',
                               round(population_growth, 2), '%'))


min <- min(df$population_growth, na.rm=TRUE)
max <- max(df$population_growth, na.rm=TRUE)


fontStyle = list(family = 'DM Sans',
                 size = 15,
                 color = 'black')

label = list(bgcolor = '#EEEEEE',
             bordercolor = 'transparent',
             font = fontStyle)


g <- list(scope = 'world',
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator'))


ind_graph <- plot_geo(df,
                      locationmode = 'world',
                      frame = ~year) %>% 
             add_trace(locations = ~iso3c,
                       z = ~population_growth,
                       zmin = min,
                       zmax = max,
                       color = ~population_growth,
                       colors = 'RdBu',
                       text = ~hover,
                       hoverinfo = 'text') %>% 
             layout(geo = g,
                    font = list(family = 'DM Sans'),
                    title = list(
                              text ='<b>Global Population Growth</b>\n1990 - 2020',
                              font = 14,
                              y = 0.3)) %>% 
             style(hoverlabel = label) %>% 
             config(displayModeBar = FALSE) %>% 
             colorbar(ticksuffix = "%",
                      tickwidth = 0.5,
                      title = " ",
                      nticks = 3,
                      thickness = 15)
ind_graph




path <- "C:/Users/Juan Felipe/OneDrive - The University of Chicago/Desktop/Shiny_practice/00_Documents/"

#saveWidget(ind_graph, paste0(path, 'world_map.html'))
