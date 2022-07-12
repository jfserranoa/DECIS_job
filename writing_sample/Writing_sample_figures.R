library(tidyverse)


df_COVID <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

head(df_COVID)

df_latam <- df_COVID %>% 
              filter(iso_code =="BRA") %>% 
              select('date',
                     'new_cases_smoothed_per_million',
                     'new_deaths_smoothed_per_million')

colnames(df_COVID)

df_latam %>% 
  ggplot(aes(x = date)) +
    geom_line(aes(y = new_cases_smoothed_per_million,
                  color = 'Cases'),
              size = 2,
              alpha = 0.8,
              linetype = 1) +
    geom_line(aes(y = new_deaths_smoothed_per_million*10,
                  color = 'Deaths (RHS)'),
              size = 2,
              alpha = 0.5,
              linetype = 2) +
    scale_y_continuous(sec.axis = sec_axis(~. / 10,
                                           name = 'Deaths per Million (smoothed)')) +
    scale_color_manual(name = '',
                       breaks = c('Cases', 'Deaths (RHS)'),
                       values = c('Cases' = '#003399', 'Deaths (RHS)' = '#0099FF')) +
    theme_minimal() +
    labs(title = 'COVID-19 in BRAZIL',
         subtitle = 'Daily New Confirmed COVID-19 Cases and Deaths per Million People',
         y = 'Cases per million (smoothed)',
         x = '') +
    theme(
      plot.title = element_text(face='bold', size = 20),
      plot.subtitle = element_text(size = 16),
      axis.title.y = element_text(size=14),
      axis.title.y.right = element_text(size=14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.position = c(0.3,0.8)
    )


#ggsave('C:/Users/Juan Felipe/OneDrive - The University of Chicago/Desktop/BRA_Covid.jpeg')

################################################################################
########################## FISCAL MONITOR ######################################
################################################################################


path <- "C:/Users/Juan Felipe/OneDrive - The University of Chicago/Desktop/Shiny_practice/00_Documents"
fiscal_df <- read_csv(paste0(path,"/","Brazil.csv"))

fiscal_df <- as.data.frame(t(as.matrix(fiscal_df)))

colnames(fiscal_df) <- fiscal_df[1,]#assign first row as column names
fiscal_df <- fiscal_df[-1,]

fiscal_df <- as.data.frame(apply(fiscal_df, 2, as.numeric))

fiscal_df <- fiscal_df %>% 
                mutate(year = rep(1997:2028, each = 1),
                       color = ifelse(year>2020,"f","r")) %>%
                select(c(9, 1, 7, 8, 10))

colnames(fiscal_df) <- c('year','fiscal_balance','gross_debt','net_debt','color')


fiscal_df <- fiscal_df[complete.cases(fiscal_df),]


g <- theme(plot.title = element_text(face='bold', size = 20),
           plot.subtitle = element_text(size = 16),
           axis.title.y = element_text(size=14),
           axis.title.y.right = element_text(size=14),
           axis.text.x = element_text(size = 14),
           axis.text.y = element_text(size = 14),
           legend.position = 'none')


group.colors <- c(f = '#0099FF', r ='#003399')


## Fiscal Balance Figure

fiscal_df %>% filter(year>2015) %>% 
  ggplot(aes(x=year, y=fiscal_balance,
             group=1,
             color=color)) +
    geom_point(size=3)+
    geom_line(size=2)+
    scale_color_manual(values = group.colors) +
    scale_x_continuous(breaks = seq(2015,2027,3)) +
    scale_y_continuous(limits = c(-15,0)) +
    annotate('rect',
             xmin = 2021, xmax = 2027,
             ymin = -15, ymax = 0,
             alpha = 0.2, fill='#999CCC') +
    theme_minimal() +
    labs(title = 'Brazil Overall Balance',
         subtitle = 'Percent of GDP',
         y = '(%)',
         x = "") +
    g

# ggsave('C:/Users/Juan Felipe/OneDrive - The University of Chicago/Desktop/BRA_fiscalB.jpeg')
              

## Gross Debt Figure

fiscal_df %>% filter(year>2015) %>% 
  ggplot(aes(x=year, y=gross_debt,
             group=1,
             color=color)) +
  geom_point(size=3)+
  geom_line(size=2)+
  scale_color_manual(values = group.colors) +
  scale_x_continuous(breaks = seq(2015,2027,3)) +
  scale_y_continuous(limits = c(70,105)) +
  annotate('rect',
           xmin = 2021, xmax = 2027,
           ymin = 70, ymax = 105,
           alpha = 0.2, fill='#999CCC') +
  theme_minimal() +
  labs(title = 'Brazil Gross Debt',
       subtitle = 'Percent of GDP',
       y = '(%)',
       x = "") +
  g

# ggsave('C:/Users/Juan Felipe/OneDrive - The University of Chicago/Desktop/BRA_Debt.jpeg')
