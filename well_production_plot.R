library(tidyverse)
options(scipen = 999)

prod <- read.csv('pool6_production.txt', header = T)
loglines <- read.csv('loglinesOriginal.csv', header = T)
well_hdr <- read.csv('petra_hdr.csv', header = T)

prod <- prod %>% select(API = API.Number, Date = Time, Gas = Op.Gas.Rate,
                        Water = Op.Water.Rate)

# doctor
prod$Date <- as.Date(substr(prod$Date, 1, 10), '%m/%d/%Y')
prod$API <- substr(prod$API, 1, 12)
prod[is.na(prod)] <- 0

# cumsums
prod <- prod %>% group_by(API) %>% mutate(Gp = cumsum(Gas)/10**6,
                                          Wp = cumsum(Water)/10**3)

# to long
prod <- reshape2::melt(prod, value.name = 'Rate', 
                       id = c('API', 'Date'),
                       na.rm = FALSE)

names(prod)[3] <- 'Stream'
prod <- prod %>%
  mutate(plot_labs = Stream) %>%
  mutate(plot_labs = str_replace(plot_labs, 'Gas', 'Gas (MCFD)')) %>%
  mutate(plot_labs = str_replace(plot_labs, 'Water', 'Water (BWPD)')) %>%
  mutate(plot_labs = str_replace(plot_labs, 'Gp', 'Gp (MMCF)')) %>%
  mutate(plot_labs = str_replace(plot_labs, 'Wp', 'Wp (MBW)')) 

prod$plot_labs <- factor(prod$plot_labs, 
                         levels = c('Wp (MBW)','Gp (MMCF)',
                                    'Water (BWPD)', 'Gas (MCFD)'))

# list of wells
apis <- unique(prod$API)

for(i in 1:length(apis)) {
  print(i)
  df <- prod[prod$API == apis[i], ]
  
  # plot title
  well_api <- well_hdr[i, 'API']
  well_well <- well_hdr[i, 'Well']
  startDate <- min(df$Date)
  stopDate <- max(df$Date)
  months_on <- round(as.numeric(stopDate - startDate)/30, 1)
  years_on <- round(months_on/12, 1)
  totals <- df %>% group_by(Stream) %>% do(tail(., n=1))
  gp <- format(as.numeric(round(totals[3, 'Rate'], 3)), big.mark = ',')
  wp <- format(as.numeric(round(totals[4, 'Rate'], 1)), big.mark = ',')
  well_title <- paste0(' ', well_well, ' (Sterling 6) \n ', 
                       'Start Date: ', startDate, ', ',
                       'Last Date: ', stopDate, '\n ',
                       'Months: ', months_on, ', Years: ', years_on,'\n ',
                       'Gp (bcf): ', gp, ', Wp (mbw): ', wp)
  
  # month scale for plot
  if(months_on < 12) date_scale <- '1 months'
  if(between(months_on, 12, 23.9)) date_scale <- '2 months'
  if(between(months_on, 24, 59.9)) date_scale <- '6 months'
  if(between(months_on, 60, 119.9)) date_scale <- '1 years'
  if(between(months_on, 120, 239.9)) date_scale <- '2 years'
  if(between(months_on, 240, 1000)) date_scale <- '5 years'
  
  
  
  p <- ggplot(df, aes(Date, Rate)) + 
    geom_line(aes(color = plot_labs), size = 1) + 
    scale_color_manual(values = c('purple', 'black', 'blue', 'red')) + 
    scale_y_log10(breaks = loglines[[1]], 
                  labels = loglines[[2]],
                  limits = c(0.1, NA)) + 
    scale_x_date(date_breaks = date_scale, date_labels = '%b-%y') +
    ggtitle(well_title) +
    xlab(' \n') + 
    ylab('Rate \n') +
    theme_bw() +
    theme(
      # text
      axis.text.x = element_text(size = 12, face = 'bold', color = 'black',
                                 angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
      axis.title.x = element_text(size = 14, face = 'bold', color = 'black'),
      axis.title.y = element_text(size = 14, face = 'bold', color = 'black'),
      legend.text = element_text(size = 12, face = 'bold', color = 'black'),
      plot.title = element_text(size = 10, face = 'bold', color = 'black'),
      # legend
      legend.background = element_rect(color = 'black'), 
      legend.title = element_blank(),
      legend.position = 'bottom', #c(.1,.1), 
      # plot
      plot.margin = margin(c(.25,.25,.25,.25), unit = 'in'),
      # grid 
      panel.grid.major.y = element_line(color = 'grey50'),
      panel.grid.major.x = element_line(color = 'grey50'),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  pdf(file = paste0('./prod/',well_well,'.pdf'), height = 11, width = 17)
    print(p)
  dev.off()
}