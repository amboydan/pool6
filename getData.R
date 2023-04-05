library(dplyr); library(tidyverse)

q <- "SELECT wi.WELL_NAME as name, wi.SANDFACE_LATITUDE as lat, wi.SANDFACE_LONGITUDE as lng,
        cast(pr.DATE_TIME as date) as dt, pr.TUBING_PRESSURE as tbg
        
        FROM WELL_INFORMATION as wi
        inner join
        PRODUCTION_HISTORY as pr
        ON wi.WELL_KEY = pr.WELL_KEY
        
        where 
        wi.FIELD_NAME = 'Kenai Gas Storage - 50.0672'
        and
        GAS_PRODUCTION_VOLUME = 0
        and
        GAS_INJECTION_VOLUME = 0
        and
        TUBING_PRESSURE between 120 and 240 
		    and
		    wi.SANDFACE_LATITUDE is not null
		    and
		    wi.SANDFACE_LONGITUDE is not null
		    and
  		  pr.DATE_TIME >= '2020-01-01'
  		  and
  		  wi.WELL_NAME <> 'GS KU 021-006RD Inj Sterling PA S6'
        "
  
# Open up the db.
str <- 'driver={SQL Server};server=ancsql04;database=Gas_Forecasting_Sandbox;trusted_connection=true'

# query
makeQuery <- function(str, q) {
  cn <- RODBC::odbcDriverConnect(str)
  df <- RODBC::sqlQuery(cn, q)
  RODBC::odbcClose(cn)
  
  return(df)
}
df <- makeQuery(str, q)

# date
df$dt <- with(df, as.Date(paste0(substr(dt, 1, 8),'01')))

# get cumulative volumes injected
q <- "SELECT year(pr.DATE_TIME) as yr, MONTH(pr.DATE_TIME) as mn, 
      sum(pr.GAS_INJECTION_VOLUME) - sum(pr.GAS_PRODUCTION_VOLUME) as bal
        
		  FROM WELL_INFORMATION as wi
      inner join
      PRODUCTION_HISTORY as pr
      ON wi.WELL_KEY = pr.WELL_KEY
        
      where 
      wi.FIELD_NAME = 'Kenai Gas Storage - 50.0672'
  		and
  		pr.DATE_TIME >= '2020-01-01'
  		
      group by year(pr.DATE_TIME), month(pr.DATE_TIME)
  		order by yr, mn"

# Get the cum data
df_cums <- makeQuery(str, q)
df_cums$dt <- with(df_cums, as.Date(paste0(yr,"-",mn,"-01")))
df_cums$cum <- cumsum(df_cums$bal)

# join the cum data to tbg press data
df_join <- left_join(df, df_cums, by = 'dt')

# model
fit <- lm(tbg ~ cum, data = df_join)
df_join$fit <- as.data.frame(predict.lm(fit, df_join, interval = 'prediction'))[['fit']]
df_join$lwr <- as.data.frame(predict.lm(fit, df_join, interval = 'prediction'))[['lwr']]
df_join$upr <- as.data.frame(predict.lm(fit, df_join, interval = 'prediction'))[['upr']]

# bin the tbg press
df_join$bin <- cut(df_join$tbg, seq(100, 240, by = 10))

p1 <- ggplot(df_join, aes(cum, tbg)) + 
  geom_point() + 
  geom_line(aes(y = fit)) +
  geom_line(aes(y = lwr), color = 'red') +
  geom_line(aes(y = upr), color = 'red') 
#print(p1)

p2 <- ggplot(df_join, aes(lat, lng, fill = bin)) + 
  geom_point(size = 5, shape = 21, color = 'black') + 
  facet_grid(yr~mn)
#print(p2)

df_var <- df_join %>%
  group_by(yr, mn) %>%
  summarize(Date = as.Date(paste0(yr, '-', mn,'-01')), 
            Count = length(tbg),
            Mean = mean(tbg),
            SD = round(sd(tbg), 0),
            Upr = qnorm(0.05, Mean, SD),
            Lwr = qnorm(0.95, Mean, SD))

p3 <- ggplot(df_join, aes(x = as.factor(mn), y = tbg)) + 
  geom_jitter(aes(fill = name), shape = 21, size = 3, width = 0.2) +
  geom_boxplot(color = 'black', alpha = 0.5, size = .8) + 
  geom_label(data = df_var, aes(y = 220, label = SD), size = 2) +
  facet_grid(yr ~ mn, scales = 'free_x') +
  xlab('\n Month') +
  ylab('Static Tubing Pressure (psig) \n') +
  ggtitle('Static Tubing Pressure Variatiation Over Time \n') + 
  theme_bw() +
  theme(
    axis.title = element_text(color = 'black', face = 'bold', 
                              size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                              size = 12),
    plot.title = element_text(color = 'black', face = 'bold', 
                              size = 14, hjust = 0.5),
    strip.text = element_text(color = 'black', face = 'bold', 
                              size = 12),
    strip.background = element_rect(fill = 'grey90'),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.background = element_rect(color = 'black'),
    legend.position = c(0.8, 0.12)
  )

print(p3)

pdf(file = 'tbg_press_boxplot.pdf', height = 17, width = 11)
p3
dev.off()

# all tbg together norm model monthly
p4 <- ggplot(df_var, aes(Date, Mean)) + 
  geom_step() +
  geom_step(aes(y = Upr), color = 'red') +
  geom_step(aes(y = Lwr), color = 'red') +
  #geom_label(aes(label = Mean)) +
  xlab('\n Date') +
  ylab('Static Tubing Pressure (psig) \n') +
  ggtitle('Static Tubing Pressure Normal Range Over Time \n') + 
  theme_bw() +
  theme(
    axis.title = element_text(color = 'black', face = 'bold', 
                              size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                             size = 12),
    plot.title = element_text(color = 'black', face = 'bold', 
                              size = 14, hjust = 0.5),
    strip.text = element_text(color = 'black', face = 'bold', 
                              size = 12),
    strip.background = element_rect(fill = 'grey90'),
    #panel.grid.minor = element_blank(),
    #panel.grid.major.x = element_blank(),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.background = element_rect(color = 'black'),
    legend.position = c(0.8, 0.12)
  )

pdf(file = 'tbg_press_over_time.pdf', height = 11, width = 17)
p4
dev.off()
