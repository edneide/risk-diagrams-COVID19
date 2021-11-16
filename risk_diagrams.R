##===========================##
## by Edneide Ramalho        ##
## Updated: 16 November 2021 ##
##===========================##

# 0. Packages -----------
library(readxl)
library(ipsum)
library(hrbrthemes)
library(tidyverse)
library(hrbrthemes)
library(pracma)
library(reshape)
library(lubridate)
library(plotly)
library(pracma)

# Inputs ---------
path <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
country <- "Spain"


# 1. Import dataframe -----------

## 1.1. Download data from Our World in Data --------
data = read_csv(path)

## 1.2. Filter for the chosen country --------
df_country <- data %>% 
  dplyr::filter(location == country) %>% 
  dplyr::select(date, total_cases, new_cases, population)

## 1.3. Creating important variables to compute EPG -------

### rho---------
incubation_time = 5 # days
rho <- rep(NA, length(df_country$date)) # vector filled with NA

### Populating rho
for (i in 8:length(rho)) {
  rho_temp = (df_country$new_cases[i] + df_country$new_cases[i-1] 
              + df_country$new_cases[i-2])/(df_country$new_cases[i-incubation_time] 
                                            + df_country$new_cases[i-1-incubation_time] + 
                                              df_country$new_cases[i-2-incubation_time])
  if (is.na(rho_temp) | is.infinite(rho_temp)){
    rho[i] = 0
  }else{rho[i] = rho_temp}  
}

### rho_7 ---------
rho_7 <- movavg(rho, 7, type = "s")

### new_cases14 ---------
new_cases14 <- rep(NA, length(df_country$date))

for (i in 14:length(new_cases14)) {
  new_cases14[i] <- sum(df_country$new_cases[(i-13):i], na.rm = TRUE)
}

### new_cases14 per 10^5 inhabitants---------
population <- df_country$population[1]
new_cases14_per105 <- (new_cases14/population) * 100000

### EPG ---------
epg <- rho_7 * new_cases14_per105

# 2. Creating the final dataframe for risk diagram plot -------
indexes_df <- data.frame(rho_7 = rho_7, 
                         new_cases14 = new_cases14,
                         ar105 = new_cases14_per105,
                         epg = epg)


final_df <- cbind(df_country, indexes_df)

# 3. Plotting the risk diagrama ------
# Preparing matrix for background color
rh = seq(0, max(final_df$rho_7, na.rm = TRUE) + 0.5, 0.01) # rho_7_days
ar = seq(0, max(final_df$ar105, na.rm = TRUE), 
         length.out = length(rh)) # attack rate
data.fit <-  expand.grid(ar = ar, rh = rh)
data.fit = data.fit %>% 
  mutate(epg = ar*rh,
         epg_conv = ifelse(epg > 100, 100, epg))

## Plotting the risk diagram and saving in jpg -----------
eixoy = bquote(~ rho ~ "(mean of the last 7 days)")
eixox = bquote("Attack rate per" ~ 10^5 ~ "inh. (last 14 dias)")

risk_diagram <- ggplot() +
  geom_tile(data = data.fit, 
            mapping = aes(ar, rh, fill= epg_conv)) +
  scale_fill_gradientn(colours = rainbow(n = 7, s = 1, v = 1, 
                                         start = 0, end = 0.3,
                                         alpha = 0.7, rev = TRUE),
                       breaks = unlist(list(Low = 0, Moderate = 30, `Moderate-high` = 70, High =100))) +  
  geom_point(data = final_df, 
             mapping = aes(x = ar105, y = rho_7),
             shape = 1, 
             size = 1.5,
             alpha = 0.2) + 
  geom_line(data = final_df, 
            mapping = aes(x = ar105, y = rho_7),
            color = "black", alpha = 0.2, linetype = "dashed") +
  geom_point(data = final_df[(dim(final_df)[1] - 30):(dim(final_df)[1]-1), ],
             mapping = aes(x = ar105, y = rho_7),
             color = "blue") + 
  geom_point(data = final_df[dim(final_df)[1], ],
             mapping = aes(x = ar105, y = rho_7),
             color = "white") + 
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks = 0:max(rho_7, na.rm = TRUE)) +
  labs(x = eixox, 
       y = eixoy,
       title = country,
       subtitle = " ",
       caption = paste("Source: Our World in Data.", "Updated in", 
                       format(as.Date(today()), "%Y-%m-%d"))) +
  ggtitle(country) + 
  theme_minimal() +
  theme(legend.position='none')
risk_diagram
ggsave(paste0("risk_diagram_", country, ".jpg"),
       height = 12, width = 15, units = "cm")

## Risk diagram in HTML ---------
xlab_html <- paste("Attack rate per ", TeX(10^50),  " inh. (last 14 days)") 

fig <- ggplotly(ggplot() +
                  geom_tile(data = data.fit, 
                            mapping = aes(ar, rh, fill= epg_conv)) +
                  scale_fill_gradientn(colours = rainbow(n = 7, s = 1, v = 1, 
                                                         start = 0, end = 0.3,
                                                         alpha = 0.7, rev = TRUE),
                                       breaks = unlist(list(Low = 0, Moderate = 30, `Moderate-high` = 70, High =100))) +  
                  geom_point(data = final_df, 
                             mapping = aes(x = ar105, y = rho_7),
                             shape = 1, 
                             size = 1.5,
                             alpha = 0.2) + 
                  geom_line(data = final_df, 
                            mapping = aes(x = ar105, y = rho_7),
                            color = "black", alpha = 0.2, linetype = "dashed") +
                  geom_point(data = final_df[(dim(final_df)[1] - 30):(dim(final_df)[1]-1), ],
                             mapping = aes(x = ar105, y = rho_7),
                             color = "blue") + 
                  geom_point(data = final_df[dim(final_df)[1], ],
                             mapping = aes(x = ar105, y = rho_7),
                             color = "white") + 
                  geom_hline(yintercept = 1) +
                  scale_y_continuous(breaks = 0:max(rho_7, na.rm = TRUE)) +
                  ggtitle(country) + 
                  theme_minimal() +
                  theme(legend.position='none'))


fig_html <- fig %>% 
  layout(xaxis = list(title = TeX("\\text{Attack rate per} 10^{5} \\text{inh. (last 14 days)}")),
         yaxis = list(title = TeX("\\rho \\text{(mean of the last 7 days)}"))) %>% 
  config(.Last.value, mathjax = 'cdn')
fig_html


htmlwidgets::saveWidget(fig_html, 
                        paste0("risk_diagram_", country, ".html"))



