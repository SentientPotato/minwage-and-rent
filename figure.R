## Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
## Define a function to convert two-digit years to four
convert_years = function(years) year(ym(paste0(years, "-01")))
## Read in data
## Minimum wage data handcoded & included, Fair Market Rate for rent data from
## https://www.huduser.gov/portal/datasets/fmr.html#history
mw = read_csv("minimum-wage.csv")
fmr = read_csv("FMR_All_1983_2022_rev.csv")
## Get the median FMR across all areas for each year,
## and determine how many hours worked at minimum wage are needed to pay rent
studio = fmr %>% 
    select(areaname, matches("fmr[0-9]+_0")) %>% 
    pivot_longer(
        cols = -areaname,
        names_to = "Year",
        names_pattern = "fmr(.*)_0",
        names_transform = convert_years,
        values_to = "FMR"
    ) %>% 
    group_by(Year) %>% 
    summarise(FMR = median(FMR, na.rm = TRUE)) %>% 
    left_join(mw, by = "Year") %>% 
    mutate(Hours = FMR / MinWage)
## Plot results
map = aes(x = Year, y = Hours)
cap = "Figure by Sentient Potato (Twitter: @SentientPotato6)"
url = "https://github.com/SentientPotato/minwage-and-rent"
cap = paste(cap, paste("Code to reproduce at", url), sep = "\n")
ttl = "Minimum wage hours needed for rent on a 40th percentile studio apartment"
plt = ggplot(data = studio, mapping = map) +
    geom_line(size = 1) +
    labs(title = ttl, caption = cap) +
    theme_bw() +
    theme(
        plot.caption = element_text(color = "#808080", hjust = 0),
        axis.title = element_blank()
    )
ggsave(
    filename = "minwage-and-rent.png", plot = plt,
    height = 3.76, width = 6.684, units = "in", dpi = 180
)
