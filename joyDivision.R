#devtools::install_github("https://github.com/UncleCamsWaterPlans/WQI")
library(WQI)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(plotly)
library(ggridges)

# Suporting tables
#####
#Supporting tables
sitelist <- read_csv("~/GitHub/joyful_water/sitelist.csv")
sitelist$`Site ID` <- as.character(sitelist$`Site ID`)


NIWA_MD <- 2.4 
WQO <- 0.14


summary <- catchment %>%
  group_by(name) %>%
  dplyr::summarise(
    GSnum     = GSnum[1],
    max       = max(Value, na.rm = TRUE),
    mean      = mean(Value, na.rm = TRUE),
    median    = median(Value, na.rm = TRUE),
    "80th"    = quantile(Value, 0.8, na.rm = TRUE),
    "95th"    = quantile(Value, 0.95, na.rm = TRUE),
  )

summary <- left_join(summary, sitelist[,c(3,8,9)], by = c("GSnum" = "Site ID"))
#assign colours to site type
summary <- summary %>%
  dplyr::mutate(col = dplyr::case_when(
    `Site type` == 'Reference' ~ "#4FBD42", 
    `Site type` ==  "Control" ~ "#428CBD",
    `Site type` == 'Impact' ~ "#B042BD",
    `Site type` == "End of System" ~ "#BD7242",
    TRUE ~ 'grey' ))


catchment <- left_join(catchment, summary)


df <- left_join(catchment, sitelist[,c(3,8,9)], by = c("GSnum" = "Site ID"))
# Concatenate site and type columns with separator
df$combined_col <- paste(df$`Site type.x`," // ", df$name, sep = "")



#####
# function for number of observations 
give.n <- function(x){
  return(c(y = max(x)+0.2, label = nobs[x])) 
  # experiment with the multiplier to find the perfect position
}
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

options(scipen=10000)


p <- ggplot(data = df, aes(
  y = fct_reorder(combined_col, `median`),
  x = Value,
  fill = after_stat(x)
)) +
  theme_bw() +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  geom_vline(aes(xintercept = NIWA_MD, linetype = paste0("NIWA Guideline: ", NIWA_MD, "mg/L")), size = 1, color = "red") +
  geom_vline(aes(xintercept = WQO, linetype = paste0("Water Quality Objective: ", WQO, "mg/L")), size = 1, color = "blue") +
  scale_linetype_manual(name = "Guidelines:", values = c(2,2),
                        guide = guide_legend(override.aes = list(color = c("red","blue")))) +
  theme_ridges() + 
  scale_x_log10(oob = scales::squish_infinite) +
  scale_y_discrete(position = "right") +
  scale_fill_viridis(name = "Nitrate-N (mg/L)" , option = "G", direction = 1, discrete = FALSE, trans = 'log10', na.value = NA, limits = c(0.001,50)) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 11),
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(
      size = 11,
      #face = "bold",
      #colour = rev(col)
    ),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_blank()
  ) +
  xlab(expression('Distribution of Nitrate-N concentrations per site')) +
  ggtitle("Nitrate-N Concentration Distribution in the Lower Herbert Catchment QLD") 
p
