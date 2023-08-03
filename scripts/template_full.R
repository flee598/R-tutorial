# Description ----

# load socio-economic data, and look at the relationship between gdp and 
# life expectancy.


# Set up -----------------------------------------------------------------------

# load packages
library(tidyverse)
library(gapminder)


# Load data --------------------------------------------------------------------


# load built-in gapminder dataset
gap_df <- gapminder::gapminder

# data cleaning ----------------------------------------------------------------


# get asia only and drop "pop" column
asia_df <- gap_df |>
  filter(continent == "Asia") |>
  filter(year == max(year)) |>
  select(-pop)


# analysis ---------------------------------------------------------------------

# run linear regression
life_exp_lm <- lm(formula = lifeExp ~ gdpPercap, data = asia_df)
summary(life_exp_lm)

# plot -------------------------------------------------------------------------

# plot gdp vs life expectency
p1 <- ggplot(data = asia_df, aes(x = log(gdpPercap),
                                 y = lifeExp,
                                 colour = country)) +
  geom_point() +
  labs(x = "Log(GDP/capita ($))",
       y = "Life expectency (yrs)") +
  theme_bw()

p1

# save outputs -----------------------------------------------------------------

# save figure
ggsave(filename = "./figures/fig1_life_gdp.png")


# save model output ...

# END --------------------------------------------------------------------------