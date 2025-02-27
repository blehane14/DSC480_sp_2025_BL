## A few select R packages

packages <- c("devtools", "broom", "broom.mixed", "cherryblossom", "corrplot", "corrr",
              "ggthemes", "here", "survey", "srvyr", "janitor", "lubridate", "magrittr",
              "scales", "skimr", "shiny", "tidyverse", "tidymodels", "survival", 
              "poissonreg", "car", "ipw", "glmet", "lme4", "tibble")

install.packages(packages)


new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

Yes
