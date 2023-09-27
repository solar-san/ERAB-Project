
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  include = TRUE
)

library(tidyverse)
library(ggridges)
library(survey)

library(ggthemes)
library(patchwork)

theme_set(
  theme_tufte(
    base_size = 32,
    base_family = 'Atkinson Hyperlegible'
    )
  )



url <- "https://www.bancaditalia.it/statistiche/tematiche/indagini-famiglie-imprese/alfabetizzazione/iacofi_ascii_2020.zip"
temp <- tempfile()

download.file(url, temp)
  
iacofi_full <- read.csv(unz(temp, "iacofi2020.csv")) %>% 
  select(-anno)

unlink(temp)



iacofi_full %>% is.na() %>% sum()



var_list <- c(
  'wght', #sample weights
  'edu', #level of education
  'qd1', #gender
  'qd7', #age
  'capofam',
  #'status', #civil status
  'reddito', #net monthly income
  #'casa',
  'area5', #geographic region
  'qf1', #who is responsible for managing money in your family?
  'qf1_a',
  'qf3_1', #did you save and how?
  'qf3_2',
  'qf3_3',
  'qf3_5',
  'qf3_6',
  'qf3_7',
  'qf3_8',
  'qf3_98',
  'qf3_99',
  'qprod1b_1', #which financial products do you invest in?
  'qprod1b_2',
  'qprod1b_3',
  'qprod1b_5',
  'qprod1b_5_1',
  'qprod1b_6',
  'qprod1b_7',
  'qprod1b_8',
  'qprod1b_10',
  'qprod1b_11', #currently investing in stocks
  'qprod1b_12',
  'qprod1b_13',
  'qprod1b_14',
  'qprod1b_15',
  'qprod1b_17',
  'qprod1b_97',
  'qprod1b_98',
  'qprod1b_99',
  'qprod1c_1', #which financial products did you invest in in the past 2y?
  'qprod1c_2',
  'qprod1c_3',
  'qprod1c_5',
  'qprod1c_5_1',
  'qprod1c_6',
  'qprod1c_7',
  'qprod1c_8',
  'qprod1c_10',
  'qprod1c_11', #invested in stocks in the past 2y
  'qprod1c_12',
  'qprod1c_13',
  'qprod1c_14',
  'qprod1c_15',
  'qprod1c_17',
  'qprod1c_97',
  'qprod1c_98',
  'qprod1c_99',
  'qprod2',   # how did you choose?
  'qprod3_int_1', #which sources influenced your choice?
  'qprod3_int_2',
  'qprod3_int_3',
  'qprod3_int_4',
  'qprod3_int_5',
  'qprod3_int_6',
  'qprod3_int_99',
  'qs1_1',    #proxy for risk aversion
  'qs1_2', #I would accept that I might lose some of my money
  'qs1_3',
  'qs1_5',
  'qs1_7',
  'qs1_8',
  'qs1_9',
  'qs1_10',
  'qs1_11',
  'qs1_13',
  'qk1', #how is your knowledge in financial questions?
  'qk3', #interest rate
  'qk5', #interest rate
  'qk6', #interest rate
  'qk7_1', #understanding of risk and return
  'qk7_1alt',
  'qk7_2',
  'qk7_3',
  'qk7_3alt',
  'FK', #financial knowledge
  'FK1',
  'FK2',
  'FK3',
  'FK4',
  'FK5',
  'FK6',
  'FK7',
  'FL', #financial literacy
  'FB', #financial behaviour
  'FB1',
  'FB7',
  'FA' #financial attitude
  )

iacofi_data <- iacofi_full %>% 
  select(
    all_of(
      var_list
    )
  )

attach(iacofi_data)

iacofi_data %>%
  str()



iacofi_data$qk7_1 <-  ifelse(iacofi_data$qk7_1 %>% is.na(), iacofi_data$qk7_1alt, iacofi_data$qk7_1)
iacofi_data$qk7_3 <- ifelse(iacofi_data$qk7_3 %>% is.na(), iacofi_data$qk7_3alt, iacofi_data$qk7_3)
iacofi_data <- iacofi_data %>%
  select(-qk7_1alt, -qk7_3alt)



iacofi_data %>% is.na() %>% sum()



#rebuilding gender variable
names(iacofi_data)[which(names(iacofi_data) == 'qd1')] <- 'gender'
names(iacofi_data)[which(names(iacofi_data) == 'qd7')] <- 'age'

iacofi_data$gender <- as.factor(
  ifelse(
    iacofi_data$gender == 0,
    'Female',
    'Male'
    )
)



iacofi_data$edu <- ifelse(
  iacofi_data$edu == 1,
  "Post-secondary or higher, with degree",
  ifelse(
    iacofi_data$edu == 2,
    "Post-secondary, no degree",
    ifelse(iacofi_data$edu == 3,
           "Upper-secondary, with diploma",
           ifelse(iacofi_data$edu == 4,
                  "Upper-secondary, no diploma",
                  ifelse(iacofi_data$edu == 5,
                         "Lower-secondary, with license",
                         ifelse(iacofi_data$edu == 6,
                                "Lower-secondary, without license",
                                ifelse(iacofi_data$edu == 7,
                                       "Primary, with license",
                                       "Primary without license, no education"
                                       )
                                )
                         )
                  )
           )
  )
) %>%
  as.factor()



iacofi_data$edu <- iacofi_data$edu %>% 
  factor(
    levels = c(
      "Primary without license, no education",
      "Primary, with license",
      "Lower-secondary, without license",
      "Lower-secondary, with license",
      "Upper-secondary, no diploma",
      "Upper-secondary, with diploma",
      "Post-seconday, no degree",
      "Post-secondary or higher, with degree"
    )
  )



iacofi_data$income <- ifelse(
  iacofi_data$reddito == 1,
  "Up to € 439",
  ifelse(iacofi_data$reddito == 2,
         "€ 440 - € 539",
         ifelse(iacofi_data$reddito == 3,
                "€ 540 - € 644",
                ifelse(iacofi_data$reddito == 4,
                       "€ 645 - € 749",
                       ifelse(iacofi_data$reddito == 5,
                              "€ 750 - € 849",
                              ifelse(iacofi_data$reddito == 6,
                                     "€ 850 - € 954",
                                     ifelse(iacofi_data$reddito == 7,
                                            "€ 955 - € 1.059,00",
                                            ifelse(iacofi_data$reddito == 8,
                                                   "€ 1.060,00 - € 1.159,00",     
                                                   ifelse(iacofi_data$reddito == 9,
                                                          "€ 1.160,00 - € 1.264,00",
                                                          ifelse(iacofi_data$reddito == 10,
                                                                 "€ 1.265,00 - € 1.369,00",
                                                          ifelse(iacofi_data$reddito == 11,
                                                                 "€ 1.370,00 - € 1.549,00",
                                                                 ifelse(iacofi_data$reddito == 12,
                                                                        "€ 1.550,00 - € 1.939,00",
                                                                        ifelse(iacofi_data$reddito == 13,
                                                                               "€ 1.940,00 - E. 2.454,00",
                                                                               ifelse(iacofi_data$reddito == 14,
                                                                                      "€ 2.455,00 - € 3.875,00",
                                                                                      "More than € 3.875,00")
                                                                               )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
                              )
                       )
                )
         )
  )
) %>%
  as.factor()



iacofi_data$income <- iacofi_data$income %>% 
  factor(
    levels = c(
      "Up to € 439",
      "€ 440 - € 539",
      "€ 540 - € 644",
      "€ 645 - € 749",
      "€ 750 - € 849",
      "€ 850 - € 954",
      "€ 955 - € 1.059,00",
      "€ 1.060,00 - € 1.159,00",
      "€ 1.160,00 - € 1.264,00",
      "€ 1.265,00 - € 1.369,00",
      "€ 1.370,00 - € 1.549,00",
      "€ 1.550,00 - € 1.939,00",
      "€ 1.940,00 - E. 2.454,00",
      "€ 2.455,00 - € 3.875,00",
      "More than € 3.875,00"
    )
  )



iacofi_data$area5 <- ifelse(
  iacofi_data$area5 == 1,
  'Nord-ovest',
  ifelse(iacofi_data$area5 == 2,
         'Nord-est',
         ifelse(iacofi_data$area5 == 3,
                'Center',
                ifelse(iacofi_data$area5 == 4,
                       'Sud',
                       'Island'
                       )
                )
         )
  ) %>%
  as.factor()



iacofi_data$area5 <- iacofi_data$area5 %>% 
  factor(
    levels = c(
      "Island",
      "Sud",
      "Center",
      "Nord-est",
      "Nord-ovest"
    )
  )



iacofi_data$qf1 <- ifelse(
  iacofi_data$qf1 == 1,
  "Only me.",
  ifelse(
    iacofi_data$qf1 == 2,
    "Me along with my family.",
    ifelse(
      iacofi_data$qf1 == 3,
      "Someone else is.",
      ifelse(
        iacofi_data$qf1 == -97,
        "I do not know.",
        NA
        )
    )
  )
)



iacofi_data$capofam <- ifelse(
  iacofi_data$capofam == 3, 
  "The head of the household is another person.",
  ifelse(
    iacofi_data$capofam == 2, 
    "My husband/wife is the head of the household.",
    "I'm the head of the household."
    )
  ) %>% 
  as.factor()


#| fig-cap: The inequality is overwhelming; \n in particular, the proportion of males declaring that their wife is earning a larger income in the family is close to being invisible.
#| fig-cap-location: margin
#| label: fig-household
iacofi_data %>% 
  ggplot(
    aes(
      fill = gender,
      x = capofam,
      weight = wght
      ),
    ) + 
  geom_bar() +
  labs(
    x = "",
    fill = 'Gender',
    title = 'Who is the head of your household?'
    ) +
  coord_flip() +
  scale_fill_viridis_d(
    option = "E"
    ) +
  theme_tufte() +
  theme(
    text = element_text(
      family =  'Atkinson Hyperlegible',
      size = 12,
    ),
    axis.text.y = element_text(
      size = 14
    ),
    axis.ticks = element_line(
      linewidth = 2,
      size = 2
    ),
    plot.title = element_text(
      face = "bold",
      size = 16
    )
  )


#| fig-cap: Number of persons by education level and geographic area in Italy.
#| fig-cap-location: margin
#| label: fig-2
iacofi_data %>%
  filter(!is.na(edu)) %>% 
  
  ggplot(
    aes(
    y = edu,
    fill = area5,
    weight = wght
    )
  ) +
  geom_bar(
  ) +
  scale_fill_viridis_d(
    option = "E"
  ) +
  labs(
    title = 'Education level by area',
    y = '',
    fill = 'Geographic region'
  ) +
  theme(
    text = element_text(
      family =  'Atkinson Hyperlegible',
      size= 12),
    axis.text.x = element_text(
      angle = 315
    ),
    legend.position = c(0.75, 0.25),
    legend.text = element_text(
      size = 10
    ),
    legend.key.size = unit(
      .5,
      "line"
      )
  ) 




#| fig-cap: The answers to this specific question have been used as grouping variables on the x-axis, while on the y-axis there is a count of the responses for each answer. The grouping variable is a categorical representation of income levels. This plot allows to visualize the differences in gender and income of each responder. While most of households share this kind of decisions, the proportion of male/females grows higher when income increases.
#| fig-cap-location: margin
#| column: page
#| label: fig-managingmoney
iacofi_data %>% 
  filter(!is.na(qf1))%>%
  ggplot(
    aes(
      x = qf1, 
      col = income,
      size = gender,
      weight = wght
      ),
    alpha = .1
    ) +
  geom_point(
    stat = 'count',
    alpha = .7
    ) +
  labs(
    title = 'Who manages money in your household?',
    x = '',
    size = 'Gender',
    col = 'Income'
    ) +
  scale_color_viridis_d(
    option = "E"
  ) +
  theme_tufte() +
  theme(
    text = element_text(
      family =  'Atkinson Hyperlegible',
      size= 18),
    axis.text.x = element_text(
      angle = 50, vjust = 1, hjust = 1, size = 18
      ),
    legend.text = element_text(
      size = 14
    ),
    legend.position = c(
      'left'
      # .75,
      # .70
      ),
    plot.title = element_text(
      face = "bold"
    )
  ) +
  guides(
   color = guide_legend(
      direction = "vertical",
      override.aes = list(
        size = 6
        )
      )
  )


#| fig-cap: Densities of income (x-axis) by degree (grouping variable on the y-axis) and area (each density curve representing an Italian region). Living in the northern regions and having spent more years pursuing a degree consistently relates to higher income and less variability.
#| fig-cap-location: margin
#| label: fig-distr
iacofi_data %>%
  filter(
    !is.na(
      edu
    )
  ) %>% 
  ggplot(
    aes(
     x = reddito,
     y = edu,
     fill = area5,
     weight = wght
     )
    ) +
  geom_density_ridges(
    alpha = .8
  ) +
  labs(
    x = 'Income distribution',
    y = '',
    fill = 'Geographic region',
    title = 'Income distribution by geographic region and education'
  ) +
  scale_fill_viridis_d(
      option = "E"
    ) +
  theme_tufte() +
  theme(
    text = element_text(
      family =  'Atkinson Hyperlegible',
      size= 17),
     legend.text = element_text(
      size = 14
    ),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(
      size = 18
      ),
    legend.position = "top"
  )



fit.sel <- lm(
  FK ~ gender + edu + age,
  data = iacofi_data
  )

kableExtra::scroll_box(
  knitr::kable(
    fit.sel %>% 
    summary() %>% 
    coefficients(),
    caption = "Statistical significance of regression for model (1):"
  )
)



kableExtra::scroll_box(
  knitr::kable(tibble(
    "cor(gender, edu)" = cor(iacofi_full$qd1, iacofi_full$edu),
    "cor(gender, age)" = cor(iacofi_full$qd7, iacofi_full$qd1),
    "cor(gender, FK)" = cor(iacofi_full$qd1, iacofi_full$FK),
    "cor(gender, income)" =cor(iacofi_full$qd1, iacofi_full$reddito),
    "cor(gender, area)" =cor(iacofi_full$qd1, iacofi_full$area),
    "cor(edu, age)" = cor(iacofi_full$qd7, iacofi_full$qd1),
    "cor(edu, FK)" = cor(iacofi_full$qd1, iacofi_full$FK),
    "cor(edu, income)" =cor(iacofi_full$qd1, iacofi_full$reddito),
    "cor(edu, area)" =cor(iacofi_full$qd1, iacofi_full$area),
    "cor(FK, age)" = cor(iacofi_full$qd7, iacofi_full$qd1),
    "cor(FK, income)" =cor(iacofi_full$qd1, iacofi_full$reddito),
    "cor(FK, area)" =cor(iacofi_full$qd1, iacofi_full$area),
    "cor(income, area)" =cor(iacofi_full$qd1, iacofi_full$area)
  ) %>% 
  t 
  )
)



iacofi_data <- iacofi_data %>% 
  mutate(
    risk_proxy = ifelse(
      qf3_7 == T | 
        qprod1c_11 == T |
        qs1_2 == T,
      T,
      F
      )
    )



fit.logi <- glm(
  risk_proxy ~ 
    capofam +
    income +
    qf1 +
    area5 +
    edu +
    FK +
    age +
    gender,
  data = iacofi_data,
  family = binomial
  )

kableExtra::scroll_box(
  knitr::kable(
    fit.logi %>% 
      summary() %>% 
      coefficients(),
    caption = "Logistic regression estimated coefficients for model (2):"
  )
)
