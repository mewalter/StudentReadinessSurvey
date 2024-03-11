


scale = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")

# Simulate data
data <- data.frame(
  q1 = sample(scale, size = 1000, replace = TRUE,
              prob = c(0.07, 0.11, 0.08, 0.28, 0.46)), 
  q2 = sample(scale, size = 1000, replace = TRUE,
              prob = c(0.19, 0.12, 0.11, 0.23, 0.35)),    
  q3 = sample(scale, size = 1000, replace = TRUE,
              prob = c(0.15, 0.25, 0.14, 0.21, 0.25)),  
  q4 = sample(scale, size = 1000, replace = TRUE,
              prob = c(0.38, 0.18, 0.13, 0.16, 0.15)),  
  q5 = sample(scale, size = 1000, replace = TRUE,
              prob = c(0.46, 0.26, 0.07, 0.14, 0.07))
)

col_names <- names(data)
data[,col_names] <- lapply(data[,col_names], factor, levels = scale)


data <- tibble %>%
  # set factors 
  mutate(
    across(
      everything(),
      ~ fct(.x, scale)
    )
  ) %>% 
  #   # convert to data frame
  as.data.frame()



agreelevel <- c("Strongly Disagree", "Disagree", "Agree","Strongly Agree")
n <- 20 # rows
p <- 30 # likert columns

fake_data <- sample(agreelevel, size = n * p, replace = TRUE) %>%
  matrix(nrow = n, ncol = p) %>%
  as_data_frame() %>%
  mutate(respondent = 1:n) %>%
  mutate_at(vars(starts_with("V")), factor)
str(fake_data) # factor levels are bad

fake_data_factored <- fake_data %>%
  mutate_at(vars(starts_with("V")),
            funs(factor(., levels = agreelevel, ordered = TRUE)))
str(fake_data_factored) # better now


likert(Item~., cookie_data, ReferenceZero=3, ylab = "Question", main = list("Cookie Data", x=unit(.62, "npc")), 
       auto.key = list(columns = 2, reverse.rows = T))

dental_hyg <- dental_hyg %>%
  rename("How Often Respondents Use Mouth Wash" = mouth_wash,
         "How Often Respondents Brush Their Teeth" = tooth_brush,
         "How Often Respondents Use Floss" = floss)
## New df for not grouped
dh <- dental_hyg %>% select(-age)
plot(likert(dh), legend.position="right")

plot(likert(dental_hyg[,c(1:3)], grouping = dental_hyg[,4]),
     legend.position="right")

name_vec <- setNames(c("V1", "V2", "V3", "V4"), c("whale", "toast", "cheese", "cow"))
                     


# https://www.henriettearndt.com/post/summarize-and-visualize-likert-scale-data
plot(data.summary, 
     type = "bar",
     # legend
     legend.position = "top",
     # bar colours
     low.color = "#FDE725FF",
     high.color = "#21918c",
     # numerical labels
     plot.percents = TRUE,
     plot.percent.high = FALSE,
     plot.percent.low = FALSE
) +
  # ggplot2 syntax
  # change axis label
  labs(y = "Proportion of total responses") +
  # insert full-length item labels
  scale_x_discrete(labels = 
                     #line break after 25 characters
                     stringr::str_wrap(questions, 25))



