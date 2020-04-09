library(tidyverse)
library(rpart)
library(rpart.plot)
#library(rattle)

library(readxl)
library(janitor)

# library(lubridate)
# library(scales)
# library(googledrive)
# library(randomcoloR)
# library(gghighlight)

setwd("~/Desktop/corona")
options("scipen"=100, digits = 4)
texto45 = theme(axis.text.x=element_text(angle=45, hjust=1))
theme_set(theme_minimal())


casos = read_excel("casos.xlsx") %>% clean_names()

str(casos)
names(casos)
table(casos$sars_cov_2_exam_result)

cc <- function(df) {
  print(df %>% is.na() %>% colSums())
}

cd <- function(df) {
  print(df %>% is.na() %>% colMeans())
}

casos %>% cc
casos %>% cd

casos$patient_id = NULL
casos$patient_addmited_to_intensive_care_unit_1_yes_0_no = NULL
casos$patient_addmited_to_regular_ward_1_yes_0_no = NULL
casos$patient_addmited_to_semi_intensive_unit_1_yes_0_no = NULL

lossmatrix <- matrix(c( 0,  1,
                        5,  0), 
                     byrow = TRUE, nrow = 2)


fit <- rpart(sars_cov_2_exam_result ~ ., method="class", data=casos, parms = list(loss = lossmatrix))
pred = predict(fit, type="class")
table(pred, casos$sars_cov_2_exam_result)

rpart.plot(fit, box.palette = "GnYlRd")

casos %>% count(sars_cov_2_exam_result)
casos %>% filter(patient_age_quantile >= 3 & (leukocytes < -0.52)) %>% count(sars_cov_2_exam_result)

casos %>% filter(sars_cov_2_exam_result == "positive") %>% count(is.na(leukocytes))


# ------------------

casos2 = read_excel("casos.xlsx") %>% clean_names() %>% filter(sars_cov_2_exam_result == "positive") %>%
         mutate(addmited = factor(case_when(
                   patient_addmited_to_regular_ward_1_yes_0_no == 1 ~ "Regular",
                   patient_addmited_to_semi_intensive_unit_1_yes_0_no == 1 ~ "Semi",
                   patient_addmited_to_intensive_care_unit_1_yes_0_no == 1 ~ "Intensive",
                   TRUE ~ "Home"), levels = c("Home", "Regular", "Semi", "Intensive")))

str(casos2$addmited)

table(casos2$addmited)
table(casos2$patient_addmited_to_intensive_care_unit_1_yes_0_no, casos2$patient_addmited_to_semi_intensive_unit_1_yes_0_no, casos2$patient_addmited_to_regular_ward_1_yes_0_no)
casos2 %>% mutate(addmited = paste0(patient_addmited_to_regular_ward_1_yes_0_no, patient_addmited_to_semi_intensive_unit_1_yes_0_no, 
                  patient_addmited_to_intensive_care_unit_1_yes_0_no)) %>%
           group_by(addmited) %>% summarise(cont = n())

casos2$patient_id = NULL
casos2$patient_addmited_to_regular_ward_1_yes_0_no = NULL
casos2$patient_addmited_to_semi_intensive_unit_1_yes_0_no = NULL
casos2$patient_addmited_to_intensive_care_unit_1_yes_0_no = NULL
casos2$sars_cov_2_exam_result = NULL

lossmatrix2 <- matrix(c( 0,  5, 10, 20,
                         5,  0, 10, 20,
                        10, 10,  0, 20,
                        20, 20, 20,  0), 
                     byrow = TRUE, nrow = 4)

# lossmatrix2 <- matrix(c( 0,  1,  1,  1,
#                          1,  0,  1,  1,
#                           1,  1,  0,  1,
#                           1,  1,  1,  0), 
#                       byrow = TRUE, nrow = 4)
# 

fit2 <- rpart(addmited ~ ., method="class", data=casos2, parms = list(loss = lossmatrix2))
              # maxdepth = 1, 
              #minsplit = 2, 
              #minbucket = 2)
pred = predict(fit2, type="class")
table(pred, casos2$addmited)


(501+7+5+5)/sum(table(pred, casos2$addmited))

516/sum(table(pred, casos2$addmited))

