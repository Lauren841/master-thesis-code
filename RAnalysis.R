################################################################################
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(mediation)
library(ggplot2)
library(ggpubr)
library(interactions)
library(ggeffects)
library(car)
library(survival)
library(lmtest)
library(ggeffects)
library(sandwich)
library(psych)
library(tidyverse)
################################################################################

Data_CSV <- read.csv("Marketing Management/Thesis/R/Nutritional information_May 25, 2025_13.57.csv")
Data_CSV <- Data_CSV %>% select(where(~ sum(is.na(.)) < nrow(Data_CSV)))
Data_CSV <- Data_CSV[-1, ]
Data_CSV[, 5] <- as.numeric(Data_CSV[[5]])
Data_CSV <- Data_CSV %>% filter(Progress == 100.0)
Data_CSV <- Data_CSV %>% select(-c(StartDate, EndDate, Status,IPAddress,Progress,
                                   Finished,RecordedDate,ResponseId,RecipientLastName,
                                   RecipientFirstName,RecipientEmail,ExternalReference,
                                   LocationLatitude,LocationLongitude,DistributionChannel,
                                   UserLanguage,Q_UnansweredPercentage,Q_UnansweredQuestions
                                   ,Q_StraightliningCount,Q_StraightliningPercentage,
                                   Q_StraightliningQuestions,Q57))
colnames(Data_CSV)[which(colnames(Data_CSV) == "Q89_1")] <- "risk_perception"
Data_CSV$risk_perception <- as.numeric(as.character(Data_CSV$risk_perception))
################################################################################
product_ids <- sprintf("P%02d", 1:36)
categories <- c("bread", "milk", "baking_fat", "cheese", "butter", "pizza",
                "drinks", "cereal", "conserved_tomato", "muesli_bar", "chicken", "nuts")
product_categories <- rep(categories, each = 3)

set.seed(123)  # for reproducibility
n_products <- 36
Products <- data.frame(
  product_id = product_ids,
  category   = product_categories,
  fats       = round(runif(n_products, 1, 25), 1),     # grams
  saturated  = round(runif(n_products, 1, 25), 1),     # grams
  carbs      = round(runif(n_products, 10, 60), 1),    # grams
  sugar      = round(runif(n_products, 1, 25), 1),     # grams
  protein    = round(runif(n_products, 2, 20), 1),     # grams
  fiber      = round(runif(n_products, 0, 10), 1),     # grams
  sodium     = round(runif(n_products, 50, 600), 0)    # mg
)

Products <- Products %>%
  mutate(
    fats   = case_when(
      product_id == "P01" ~ 1.6,
      product_id == "P02" ~ 1.3,
      product_id == "P03" ~ 5.1,
      product_id == "P04" ~ 0.5,
      product_id == "P05" ~ 1.7,
      product_id == "P06" ~ 3.6,
      product_id == "P07" ~ 90.6,
      product_id == "P08" ~ 97.0,
      product_id == "P09" ~ 97.0,
      product_id == "P10" ~ 3.7,
      product_id == "P11" ~ 32.0,
      product_id == "P12" ~ 7.1,
      product_id == "P13" ~ 38.0,
      product_id == "P14" ~ 75.0,
      product_id == "P15" ~ 82.0,
      product_id == "P16" ~ 4.3,
      product_id == "P17" ~ 12.0,
      product_id == "P18" ~ 6.0,
      product_id == "P19" ~ 0.0,
      product_id == "P20" ~ 0.0,
      product_id == "P21" ~ 0.0,
      product_id == "P22" ~ 0.9,
      product_id == "P23" ~ 18.0,
      product_id == "P24" ~ 15.3,
      product_id == "P25" ~ 0.2,
      product_id == "P26" ~ 1.0,
      product_id == "P27" ~ 0.5,
      product_id == "P28" ~ 14.0,
      product_id == "P29" ~ 34.0,
      product_id == "P30" ~ 25.7,
      product_id == "P31" ~ 5.0,
      product_id == "P32" ~ 18.2,
      product_id == "P33" ~ 5.6,
      product_id == "P34" ~ 55.4,
      product_id == "P35" ~ 45.0,
      product_id == "P36" ~ 47.4,
      TRUE ~ fats
    ),
    saturated   = case_when(
      product_id == "P01" ~ 0.2,
      product_id == "P02" ~ 0.3,
      product_id == "P03" ~ 0.7,
      product_id == "P04" ~ 0.1,
      product_id == "P05" ~ 1.2,
      product_id == "P06" ~ 2.5,
      product_id == "P07" ~ 8.7,
      product_id == "P08" ~ 45.0,
      product_id == "P09" ~ 9.3,
      product_id == "P10" ~ 2.4,
      product_id == "P11" ~ 21.0,
      product_id == "P12" ~ 4.9,
      product_id == "P13" ~ 8.7,
      product_id == "P14" ~ 35.0,
      product_id == "P15" ~ 58.0,
      product_id == "P16" ~ 0.4,
      product_id == "P17" ~ 3.9,
      product_id == "P18" ~ 2.4,
      product_id == "P19" ~ 0.0,
      product_id == "P20" ~ 0.0,
      product_id == "P21" ~ 0.0,
      product_id == "P22" ~ 0.2,
      product_id == "P23" ~ 2.2,
      product_id == "P24" ~ 3.8,
      product_id == "P25" ~ 0.0,
      product_id == "P26" ~ 0.2,
      product_id == "P27" ~ 0.1,
      product_id == "P28" ~ 1.8,
      product_id == "P29" ~ 4.8,
      product_id == "P30" ~ 7.2,
      product_id == "P31" ~ 1.6,
      product_id == "P32" ~ 5.8,
      product_id == "P33" ~ 2.0,
      product_id == "P34" ~ 4.7,
      product_id == "P35" ~ 4.9,
      product_id == "P36" ~ 47.4,
      TRUE ~ saturated
    ),
    carbs  = case_when(
      product_id == "P01" ~ 42.1,
      product_id == "P02" ~ 45.0,
      product_id == "P03" ~ 44.9,
      product_id == "P04" ~ 4.5,
      product_id == "P05" ~ 4.5,
      product_id == "P06" ~ 4.6,
      product_id == "P07" ~ 0.6,
      product_id == "P08" ~ 0.1,
      product_id == "P09" ~ 1.1,
      product_id == "P10" ~ 1.6,
      product_id == "P11" ~ 0.0,
      product_id == "P12" ~ 2.7,
      product_id == "P13" ~ 1.3,
      product_id == "P14" ~ 0.6,
      product_id == "P15" ~ 1.0,
      product_id == "P16" ~ 25.7,
      product_id == "P17" ~ 26.0,
      product_id == "P18" ~ 30.0,
      product_id == "P19" ~ 0.0,
      product_id == "P20" ~ 6.1,
      product_id == "P21" ~ 8.6,
      product_id == "P22" ~ 85.2,
      product_id == "P23" ~ 54.0,
      product_id == "P24" ~ 58.0,
      product_id == "P25" ~ 5.6,
      product_id == "P26" ~ 14.5,
      product_id == "P27" ~ 14.6,
      product_id == "P28" ~ 43.0,
      product_id == "P29" ~ 32.0,
      product_id == "P30" ~ 47.8,
      product_id == "P31" ~ 0.0,
      product_id == "P32" ~ 0.0,
      product_id == "P33" ~ 0.7,
      product_id == "P34" ~ 3.8,
      product_id == "P35" ~ 28.0,
      product_id == "P36" ~ 20.8,
      TRUE ~ carbs
    ),
    sugar   = case_when(
      product_id == "P01" ~ 1.0,
      product_id == "P02" ~ 2.7,
      product_id == "P03" ~ 1.3,
      product_id == "P04" ~ 4.5,
      product_id == "P05" ~ 4.5,
      product_id == "P06" ~ 4.6,
      product_id == "P07" ~ 0.6,
      product_id == "P08" ~ 0.1,
      product_id == "P09" ~ 1.0,
      product_id == "P10" ~ 1.6,
      product_id == "P11" ~ 0.0,
      product_id == "P12" ~ 2.7,
      product_id == "P13" ~ 0.5,
      product_id == "P14" ~ 0.6,
      product_id == "P15" ~ 1.0,
      product_id == "P16" ~ 4.3,
      product_id == "P17" ~ 3.9,
      product_id == "P18" ~ 2.0,
      product_id == "P19" ~ 0.0,
      product_id == "P20" ~ 6.1,
      product_id == "P21" ~ 8.6,
      product_id == "P22" ~ 1.0,
      product_id == "P23" ~ 5.6,
      product_id == "P24" ~ 16.6,
      product_id == "P25" ~ 4.0,
      product_id == "P26" ~ 14.5,
      product_id == "P27" ~ 12.2,
      product_id == "P28" ~ 6.1,
      product_id == "P29" ~ 17.0,
      product_id == "P30" ~ 21.9,
      product_id == "P31" ~ 0.0,
      product_id == "P32" ~ 0.0,
      product_id == "P33" ~ 0.2,
      product_id == "P34" ~ 3.8,
      product_id == "P35" ~ 19.0,
      product_id == "P36" ~ 5.0,
      TRUE ~ sugar
    ),
    fiber   = case_when(
      product_id == "P01" ~ 8.0,
      product_id == "P02" ~ 4.4,
      product_id == "P03" ~ 2.9,
      product_id == "P04" ~ 0.0,
      product_id == "P05" ~ 0.0,
      product_id == "P06" ~ 0.0,
      product_id == "P07" ~ 0.0,
      product_id == "P08" ~ 0.0,
      product_id == "P09" ~ 0.0,
      product_id == "P10" ~ 0.0,
      product_id == "P11" ~ 0.0,
      product_id == "P12" ~ 0.0,
      product_id == "P13" ~ 0.0,
      product_id == "P14" ~ 0.0,
      product_id == "P15" ~ 0.0,
      product_id == "P16" ~ 3.1,
      product_id == "P17" ~ 2.3,
      product_id == "P18" ~ 2.1,
      product_id == "P19" ~ 0.0,
      product_id == "P20" ~ 0.0,
      product_id == "P21" ~ 0.0,
      product_id == "P22" ~ 3.1,
      product_id == "P23" ~ 12.0,
      product_id == "P24" ~ 13.1,
      product_id == "P25" ~ 1.4,
      product_id == "P26" ~ 2.3,
      product_id == "P27" ~ 4.1,
      product_id == "P28" ~ 22.0,
      product_id == "P29" ~ 13.0,
      product_id == "P30" ~ 7.1,
      product_id == "P31" ~ 2.5,
      product_id == "P32" ~ 0.5,
      product_id == "P33" ~ 1.8,
      product_id == "P34" ~ 8.2,
      product_id == "P35" ~ 7.4,
      product_id == "P36" ~ 3.8,
      TRUE ~ fiber
    ),
    protein   = case_when(
      product_id == "P01" ~ 9.5,
      product_id == "P02" ~ 9.3,
      product_id == "P03" ~ 10.7,
      product_id == "P04" ~ 3.5,
      product_id == "P05" ~ 3.5,
      product_id == "P06" ~ 3.6,
      product_id == "P07" ~ 0.4,
      product_id == "P08" ~ 0.1,
      product_id == "P09" ~ 0.7,
      product_id == "P10" ~ 12.0,
      product_id == "P11" ~ 25.0,
      product_id == "P12" ~ 16.0,
      product_id == "P13" ~ 0.5,
      product_id == "P14" ~ 0.5,
      product_id == "P15" ~ 0.5,
      product_id == "P16" ~ 7.3,
      product_id == "P17" ~ 11.0,
      product_id == "P18" ~ 8.7,
      product_id == "P19" ~ 0.0,
      product_id == "P20" ~ 0.0,
      product_id == "P21" ~ 0.7,
      product_id == "P22" ~ 6.8,
      product_id == "P23" ~ 12.0,
      product_id == "P24" ~ 8.6,
      product_id == "P25" ~ 1.4,
      product_id == "P26" ~ 4.8,
      product_id == "P27" ~ 5.4,
      product_id == "P28" ~ 10.0,
      product_id == "P29" ~ 17.0,
      product_id == "P30" ~ 12.1,
      product_id == "P31" ~ 18.7,
      product_id == "P32" ~ 16.2,
      product_id == "P33" ~ 19.0,
      product_id == "P34" ~ 25.4,
      product_id == "P35" ~ 16.0,
      product_id == "P36" ~ 21.2,
      TRUE ~ protein
    ),
    sodium   = case_when(
      product_id == "P01" ~ 0.9,
      product_id == "P02" ~ 0.9,
      product_id == "P03" ~ 1.0,
      product_id == "P04" ~ 0.1,
      product_id == "P05" ~ 0.1,
      product_id == "P06" ~ 0.1,
      product_id == "P07" ~ 0.1,
      product_id == "P08" ~ 0.6,
      product_id == "P09" ~ 0.4,
      product_id == "P10" ~ 0.8,
      product_id == "P11" ~ 2.2,
      product_id == "P12" ~ 2.1,
      product_id == "P13" ~ 0.1,
      product_id == "P14" ~ 0.9,
      product_id == "P15" ~ 0.0,
      product_id == "P16" ~ 0.8,
      product_id == "P17" ~ 1.1,
      product_id == "P18" ~ 0.8,
      product_id == "P19" ~ 0.1,
      product_id == "P20" ~ 0.1,
      product_id == "P21" ~ 0.1,
      product_id == "P22" ~ 0.1,
      product_id == "P23" ~ 0.1,
      product_id == "P24" ~ 0.1,
      product_id == "P25" ~ 0.1,
      product_id == "P26" ~ 1.0,
      product_id == "P27" ~ 2.0,
      product_id == "P28" ~ 0.1,
      product_id == "P29" ~ 0.1,
      product_id == "P30" ~ 1.0,
      product_id == "P31" ~ 0.2,
      product_id == "P32" ~ 0.2,
      product_id == "P33" ~ 1.4,
      product_id == "P34" ~ 0.0,
      product_id == "P35" ~ 16.0,
      product_id == "P36" ~ 0.8,
      TRUE ~ sodium
    )  )
################################################################################
Products <- Products %>%
  group_by(category) %>%
  mutate(ChoiceNumber = row_number()) %>%
  ungroup() %>%
  mutate(CategoryID = rep(1:12, each = 3))  


Choices <- Data_CSV %>%
  mutate(RespondentID = row_number()) %>%  # Create a respondent ID
  select(RespondentID, risk_perception, Q1:Q24)  # Then select the columns you need
Choices <- Choices %>%
  pivot_longer(cols = starts_with("Q"),
               names_to = "Question",
               values_to = "ChosenProduct")
Choices <- Choices %>%
  mutate(Group = ifelse(Question %in% paste0("Q", 1:12), 0, 1))
Choices <- Choices %>%
  mutate(Question = ifelse(
    Question %in% paste0("Q", 13:24),
    paste0("Q", as.numeric(gsub("Q", "", Question)) - 12),
    Question
  ))
Choices <- Choices %>%
  filter(!is.na(ChosenProduct) & ChosenProduct != "")

Choices <- Choices %>%
  mutate(CategoryID = as.numeric(gsub("Q", "", Question)))

Choices <- Choices %>% mutate(ChosenProduct = as.integer(ChosenProduct))

Products$product_id <- as.integer(gsub("^P", "", Products$product_id))
Choices$ChosenProduct <- as.integer(Choices$ChosenProduct)

# Step 1:
Respondent_id <- unique(Choices$RespondentID)
product_id <- unique(Products$product_id)
merged <- expand.grid(RespondentID = Respondent_id, product_id = product_id)

# Step 2: Add category info to products and join it to the grid
merged <- merged %>%
  left_join(Products, by = "product_id")

# Step 3: Mark choices with a flag (Chosen = 1)
Choices <- Choices %>%
  mutate(Chosen = 1) %>%
  rename(product_id = ChosenProduct)  
# Step extra:
Choices <- Choices %>%
  mutate(product_id = (CategoryID - 1) * 3 + product_id)

# Step 4: Merge choices into full grid
merged <- merged %>%
  left_join(Choices %>% select(RespondentID, CategoryID, product_id, Chosen),
            by = c("RespondentID", "CategoryID", "product_id"))
group_map <- Choices %>%
  distinct(RespondentID, CategoryID, Group)

merged <- merged %>%
  left_join(group_map, by = c("RespondentID", "CategoryID"))

# Step 5: Replace NAs with 0 (not chosen)
merged <- merged %>%
  mutate(Chosen = ifelse(is.na(Chosen), 0, Chosen))
################################################################################
cols_to_convert <- paste0("Q", 1:24)
Data_CSV[cols_to_convert] <- lapply(Data_CSV[cols_to_convert], function(x) as.numeric(as.character(x)))
Data_CSV[cols_to_convert] <- lapply(Data_CSV[cols_to_convert], function(x) ifelse(x == 1, 1, 0))

Data_CSV$control <- rowSums(Data_CSV[, paste0("Q", 1:12)], na.rm = TRUE)
Data_CSV$intervention <- rowSums(Data_CSV[, paste0("Q", 13:24)], na.rm = TRUE)
Data_Scores <- Data_CSV[, !(names(Data_CSV) %in% cols_to_convert)]
Data_Scores <- Data_Scores[, c("control", "intervention", setdiff(names(Data_Scores), c("control", "intervention")))]
################################################################################

#Q25
Data_Scores$Q25 <- as.numeric(gsub("[^0-9]", "", Data_Scores$Q25))
Data_Scores$Q25 <- ifelse(is.na(Data_Scores$Q25), 0, ifelse(Data_Scores$Q25 == 500, 1, 0))
Data_Scores$Q25[10] <- 1
#Q26
Data_Scores$Q26 <- as.numeric(gsub("[^0-9]", "", Data_Scores$Q26))
Data_Scores$Q26 <- ifelse(is.na(Data_Scores$Q26), 0, ifelse(Data_Scores$Q26 == 10, 1, 0))
#Q27
Data_Scores$Q27 <- gsub("[^0-9,\\.]", "", Data_Scores$Q27)
Data_Scores$Q27 <- gsub(",", ".", Data_Scores$Q27)
Data_Scores$Q27 <- as.numeric(Data_Scores$Q27)
Data_Scores$Q27 <- ifelse(substr(Data_Scores$Q27, 1, 1) == ".", paste0("0", Data_Scores$Q27), Data_Scores$Q27)
Data_Scores$Q27 <- ifelse(Data_Scores$Q27 == 0.1, 1, 0)
#Q28
Data_Scores$Q28 <- as.numeric(Data_Scores$Q28)
Data_Scores$Q28 <- ifelse(Data_Scores$Q28 == 3, 1, 0)
#Q29
Data_Scores$Q29 <- as.numeric(Data_Scores$Q29)
Data_Scores$Q29 <- ifelse(Data_Scores$Q29 == 2, 1, 0)
#Q30
Data_Scores$Q30 <- substr(Data_Scores$Q30, 1, 3)
Data_Scores$Q30 <- gsub("[^0-9]", "", Data_Scores$Q30)
Data_Scores$Q30 <- as.numeric(Data_Scores$Q30)
Data_Scores$Q30 <- ifelse(is.na(Data_Scores$Q30), 0, ifelse(Data_Scores$Q30 == 2, 1, 0))
#Q31
Data_Scores$Q31 <- substr(Data_Scores$Q31, 1, 11)
Data_Scores$Q31[c(13, 60, 99,105,111,115)] <- "2/100"
Data_Scores$Q31 <- gsub("op", "/", Data_Scores$Q31, ignore.case = TRUE)
Data_Scores$Q31 <- gsub("%", "/100", Data_Scores$Q31)
Data_Scores$Q31 <- gsub("[^0-9/]", "", Data_Scores$Q31)
Data_Scores$Q31 <- ifelse(Data_Scores$Q31 == "2", "2/100", Data_Scores$Q31)
Data_Scores$Q31 <- ifelse(Data_Scores$Q31 %in% c("2/100", "1/50"), 1, 0)
#Q32
Data_Scores$Q32 <- tolower(Data_Scores$Q32)
Data_Scores$Q32 <- gsub("\\btien\\b", "10", Data_Scores$Q32)
Data_Scores$Q32 <- as.numeric(gsub("[^0-9]", "", Data_Scores$Q32))
Data_Scores$Q32 <- ifelse(Data_Scores$Q32 == 10, 1, 0)
#Q33
Data_Scores$Q33 <- tolower(Data_Scores$Q33)
Data_Scores$Q33 <- gsub("\\bhonderd\\b", "100", Data_Scores$Q33)
Data_Scores$Q33 <- gsub("[^0-9]", "", Data_Scores$Q33)
Data_Scores$Q33 <- ifelse(Data_Scores$Q33 == "100", 1, 0)
#Q34
Data_Scores$Q34 <- gsub("[^0-9]", "", Data_Scores$Q34)
Data_Scores$Q34 <- ifelse(Data_Scores$Q34 == "20", 1, 0)
#Q35
Data_Scores$Q35 <- gsub("[^0-9]", "", Data_Scores$Q35)
Data_Scores$Q35 <- ifelse(Data_Scores$Q35 == "116" | Data_Scores$Q35 == "114", "5", Data_Scores$Q35)
Data_Scores$Q35 <- ifelse(Data_Scores$Q35 == "5", 1, 0)
#Q36

colnames(Data_Scores)[which(names(Data_Scores) %in% paste0("Q", 25:35))] <- paste0("math", 1:11)#Q54


################################################################################
#GRAPH
Data_Scores$Q49 <- gsub("[^0-9]", "", Data_Scores$Q49)
Data_Scores$Q49 <- ifelse(Data_Scores$Q49 == "25", 1, 0)
#Q54
Data_Scores$Q54 <- as.numeric(gsub("[^0-9]", "", Data_Scores$Q54))
Data_Scores$Q54 <- ifelse(Data_Scores$Q54 == "3", 1, 0)
#Q55
Data_Scores$Q55 <- substr(Data_Scores$Q55, 1, 4)
Data_Scores$Q55 <- gsub("[^0-9]", "", Data_Scores$Q55)
Data_Scores$Q55 <- ifelse(Data_Scores$Q55 == "20", 1, 0)
#Q56
Data_Scores$Q56 <- as.numeric(gsub("[^0-9]", "", Data_Scores$Q56))
Data_Scores$Q56 <- ifelse(Data_Scores$Q56 == "4", 1, 0)
#SCORES

colnames(Data_Scores)[colnames(Data_Scores) == "Q49"] <- "graph1"
colnames(Data_Scores)[colnames(Data_Scores) == "Q54"] <- "graph2"
colnames(Data_Scores)[colnames(Data_Scores) == "Q55"] <- "graph3"
colnames(Data_Scores)[colnames(Data_Scores) == "Q56"] <- "graph4"
colnames(Data_Scores)[colnames(Data_Scores) == "Q50"] <- "sex"
colnames(Data_Scores)[colnames(Data_Scores) == "Q51"] <- "age"
colnames(Data_Scores)[colnames(Data_Scores) == "Q52"] <- "education"

Data_Scores$math_total <- rowSums(Data_Scores[, paste0("math", 1:11)], na.rm = TRUE)
Data_Scores$graph_total <- rowSums(Data_Scores[, paste0("graph", 1:4)], na.rm = TRUE)
Data_Scores <- Data_Scores %>%
  relocate(math_total, .after = math11) %>%
  relocate(graph_total, .after = graph4)
Data_Scores <- Data_Scores %>%
  mutate(group = ifelse(control > 0, 0,ifelse(intervention > 0, 1, NA)),
    score = ifelse(control > 0, control, intervention))
Data_Scores <- Data_Scores %>%
  select(group, score, everything(), -control, -intervention)

colnames(Data_Scores)[colnames(Data_Scores) == "Duration..in.seconds."] <- "duration"
Data_Scores$duration <- as.numeric(Data_Scores$duration)
Data_Scores$age <- as.numeric(as.character(Data_Scores$age))

################################################################################
#RQ1
Model1A<-t.test(score ~ group, data = Data_Scores)
Model1A
var.test(score ~ group, data = Data_Scores)

Model1B<-lm(score ~ group, data = Data_Scores)
summary(Model1B)
plot(Model1B)


Model1C<-lm(score ~ group + sex + age + education, data = Data_Scores)
summary(Model1C)
vif(Model1C)
#RQ1_plot
ggplot(Data_Scores, aes(x = factor(group, labels = c("Control", "Intervention")), y = score)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", width = 0.6) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Average Healthiest Product Identification by Group",
       x = "Table Format Group", y = "Average Score (0–12)") +
  ylim(0, 11.5) +
  theme_minimal()
################################################################################
#RQ2
colSums(is.na(Data_Scores[c("group", "risk_perception", "score")]))

#med model
med_model <- lm(risk_perception ~ group, data = Data_Scores)
summary(med_model)
#outcome model (with mediator)
out_model <- lm(score ~ group + risk_perception, data = Data_Scores)
summary(out_model)

#mediation analysis
med_out <- mediate(med_model, out_model, treat = "group", mediator = "risk_perception", boot = TRUE)
summary(med_out)

#Assumptions
colSums(is.na(Data_Scores[c("group", "risk_perception", "score")]))

# Diagnostic plots for mediator model
par(mfrow = c(2, 2))
plot(med_model)  # residual plots for risk_perception ~ group

# Diagnostic plots for outcome model
par(mfrow = c(2, 2))
plot(out_model)  # residual plots for score ~ group + risk_perception

# Normality test for mediator model residuals
shapiro.test(residuals(med_model))

# Normality test for outcome model residuals
shapiro.test(residuals(out_model))

# For mediator model
bptest(med_model)

# For outcome model
bptest(out_model)

vif(out_model)

#plots
p1 <- ggplot(Data_Scores, aes(x = score, y = risk_perception, color = factor(group))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0, 11.5)) +
  scale_y_continuous(breaks = 1:7, limits = c(1, 7)) +
  labs(title = "Score and Risk Perception by Group",
       x = "Score (0–12)", y = "Risk Perception (1–7)", color = "Group") +
  theme_minimal()

# Boxplot of risk perception by group
p2 <- ggplot(Data_Scores, aes(x = factor(group, labels = c("Control", "Intervention")), y = risk_perception, fill = factor(group))) +
  geom_boxplot() +
  scale_y_continuous(breaks = 1:7, limits = c(1, 7)) +
  labs(
    title = "Risk Perception by Table Format Group",
    x = "Group",
    y = "Risk Perception (1–7)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggarrange(p2, p1, ncol = 2)

ggplot(Data_Scores, aes(x = risk_perception, fill = factor(group, labels = c("Control", "Intervention")))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  coord_flip() +
  labs(
    title = "Distribution of Risk Perception by Group",
    x = "Risk Perception (1 = Low, 7 = High)",
    y = "Count",
    fill = "Group"
  ) +
  theme_minimal()
################################################################################
#EXTRA
merged$choice_set <- interaction(merged$RespondentID, merged$CategoryID)
modelX <- clogit(Chosen ~ fats + saturated + carbs + sugar + protein + fiber + 
                   sodium + strata(choice_set), data = merged)

summary(modelX)

model_interactionX <- clogit(
  Chosen ~ fats * Group + saturated * Group + carbs * Group + 
    sugar * Group + protein * Group + fiber * Group + sodium * Group + 
    strata(choice_set),
  data = merged)
summary(model_interactionX)
################################################################################
#RQ4A
Data_Scores$math_c <- scale(Data_Scores$math_total, center = TRUE, scale = FALSE)

mod_model_tableA <- lm(score ~ group * math_c, data = Data_Scores)
summary(mod_model_tableA)

Data_Scores$math_c <- as.numeric(scale(Data_Scores$math_total, center = TRUE, scale = FALSE))
mod_model_tableA <- lm(score ~ group * math_c, data = Data_Scores)
summary(mod_model_tableA)

#Assumptions
par(mfrow = c(2, 2))
plot(mod_model_tableA)
bptest(mod_model_tableA)
shapiro.test(residuals(mod_model_tableA))
vif(mod_model_tableA)


#plots
pred <- ggpredict(mod_model_tableA, terms = c("math_c", "group"))

ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(title = "Predicted Scores by Numeracy and Group",
       x = "Numeracy (Centered)",
       y = "Predicted Score",
       color = "Group", fill = "Group") +
  scale_y_continuous(limits = c(0, 12)) +
  theme_minimal()
summary(mod_model_tableA)


#RQ4B
med_model4b <- lm(risk_perception ~ group * math_c, data = Data_Scores)
out_model4b <- lm(score ~ group + risk_perception + math_c, data = Data_Scores)
mod_med_out4b <- mediate(med_model4b, out_model4b, treat = "group", mediator = "risk_perception", 
                       covariates = c("math_c"), boot = TRUE)
summary(mod_med_out4b)

#Assumptions
par(mfrow = c(2, 2))
plot(med_model4b)  # for the mediator model
plot(out_model4b)  # for the outcome model
# Normality of residuals
shapiro.test(residuals(med_model4b))
shapiro.test(residuals(out_model4b))
# Homoscedasticity
bptest(med_model4b)
bptest(out_model4b)
# Multicollinearity
vif(med_model4b)
vif(out_model4b)


################################################################################
#RQ5A
Data_Scores$graph_c <- as.numeric(scale(Data_Scores$graph_total, center = TRUE, scale = FALSE)[, 1])

mod_model_graphA <- lm(score ~ group * graph_c, data = Data_Scores)
summary(mod_model_graphA)

# Model for group 0
mod_group0 <- lm(score ~ graph_c, data = subset(Data_Scores, group == 0))
# Model for group 1
mod_group1 <- lm(score ~ graph_c, data = subset(Data_Scores, group == 1))
#Summaries
summary(mod_group0)
summary(mod_group1)

#Assumptions
par(mfrow = c(2, 2))
plot(mod_model_graphA)
shapiro.test(residuals(mod_model_graphA))
bptest(mod_model_graphA)
vif(mod_model_graphA)


#plot
pred_graph <- ggpredict(mod_model_graphA, terms = c("graph_c", "group"))
ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "Interaction Effect: Group × Graphical Literacy",
    x = "Graphical Literacy (Centered)",
    y = "Predicted Score",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 12))  # adjust as needed


#RQ5B
med_model_graph5b <- lm(risk_perception ~ group * graph_c, data = Data_Scores)
out_model_graph5b <- lm(score ~ group + risk_perception + graph_c, data = Data_Scores)
mod_med_out_graph5b <- mediate(med_model_graph5b, out_model_graph5b, treat = "group", mediator = "risk_perception", 
                             covariates = c("graph_c"), boot = TRUE)
summary(mod_med_out_graph5b)

#Assumptions
par(mfrow = c(2, 2))
plot(med_model_graph5b)
plot(out_model_graph5b)
shapiro.test(residuals(med_model_graph5b))
shapiro.test(residuals(out_model_graph5b))
bptest(med_model_graph5b)
bptest(out_model_graph5b)
vif(out_model_graph5b)

#Robust Standard Errors
coeftest(med_model_graph5b, vcov = vcovHC(med_model_graph5b, type = "HC3"))


################################################################################
#EXTRA
mod_time <- lm(score ~ group * duration, data = Data_Scores)
summary(mod_time)

mod_sex_age_education <- lm(score ~ sex + age + education, data = Data_Scores)
summary(mod_sex_age_education)

mod_interaction <- lm(score ~ sex * age * education, data = Data_Scores)
summary(mod_interaction)


################################################################################
#Math and graph
#sex
table(Data_Scores$sex)         # Check distribution of sex (should be coded 1 and 2)
table(Data_Scores$education)   # Check education levels
df_sex12 <- subset(Data_Scores, sex %in% c(1, 2))

t.test(math_total ~ sex, data = df_sex12)
t.test(graph_total ~ sex, data = df_sex12)

cor.test(Data_Scores$math_total, Data_Scores$age, use = "complete.obs")
cor.test(Data_Scores$graph_total, Data_Scores$age, use = "complete.obs")

cor.test(Data_Scores$math_total, Data_Scores$graph_total, use = "complete.obs")


ggplot(Data_Scores, aes(x = math_total, y = graph_total)) +
  geom_jitter(width = 0.3, height = 0.3, size = 2, alpha = 0.7) +  # adds jitter
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Correlation Between Math and Graph Scores",
       x = "Math Score",
       y = "Graph Score") +
  theme_minimal()


df_cluster <- na.omit(Data_Scores[, c("math_total", "graph_total")])
df_scaled <- scale(df_cluster)
wss <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  kmeans_model <- kmeans(df_scaled, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Plot for Optimal k")
abline(v = which.min(diff(diff(wss))), col = "red", lty = 2)


df_cluster <- Data_Scores[, c("math_total", "graph_total")]
df_cluster <- na.omit(df_cluster)  # Remove missing values if needed
df_scaled <- scale(df_cluster)
set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 3)  # Try 3 clusters
Data_Scores$cluster <- as.factor(kmeans_result$cluster)


ggplot(Data_Scores, aes(x = math_total, y = graph_total, color = cluster)) +
  geom_jitter(width = 0.3, height = 0.3, size = 2) +
  labs(title = "Clusters Based on Math and Graph Scores") +
  theme_minimal()
################################################################################
#Math and Graph 2

median_math <- median(Data_Scores$math_total, na.rm = TRUE)

Data_Scores <- Data_Scores %>%
  mutate(
    numeracy_group = if_else(math_total >= median_math, "High numeracy", "Low numeracy")
  )
# Reshape to long format to analyze item-level performance
math_items <- paste0("math", 1:11)

math_long <- Data_Scores %>%
  pivot_longer(
    cols = all_of(math_items),
    names_to = "item",
    values_to = "item_score"
  )

ggplot(math_long, aes(x = item, y = item_score, fill = numeracy_group)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.7)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.7), width = 0.2) +
  labs(
    x = "Math Item",
    y = "Proportion Correct",
    fill = "Numeracy Group"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Dark2")

graph_items <- paste0("graph", 1:4)
graph_long <- Data_Scores %>%
  pivot_longer(
    cols = all_of(graph_items),
    names_to = "item",
    values_to = "item_score"
  )
ggplot(graph_long, aes(x = item, y = item_score, fill = numeracy_group)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.7)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.7), width = 0.2) +
  labs(
    x = "Graph Item",
    y = "Proportion Correct",
    fill = "Numeracy Group"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Dark2")
################################################################################
#Low and high numeracy with RQ4 and 5
# Create binary groups from median split
median_math <- median(Data_Scores$math_total, na.rm = TRUE)
median_graph <- median(Data_Scores$graph_total, na.rm = TRUE)
#RQ4a
Data_Scores <- Data_Scores %>%
  mutate(
    numeracy_group = factor(if_else(math_total >= median_math, "High numeracy", "Low numeracy")),
    graph_group = factor(if_else(graph_total >= median_graph, "High graph literacy", "Low graph literacy"))
  )
mod_rq4a_group <- lm(score ~ group * numeracy_group, data = Data_Scores)
summary(mod_rq4a_group)
#RQ4b
med_model_rq4b <- lm(risk_perception ~ group * numeracy_group, data = Data_Scores)
out_model_rq4b <- lm(score ~ group + risk_perception + numeracy_group, data = Data_Scores)
med_result_rq4b <- mediate(
  model.m = med_model_rq4b,
  model.y = out_model_rq4b,
  treat = "group",
  mediator = "risk_perception",
  boot = TRUE
)
summary(med_result_rq4b)
#RQ5a
mod_rq5a_group <- lm(score ~ group * graph_group, data = Data_Scores)
summary(mod_rq5a_group)

#RQ5b
med_model_rq5b <- lm(risk_perception ~ group * graph_group, data = Data_Scores)
out_model_rq5b <- lm(score ~ group + risk_perception + graph_group, data = Data_Scores)

med_result_rq5b <- mediate(
  model.m = med_model_rq5b,
  model.y = out_model_rq5b,
  treat = "group",
  mediator = "risk_perception",
  boot = TRUE
)

summary(med_result_rq5b)


################################################################################
#Factor dataframe
merged_questions <- list()
for (i in 1:12) {
  merged_questions[[i]] <- ifelse(
    is.na(Data_CSV[[paste0("Q", i)]]),
    Data_CSV[[paste0("Q", i + 12)]],
    Data_CSV[[paste0("Q", i)]]
  )
}
Factor <- as.data.frame(merged_questions)
colnames(Factor) <- paste0("Q", 1:12)
Factor$group <- ifelse(!is.na(Data_CSV$Q1), 0, 1)

#Full sample factor analysis
# Subset only Q1 to Q12
Factorchoice <- Factor[, paste0("Q", 1:12)]
# Step 1: Compute tetrachoric correlations
tetra <- tetrachoric(Factorchoice)$rho
fa.parallel(tetra, fa = "fa", n.iter = 100, fm = "minres")  # minres = method used in your fa()

# Step 2: Factor analysis (choose number of factors, e.g., 2)
fc_result <- fa(tetra, nfactors = 2, rotate = "oblimin")
print(fc_result, cut = 0.3)

factor_scores <- factor.scores(Factorchoice, fc_result, method = "Thurstone")

# Factor Predict

Factor$scores1 <- factor_scores$scores[, 1]  # Factor 1
Factor$scores2 <- factor_scores$scores[, 2]  # Factor 2
Data_Scores$scores1 <- Factor$scores1
Data_Scores$scores2 <- Factor$scores2

model_score <- lm(score ~ scores1 + scores2 + group, data = Data_Scores)
summary(model_score)

