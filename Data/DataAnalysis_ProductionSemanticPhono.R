#Data cleaning and analysis for Productoin by typing using semantically and phonologically
#similar words. Lists are presented at study, where critical lures are presented at test
#study words are words taken from Watson, Balota, & Roediger, 2003

library("tidyverse")
library(tidyr)
library(plyr)
#install.packages("jsonlite")
library("jsonlite")
data<-unlist(jsonlite::fromJSON("/Users/jackiespear/ProductionTypingSemanticPhono.json"), recursive = FALSE, use.names = TRUE)
#install.packages("dplyr")
library(dplyr)
data <- bind_rows(data)

#View(data)
#data<-flatten(x = data, recursive = TRUE)

total_subjects_run <- length(unique(data$subject))
total_subjects_run
#dat_study[dat_study$category %in% c("produce"), ]

#data <- data[!grepl("<p style='font-size: 28px; color: red;'>", data$instruc),] #this reomves the entire row
#data$stimulus <- gsub('<span style="color: green">', "",as.character(data$word2)) #removing the html formatting from read/produce instruction
#data$stimulus <- gsub('<span style="color: red">', "",as.character(data$word2))
#data$stimulus <- gsub("</span></p>", "",as.character(data$word2))
data$response <- gsub('list\\(response \\= "', "",as.character(data$response))
data$response <- gsub('"\\)', "",as.character(data$response))
data$response <- gsub('list\\(gender \\= "', "",as.character(data$response))
data$response <- gsub('list\\(age \\= "', "",as.character(data$response))
#data$stimulus <- gsub('<p style="font-size: 48px;">', "",as.character(data$stimulus))
#data$stimulus <- gsub('</p>', "",as.character(data$stimulus))

#Counting the number of attention checks per subject
attention_checks <- data %>%
  group_by(subject) %>%
  dplyr::summarise(Number = sum(phase == "Attention_Check", na.rm=TRUE))

data <- data %>% unnest_wider(data, names_sep = "_")

data <- data %>% 
  dplyr::rename("condition" = "data_condition",
                "list" = "data_list",
                "type2" = "data_type",
                "word" = "data_word")

dat_test <- data[data$phase %in% c("test"), ]
dat_study <- data[data$phase %in% c("study"), ]
dat_demo <- data[data$phase %in% c("Demographics"),]
dat_demo <- dat_demo[,c('subject','type','response')]
#dat_demo< - dat_demo %>% 
#  pivot_wider(names_from = type, values_from = response)



dat_study_prod <- dat_study[dat_study$condition %in% c("phono_produced", "semantic_produced"), ]
dat_study_read <- dat_study[dat_study$condition %in% c("phono_read", "semantic_read"), ]

#getting number of "yes" responses for delinquent responding during test phase
button_mashing <- dat_test %>%
  group_by(subject) %>%
  dplyr::summarise(Yes_Button_Presses = sum(response == "y", na.rm=TRUE))

#counting the number of times s's did not type when they should have (for produced items)
not_typing_sums <- dat_study_prod %>%
  group_by(subject) %>%
  dplyr::summarise(Number = sum(response =="", na.rm=TRUE))

#number of times s's typed when they shouldn't have (for read items)
typing_sums <- dat_study_read %>%
  group_by(subject) %>%
  dplyr::summarise(Number = sum(response !="", na.rm=TRUE))

length(which(not_typing_sums$Number > 6)) #counting s's that did not TYPE at least 80% of the time to exclude
not_typing_sums$subject[not_typing_sums$Number > 6] #returning those subjects

length(which(not_typing_sums$Number > 6))
not_typing_sums$subject[not_typing_sums$Number > 6]

data_count_list <- aggregate(data = dat_test,         #counting how many s's in each list
                             subject ~ condition_list,
                             function(subject) length(unique(subject)))
data_count_list

dat_test = filter(dat_test, 
                  subject != "auqq5ew6xf7dycn" & #not typing
                  subject != "chvxq2n1n60c2fw", #not typing
                  subject != "08ffxnxuh4b909j", #not typing
                  subject != "tbuphun92wk0ehh", #not typing
                  subject != "ydgh8ann6o9b7b2", #not typing
                  subject != "px2q73zu4wjr4d5", #button mashing
                  subject != "lupm0ekc3cecc78", #button mashing
                  subject != "cbdwc5wmu3brvtd" #button mashing
)

data_count_list <- aggregate(data = dat_test,       #counting how many s's in each list
                             subject ~ condition_list,
                             function(subject) length(unique(subject)))
data_count_list


#########################################################################################################
#mean(full_data_test$response == "y")
#########################################################################################################
dat_test$response <- ifelse(dat_test$response =="y", 1, 0)

library(plotrix) #for standard error

Step1 <- dat_test %>% #getting hits and FAs for subjects
  group_by(subject, condition) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2 <- Step1 %>% group_by(condition) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

vec <- c("semantic_produced", "phono_produced", "semantic_read", "phono_read",
                     "semantic_produced_critical", "phonological_produced_critical", 
                     "semantic_unproduced_critical", "phonological_unproduced_critical",
                     "semantic_new_critical", "phonological_new_critical",
                     "semantic_new", "phono_new")

Step2_reordered <- Step2[match(vec, Step2$condition), ] 
Step2_produced <- Step2_reordered[1:2,]
Step2_read <- Step2_reordered[3:4,]
Step2_related <- Step2_reordered[5:8,]
Step2_unrelated <- Step2_reordered[9:12,]


G1 <- ggplot(Step2_produced, aes(x = reorder(condition, -means), y = means)) +
  geom_bar(stat = "identity", color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
)

G1 <- G1 + ggtitle("Produced") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(0,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_col() +
  scale_x_discrete(labels=c('Phono \n Produced \n', 'Semantic \n  Produced \n')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8)) 
  

G2 <- ggplot(Step2_read, aes(x = reorder(condition, -means), y = means)) +
  geom_bar(stat = "identity", color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G2 <- G2 + ggtitle("Read") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(0,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_col() +
  scale_x_discrete(labels=c('Phono \n Read \n', 'Semantic \n Read \n')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8)) 
  

G3 <- ggplot(Step2_related, aes(x = reorder(condition, -means), y = means)) +
  geom_bar(stat = "identity", color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G3 <- G3 + ggtitle("Related \n Lures") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(0,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_col() +
  scale_x_discrete(labels=c('Semantic \n Produced \n Critical', 'Phono \n Produced \n Critical', 'Semantic \n Read \n Critical', 'Phono \n Read \n Critical')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8))

G4 <- ggplot(Step2_unrelated, aes(x = reorder(condition, -means), y = means)) +
  geom_bar(stat = "identity", color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G4 <- G4 + ggtitle("Unrelated \n Lures") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(0,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_col() +
  scale_x_discrete(labels=c('Semantic \n New \n Critical', 'Phono \n New \n Critical', 'Semantic \n New', 'Phono \n New')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8))

library("gridExtra")
library("grid")
grid.arrange(G1, G2, G3, G4, ncol = 4, top=textGrob("Production by Typing with words \n Semantic & Phono \n Online"))







# library("ggsignif")
# G3 <- G3 + geom_signif(y_position=c(0.9), 
#                        xmin=c("produce", 1.8), 
#                        xmax=c("read", 2.2),annotation=c("*", "NS"), 
#                        tip_length=0.025)

dat_test$condition <- as.character(dat_test$condition)
dat_test$type2 <- as.character(dat_test$type2)

ANOVA <- aov(response ~ condition * type2, data = dat_test)
summary(ANOVA)

dat_test2 <- (dat_test[,-c(1:5,7:10,12:15,17:22)]) 

library(reshape2)
data_wide <- dcast(dat_test2, subject ~ condition, value.var="response",fun.aggregate=mean)
data_wide <- as.matrix(data_wide[,-1])   #delete subject column for contrasts
#reoder columns for contrasts

col_order <- c("semantic_produced", "phono_produced", "semantic_read", "phono_read",
                      "semantic_produced_critical", "phonological_produced_critical", 
                      "semantic_unproduced_critical", "phonological_unproduced_critical",
                      "semantic_new_critical", "phonological_new_critical",
                      "semantic_new", "phono_new")
data_wide <- data_wide[, col_order]
#conduct contrasts
t.test(as.matrix(data_wide) %*% c(1, 1, -1, -1, 0,0,0,0,0,0,0,0)) #produce vs. read
t.test(as.matrix(data_wide) %*% c(0,0,0,0, 1,1,1,1, -1,-1,-1,-1)) #related lures vs. unrelated lures
t.test(as.matrix(data_wide) %*% c(0,0,0,0, 1,-1,0,0, 0,0,0,0)) #semantic critical vs. phono critical
t.test(as.matrix(data_wide) %*% c(0,0,0,0, 1,1,-1,-1, 0,0,0,0)) #produced critical vs. read critical

##########################################################

write.csv(data_wide, "/Users/jackiespear/productionTypingSemanticPhono.csv", row.names=FALSE)



