# datasets = list.files(path = "./dataset", pattern = NULL, all.files = FALSE,
#            full.names = FALSE, recursive = FALSE,
#            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
d = read.csv("dataset/heart.csv")
# d2 = read.csv("dataset/heart_statlog.data.csv")
# heart<-d2 %>% mutate_if(is.integer, as.factor)
# heart<-d2 %>% mutate_if(is.numeric, as.factor)
# h2 = d2 %>% map_df(~class(.x)) %>%  gather(key="var", value = "class") 
# # class(heart$oldpeak)
# str(heart)


heart_disease_dataset <- d

data_temp = data.frame(heart_disease_dataset)

#--------------------------- Xu li dataset ----------------------
#kiem tra du lieu
head(heart_disease_dataset)
str(heart_disease_dataset)
colnames(heart_disease_dataset)
nrow(heart_disease_dataset)
dim(heart_disease_dataset)
#Prepare column names
names <- c("Age",
            "Sex",
            "Chest_Pain_Type",
            "Resting_Blood_Pressure",
            "Serum_Cholesterol",
            "Fasting_Blood_Sugar",
            "Resting_ECG",
            "Max_Heart_Rate_Achieved",
            "Exercise_Induced_Angina",
            "ST_Depression_Exercise",
            "Peak_Exercise_ST_Segment",
            "Num_Major_Vessels_Flouro",
            "Thalassemia",
            "Diagnosis_Heart_Disease")

#Apply column names to the dataframe
colnames(heart_disease_dataset) <- names

#Glimpse data to verify new column names are in place
heart_disease_dataset %>% glimpse()

#Determine the number of values in each level of dependent variable
#Thong ke cac chuan doan bi benh tim hay khong
heart_disease_dataset %>% 
    drop_na() %>%
    group_by(Diagnosis_Heart_Disease) %>%
    count() %>% 
    ungroup() %>%kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)

#Identify the different levels of Thalassemia
heart_disease_dataset %>% 
    drop_na() %>%
    group_by(Thalassemia) %>%
    count() %>% 
    ungroup() %>%
    kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)


#Drop NA's, convert to factors, lump target variable to 2 levels, remove "?", reorder variables
heart_dataset_clean_tbl <- heart_disease_dataset %>%
    drop_na() %>%
    mutate_at(c("Resting_ECG", 
                "Fasting_Blood_Sugar", 
                "Sex", 
                "Diagnosis_Heart_Disease",
                "Thalassemia",
                "Exercise_Induced_Angina",
                "Peak_Exercise_ST_Segment", 
                "Chest_Pain_Type"), as_factor) %>%
    mutate(Num_Major_Vessels_Flouro = as.numeric(Num_Major_Vessels_Flouro)) %>%
    mutate(Diagnosis_Heart_Disease = fct_lump(Diagnosis_Heart_Disease,n = 1, other_level = "1")) %>% 
    filter(!Thalassemia %in% c("?",0, 4, 5)) %>%
    select(Age, 
            Resting_Blood_Pressure, 
            Serum_Cholesterol, 
            Max_Heart_Rate_Achieved, 
            ST_Depression_Exercise,
            Num_Major_Vessels_Flouro,
            everything())
#
#Glimpse data
heart_dataset_clean_tbl %>%
    glimpse()

#---------------------Check ----------------
#heart_disease_dataset$Age[1]

#Lay ra cot tuoi va bi benh
heart_dataset_clean_tbl[,c("Age", "Diagnosis_Heart_Disease"), drop = FALSE]
#Chuyen qua string
heart_dataset_clean_str = heart_dataset_clean_tbl  %>%
  select(Sex,
         Chest_Pain_Type,
         Fasting_Blood_Sugar,
         Resting_ECG,
         Exercise_Induced_Angina,
         Peak_Exercise_ST_Segment,
         Thalassemia,
         Diagnosis_Heart_Disease) %>%
  mutate(Sex = recode_factor(Sex, `0` = "female",`1` = "male" ),
         Chest_Pain_Type = recode_factor(Chest_Pain_Type, `1` = "typical",   
                                         `2` = "atypical",
                                         `3` = "non-angina", 
                                         `0` = "asymptomatic"),
         Fasting_Blood_Sugar = recode_factor(Fasting_Blood_Sugar, `0` = "<= 120 mg/dl", 
                                             `1` = "> 120 mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                     `1` = "ST-T abnormality",
                                     `2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina, `0` = "no",
                                                 `1` = "yes"),
         Peak_Exercise_ST_Segment = recode_factor(Peak_Exercise_ST_Segment, `1` = "up-sloaping",
                                                  `2` = "flat",
                                                  `0` = "down-sloaping"),
         Thalassemia = recode_factor(Thalassemia, `1` = "normal",
                                     `2` = "fixed defect",
                                     `3` = "reversible defect"),
         Diagnosis_Heart_Disease = recode_factor(Diagnosis_Heart_Disease, `0` = "No Disease",
                                                 `1` = "Disease"))

#Phan tram nguoi benh va khong benh
round(prop.table(table(heart_dataset_clean_str$Diagnosis_Heart_Disease)),2)

#Phan tram nguoi nam va nu mac benh
round(prop.table(table(heart_dataset_clean_str$Sex, heart_dataset_clean_str$Diagnosis_Heart_Disease)),2)
# So luong benh tim theo nhip tim
heart_rate <- heart_dataset_clean_str %>% 
  group_by(Diagnosis_Heart_Disease) %>% 
  count(Chest_Pain_Type)
heart_rate
# So luong benh tim theo gioi tinh
heart_rate_sex <- heart_dataset_clean_str %>% 
  group_by(Diagnosis_Heart_Disease) %>% 
  count(Sex)
heart_rate_sex


# 
# a = heart_dataset_clean_tbl
# # a%>%
# #   mutate_if(is.factor, as.numeric)%>%
# #   map_df(~class(.x)) %>%  gather(key="var", value = "class") 
# a$Diagnosis_Heart_Disease = as.numeric(a$Diagnosis_Heart_Disease)
# class(a$Diagnosis_Heart_Disease)
# cor(a[, c("Age", "Diagnosis_Heart_Disease")])
# mohinh1 = lm(Diagnosis_Heart_Disease~Age,data = a)
# summary(mohinh1)
# mohinhmau = a%>%mutate_if(is.integer, as.factor)%>%mutate_if(is.numeric, as.factor)
# str(mohinhmau)
# # %>%map_df(~class(.x))   
# mohinh1 = glm(Diagnosis_Heart_Disease~., data = heart_dataset_clean_tbl)
# summary(mohinh1)
#-----------------------------------

# sum(is.na(heart_dataset_clean_tbl$Thalassemia))
#Select categorical vars, recode them to their character values, convert to long format
hd_long_fact_tbl <- heart_dataset_clean_tbl  %>%
    select(Sex,
            Chest_Pain_Type,
            Fasting_Blood_Sugar,
            Resting_ECG,
            Exercise_Induced_Angina,
            Peak_Exercise_ST_Segment,
            Thalassemia,
            Diagnosis_Heart_Disease) %>%
    mutate(Sex = recode_factor(Sex, `0` = "female",`1` = "male" ),
            Chest_Pain_Type = recode_factor(Chest_Pain_Type, `1` = "typical",   
                                             `2` = "atypical",
                                             `3` = "non-angina", 
                                             `0` = "asymptomatic"),
            Fasting_Blood_Sugar = recode_factor(Fasting_Blood_Sugar, `0` = "<= 120 mg/dl", 
                                                 `1` = "> 120 mg/dl"),
            Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                         `1` = "ST-T abnormality",
                                         `2` = "LV hypertrophy"),
            Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina, `0` = "no",
                                                     `1` = "yes"),
            Peak_Exercise_ST_Segment = recode_factor(Peak_Exercise_ST_Segment, `1` = "up-sloaping",
                                                      `2` = "flat",
                                                      `0` = "down-sloaping"),
            Thalassemia = recode_factor(Thalassemia, `1` = "normal",
                                         `2` = "fixed defect",
                                         `3` = "reversible defect"))%>%
    pivot_longer(-Diagnosis_Heart_Disease)

#Must gather() data first in order to facet wrap by key 
#(default gather call puts all var names into new key col)
hd_long_cont_tbl <- heart_dataset_clean_tbl  %>%
  select(Age,
         Resting_Blood_Pressure,
         Serum_Cholesterol,
         Max_Heart_Rate_Achieved,
         ST_Depression_Exercise,
         Num_Major_Vessels_Flouro,
         Diagnosis_Heart_Disease) %>% 
  gather(key = "key", 
         value = "value",
         -Diagnosis_Heart_Disease)