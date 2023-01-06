#----------------------------- Bieu do ---------------------------------


chart_longfact <- hd_long_fact_tbl %>% 
    ggplot(aes(value)) +
    geom_bar(aes(x = value, 
                fill = Diagnosis_Heart_Disease), 
                alpha = 0.4, 
                position = "dodge", 
                color = "black",
                width = .8
    ) +
    labs(x = "",
        y = "",
        title = "Scaled Effect of Categorical Variables") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
    facet_wrap(~ name, scales = "free", nrow = 8, ncol = 2) +
    scale_fill_manual(
        values = c("#fde725ff", "#20a486ff"),
        name   = "Heart\nDisease",
        labels = c("No HD", "Yes HD")
    )
    
    
#Hiển thị biểu đồ đi kiểm tra
boxplot_numeric = hd_long_cont_tbl %>% 
    ggplot(aes(y = value)) +
    geom_boxplot(aes(fill = Diagnosis_Heart_Disease),
                alpha  = .6,
                fatten = .7) +
    labs(x = "",
        y = "",
        title = "Boxplots for Numeric Variables") +
    scale_fill_manual(
        values = c("#fde725ff", "#20a486ff"),
        name   = "Heart\nDisease",
        labels = c("No HD", "Yes HD")) +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
    facet_wrap(~ key, 
                scales = "free", 
                ncol   = 3)


#sapply(heart_dataset_clean_tbl, class)
#Correlation matrix using Pearson method, default method is Pearson
matrix_pearson = heart_dataset_clean_tbl %>%
    mutate_at(c("Resting_ECG", 
                "Fasting_Blood_Sugar", 
                "Sex", 
                "Diagnosis_Heart_Disease",
                "Thalassemia",
                "Exercise_Induced_Angina",
                "Peak_Exercise_ST_Segment", 
                "Chest_Pain_Type"), as.numeric)%>% 
    ggcorr(high = "#20a486ff",
        low = "#fde725ff",
        label = TRUE, 
        hjust = .75, 
        size = 3, 
        label_size = 3,
        nbreaks = 5
) +
    labs(title = "Correlation Matrix",
        subtitle = "Pearson Method Using Pairwise Obervations")
    
    
#Correlation matrix using Kendall method
matrix_kendall = heart_dataset_clean_tbl %>% 
    mutate_at(c("Resting_ECG", 
                "Fasting_Blood_Sugar", 
                "Sex", 
                "Diagnosis_Heart_Disease",
                "Thalassemia",
                "Exercise_Induced_Angina",
                "Peak_Exercise_ST_Segment", 
                "Chest_Pain_Type"), as.numeric)%>% 
    ggcorr(method = c("pairwise", "kendall"),
        high = "#20a486ff",
        low = "#fde725ff",
        label = TRUE, 
        hjust = .75, 
        size = 3, 
        label_size = 3,
        nbreaks = 5
    ) +
    labs(title = "Correlation Matrix",
        subtitle = "Kendall Method Using Pairwise Observations")
