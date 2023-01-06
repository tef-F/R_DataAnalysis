# -------------------- Xay Dung Mo Hinh ---------------------------------
#tránh thay đổi cho lần chạy khác
set.seed(1333)

#tạch đối tượng
train_test_split <- heart_dataset_clean_tbl %>%
    initial_split(prop = .8, strata = "Diagnosis_Heart_Disease")

#mô hình xây dựng
train_tbl <- train_test_split %>% training()

#mô hình kiểm thử
test_tbl <- train_test_split %>% testing()

#Set up recipe (sử dụng dữ liệu mô hình đào tạo ở đây để tránh rò rỉ)
the_recipe <- recipe(Diagnosis_Heart_Disease ~ ., data = train_tbl) %>%
#[Processing Step 1]
#[Processing Step 2]
prep(train_tbl, retain = TRUE)

#áp dụng công thức cho dữ liệu mô hình xây dựng
train_processed_data <- juice(the_recipe)

#áp dụng công thức cho dữ liệu mô hình kiểm thử
test_processed_data <- bake(the_recipe, new_data = test_tbl)



#Thiết lập mô hình xây dựng
set.seed(100)
#Mô hình hồi quy logistic
log_regr_hd_model <- logistic_reg(mode = "classification") %>%
    set_engine("glm") %>%
    fit(Diagnosis_Heart_Disease ~ ., data = train_processed_data)

#Hiện thị các hệ số mô hình và thêm tỷ lệ chênh lệch để có thể hiểu được
table_coefficients = broom::tidy(log_regr_hd_model$fit) %>%
    arrange(desc(estimate)) %>%
    mutate(odds_ratio = exp(estimate)) %>%
    kable(align = rep("c", 5), digits = 3) %>%
    kable_styling("full_width" = FALSE,
        latex_options = "basic",
        wraptable_width = "1pt",
        bootstrap_options = "bordered")


#Đưa ra dự đoán bằng cách sử dụng bộ thử nghiệm
first_training_prediction <- predict(log_regr_hd_model,
                                    new_data = test_tbl,
                                    type     = "class")

#Thêm dự đoán dưới dạng cột mới trong tập dữ liệu trái tim
first_training_prediction_full_tbl <- test_processed_data %>%
    mutate(Predicted_Heart_Disease = first_training_prediction$.pred_class)

#Glimpse data
first_training_prediction_full_tbl %>% glimpse()

#Sử dụng cột dự đoán và cột thực tế để tạo đối tượng ma trận nhầm lẫn
conf_mat_obj <- first_training_prediction_full_tbl %>%
    conf_mat(truth = Diagnosis_Heart_Disease,
            estimate = Predicted_Heart_Disease)

#Gọi conf_mat và cung cấp các cột cho truth, prediction
#Pluck() để trích xuất dữ liệu conf_matrix thành cols và chuyển thành tibble để vẽ đồ thị
conf_matrix_plt_obj <- first_training_prediction_full_tbl %>%
    conf_mat(truth = Diagnosis_Heart_Disease,
            estimate = Predicted_Heart_Disease) %>%
    pluck(1) %>%
    as_tibble() %>%
    mutate("outcome" = c("true_negative",
                        "false_positive",
                        "false_negative",
                        "true_positive")) %>%
    mutate(Prediction = recode(Prediction, `0` = "No Heart Disease",
                                `1` = "Heart Disease")) %>%
    mutate(Truth = recode(Truth,  `0` = "No Heart Disease",
                        `1` = "Heart Disease"))

#Chuyển đổi sang định dạng kable
table_prediction = conf_matrix_plt_obj %>%
    kable(align = rep("c", 4)) %>%
    kable_styling("full_width" = FALSE,
                latex_options = "basic",
                wraptable_width = "1pt",
                bootstrap_options = "bordered")

#Vẽ ma trận nhầm lẫn
confusion_matrix = conf_matrix_plt_obj %>%
    ggplot(aes(x = Truth, y = Prediction)) +
    geom_tile(aes(fill = n), alpha = .8) +
    geom_text(aes(label = n), color = "white") +
    scale_fill_viridis_c() +
    theme(legend.title = element_blank()) +
    labs(
        title = "Confusion Matrix",
        subtitle = "Heart Disease Prediction Using Logistic Regression"
    )

#Gọi summary() trên ma trận nhầm lẫn đưa ra tất cả các phép đo hiệu suất
#Lọc những thứ quan tâm
log_reg_performance_tbl <- summary(conf_mat_obj) %>% filter(
    .metric == "accuracy" | 
    .metric == "sens" |
    .metric == "spec" |
    .metric == "ppv"  |
    .metric == "npv"  |
    .metric == "f_meas") %>%
    select(-.estimator) %>%
    rename("metric" = .metric, 
            "estimate" = .estimate) %>%
    mutate("estimate" = estimate %>% signif(digits = 3)) %>%
    mutate(metric = recode(metric, "sens" = "sensitivity"),
            metric = recode(metric, "spec" = "specificity"),
            metric = recode(metric, "ppv"  = "positive predictive value"),
            metric = recode(metric, "npv"  = "negative predictive value")) %>%
    kable(align = rep("c", 3))%>%kable_styling("full_width" = F)

#Hiển thị tóm tắt hiệu suất dưới dạng kable
perfomance_summary = log_reg_performance_tbl %>%
    kable_styling("full_width" = FALSE, 
                latex_options = "basic", 
                wraptable_width = "1pt", 
                bootstrap_options = "bordered")

#--------------------- Xay dung mo hinh du doan voi 10 mau thu ------------------
#create multiple split objects w/ vfold cross-validation resampling
set.seed(925)
hd_cv_split_objects <- heart_dataset_clean_tbl %>%
    vfold_cv(strata = Diagnosis_Heart_Disease)
# hd_cv_split_objects

#chức năng lớn có một đối tượng phân tách và một id
make_cv_predictions_fcn <- function(split, id){
    #trích xuất dữ liệu cho bộ phân tích từ đối tượng phân tách
    #prep(train) công thức và trả lại công thức cập nhật
    #bake(apply) công thức được đào tạo cho dữ liệu mới
    analysis_tbl <- analysis(split)
    trained_analysis_recipe <- prep(the_recipe ,training = analysis_tbl)
    baked_analysis_data_tbl <- bake(trained_analysis_recipe, new_data = analysis_tbl)

    #xác định mô hình trong cú pháp parsnip
    model <- logistic_reg(mode = "classification") %>%
        set_engine("glm") %>%
        fit(Diagnosis_Heart_Disease ~ ., data = baked_analysis_data_tbl)

    #giống như trên nhưng đối với bộ đánh giá (giống như bộ kiểm tra nhưng đối với mẫu lại)
    assessment_tbl <- assessment(split)
    trained_assessment_recipe <- prep(the_recipe, training = assessment_tbl)
    baked_assessment_data_tbl <- bake(trained_assessment_recipe, new_data = assessment_tbl)

    #make a tibble with the results
    tibble(
        "id" = id,
        "truth" = baked_assessment_data_tbl$Diagnosis_Heart_Disease,
        "prediction" = unlist(
            predict(model, 
                    new_data = baked_assessment_data_tbl
            )
        )
    )
}

#ánh xạ chức năng lớn tới mọi obj / id được chia trong tbl cv split ban đầu
cv_predictions_tbl <- map2_df(.x = hd_cv_split_objects$splits,
                                .y = hd_cv_split_objects$id,
                                ~make_cv_predictions_fcn(split = .x, id = .y))

#see results 
table_prediction10 = cv_predictions_tbl %>%
    head(10) %>%
    kable(align = rep("c", 3)) %>%
    kable_styling("full_width" = FALSE)


#define desired metrics
desired_metrics <- metric_set(accuracy,
                                sens,
                                spec,
                                ppv,
                                npv,
                                f_meas)

#group by fold and use get desired metrics [metric_set fcn is from yardstick]
cv_metrics_long_tbl <- cv_predictions_tbl %>%
    group_by(id) %>%
    desired_metrics(truth = truth, estimate = prediction)

#see results
table_metrics_long = cv_metrics_long_tbl %>%
    head(10) %>%
    kable(align = rep("c", 4)) %>%
    kable_styling("full_width" = FALSE)

#visualize results
plot_metrics = cv_metrics_long_tbl %>% 
    ggplot(aes(x = .metric, y = .estimate)) +
    geom_boxplot(aes(fill = .metric),
                alpha = .6, 
                fatten = .7) +
    geom_jitter(alpha = 0.2, width = .05) +
    labs(x = "",
        y = "",
        title = "Boxplots for Logistic Regression",
        subtitle = "Model Metrics, 10-Fold Cross Validation") +
    scale_fill_viridis_d() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    theme(legend.title = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

#Gia tri du doan trung binh

#calculate the mean from all the folds for each metric
cv_mean_metrics_tbl <- cv_metrics_long_tbl %>%
    group_by(.metric) %>%
    summarize("Avg" = mean(.estimate)) %>%
    ungroup()

cv_mean_metrics_tbl %>%
    mutate(Average = Avg %>% signif(digits = 3)) %>%
    select(.metric, Average) %>%
    kable(align = rep("c", 2)) %>%
    kable_styling("full_width" = FALSE)