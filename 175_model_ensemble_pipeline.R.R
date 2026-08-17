# ============================
# 加载所需的R包
# ============================
library(tgp)             # 贝叶斯树高斯过程
library(openxlsx)        # Excel文件操作
library(seqinr)          # 序列分析
library(plyr)            # 数据处理工具
library(randomForestSRC) # 随机森林
library(glmnet)          # 广义线性模型和正则化
library(plsRglm)         # 偏最小二乘回归
library(gbm)             # 梯度提升机
library(caret)           # 机器学习算法训练和测试
library(mboost)          # 增强模型
library(e1071)           # SVM等算法
library(BART)            # 贝叶斯加法回归树
library(MASS)            # 广泛应用的统计方法
library(snowfall)        # 并行计算支持
library(xgboost)         # 极端梯度提升模型
library(ComplexHeatmap)  # 复杂热图绘制
library(RColorBrewer)    # 颜色选择
library(pROC)            # ROC曲线工具
library(circlize)        # 圆形可视化
library(class)           # KNN算法
library(ada)             # AdaBoost算法
library(smotefamily)
# ============================
min.selected.var = 3     # 设置变量选择的最小数目
max.selected.var = 15    # 设置变量选择的最大数目
top.models.for.roc = 15  # 设置绘制ROC曲线的模型数量

# ============================
# 自动识别训练集和测试集文件
# ============================
all_files <- list.files(pattern = "\\.csv$", ignore.case = TRUE)
train_files <- all_files[grepl("^train", all_files, ignore.case = TRUE)]
test_files <- all_files[grepl("^test_", all_files, ignore.case = TRUE)]

if(length(train_files) == 0) {
  stop("未找到训练集文件（以train_开头的CSV文件）")
}
if(length(test_files) == 0) {
  stop("未找到测试集文件（以test_开头的CSV文件）")
}

cat("========================================\n")
cat("发现的数据文件：\n")
cat(sprintf("  训练集: %s\n", paste(train_files, collapse = ", ")))
cat(sprintf("  测试集: %s\n", paste(test_files, collapse = ", ")))
cat("========================================\n\n")

# 定义一个函数RunML，用于运行机器学习算法
RunML <- function(method, Train_set, Train_label, mode = "Model", classVar){
  # 清理和准备算法名称和参数
  method = gsub(" ", "", method) # 去除方法名称中的空格
  method_name = gsub("(\\w+)\\[(.+)\\]", "\\1", method)  # 从方法名称中提取算法名称
  method_param = gsub("(\\w+)\\[(.+)\\]", "\\2", method) # 从方法名称中提取参

  # 根据提取的算法名称，准备相应的参数
  method_param = switch(
    EXPR = method_name,
    "Enet" = list("alpha" = as.numeric(gsub("alpha=", "", method_param))),
    "Stepglm" = list("direction" = method_param),
    NULL  # 如果没有匹配到任何名称，返回NULL
  )

  # 输出正在运行的算法和使用的变量数
  message("Run ", method_name, " algorithm for ", mode, "; ",
          method_param, ";",
          " using ", ncol(Train_set), " Variables")

  # 将传入的参数和提取的参数组合成一个新的参数列表
  args = list("Train_set" = Train_set,
              "Train_label" = Train_label,
              "mode" = mode,
              "classVar" = classVar)
  args = c(args, method_param)

  # 使用do.call动态调用相应的算法实现函数
  obj <- do.call(what = paste0("Run", method_name),
                 args = args)

  # 根据模式，输出不同的信息
  if(mode == "Variable"){
    message(length(obj), " Variables retained;\n")
  }else{message("\n")}
  return(obj)
}

# 定义用于运行Elastic Net正则化线性模型的函数
RunEnet <- function(Train_set, Train_label, mode, classVar, alpha){
  # 使用交叉验证找到最优的正则化参数
  cv.fit = cv.glmnet(x = Train_set,
                     y = Train_label[[classVar]],
                     family = "binomial", alpha = alpha, nfolds = 10)
  # 建立最终模型
  fit = glmnet(x = Train_set,
               y = Train_label[[classVar]],
               family = "binomial", alpha = alpha, lambda = cv.fit$lambda.min)
  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行Lasso正则化线性模型的函数
RunLasso <- function(Train_set, Train_label, mode, classVar){
  RunEnet(Train_set, Train_label, mode, classVar, alpha = 1)
}

# 定义用于运行Ridge正则化线性模型的函数
RunRidge <- function(Train_set, Train_label, mode, classVar){
  RunEnet(Train_set, Train_label, mode, classVar, alpha = 0)
}

# 定义用于运行逐步广义线性模型的函数
RunStepglm <- function(Train_set, Train_label, mode, classVar, direction){
  # 使用glm函数和step函数逐步选择模型
  fit <- step(glm(formula = Train_label[[classVar]] ~ .,
                  family = "binomial",
                  data = as.data.frame(Train_set)),
              direction = direction, trace = 0)
  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行支持向量机的函数
RunSVM <- function(Train_set, Train_label, mode, classVar){
  # 将数据框转换为因子类型，适合SVM模型输入
  data <- as.data.frame(Train_set)
  data[[classVar]] <- as.factor(Train_label[[classVar]])
  # 建立SVM模型
  fit = svm(formula = eval(parse(text = paste(classVar, "~."))),
            data= data, probability = T)
  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行线性判别分析的函数
RunLDA <- function(Train_set, Train_label, mode, classVar){
  # 准备数据，将类变量转换为因子类型
  data <- as.data.frame(Train_set)
  data[[classVar]] <- as.factor(Train_label[[classVar]])
  # 使用train函数建立LDA模型
  fit = train(eval(parse(text = paste(classVar, "~."))),
              data = data,
              method="lda",
              trControl = trainControl(method = "cv"))
  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行梯度提升机的函数
RunglmBoost <- function(Train_set, Train_label, mode, classVar){
  # 准备数据，将类变量和训练集绑定
  data <- cbind(Train_set, Train_label[classVar])
  data[[classVar]] <- as.factor(data[[classVar]])

  # 建立GLMBoost模型
  # 使用较大的迭代次数以获得最佳性能
  fit <- glmboost(eval(parse(text = paste(classVar, "~."))),
                  data = data,
                  family = Binomial(),
                  control = boost_control(mstop = 100))

  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

#################### 定义用于运行偏最小二乘回归和广义线性模型的函数
RunplsRglm <- function(Train_set, Train_label, mode, classVar){
  
  # 准备数据合并
  data_df <- as.data.frame(Train_set)
  # smotefamily 需要目标变量为数值型的字符或因子
  data_df[[classVar]] <- as.numeric(as.character(Train_label[[classVar]]))
  
  # 执行 SMOTE 算法进行过采样
  # K=5 是 KNN 的默认邻居数，dup_size=0 表示自动平衡至 1:1
  smote_result <- SMOTE(X = data_df[, -ncol(data_df)], 
                        target = data_df[[classVar]], 
                        K = 5, dup_size = 0)
  
  # 提取平衡后的数据集
  Train_set_smote <- smote_result$data[, -ncol(smote_result$data)]
  Train_label_smote <- smote_result$data$class
  
  # 确保标签是数值型，适配 plsRglm 的 logistic 模式
  Train_label_smote <- as.numeric(Train_label_smote)
  
  # 使用平衡后的数据进行 10折交叉验证
  # 此时模型在平衡数据上学习，能有效抵抗过拟合
  cv.plsRglm.res = cv.plsRglm(formula = Train_label_smote ~ .,
                              data = Train_set_smote,
                              nt=10, verbose = FALSE)
  
  # 构建最终模型
  fit <- plsRglm(Train_label_smote,
                 Train_set_smote,
                 modele = "pls-glm-logistic",
                 verbose = F, sparse = T)
  
  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行随机森林的函数
RunRF <- function(Train_set, Train_label, mode, classVar){
  # 设置随机森林参数，如树的最小节点大小
  rf_nodesize = 5 # 可根据需要调整
  # 准备数据，将类变量转换为因子
  Train_label[[classVar]] <- as.factor(Train_label[[classVar]])
  # 建立随机森林模型
  fit <- rfsrc(formula = formula(paste0(classVar, "~.")),
               data = cbind(Train_set, Train_label[classVar]),
               ntree = 1000, nodesize = rf_nodesize,
               importance = T,
               proximity = T,
               forest = T)
  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行梯度提升机的函数
RunGBM <- function(Train_set, Train_label, mode, classVar){
  # 获取样本数量
  n_samples <- nrow(Train_set)

  # 根据样本数量调整参数
  if (n_samples < 50) {
    # 样本数较少时，使用更小的参数
    n_minobsinnode <- 2
    cv_folds <- 3
    n_trees <- 100
    interaction_depth <- 2
  } else if (n_samples < 100) {
    # 样本数中等时
    n_minobsinnode <- 5
    cv_folds <- 5
    n_trees <- 500
    interaction_depth <- 3
  } else {
    # 样本数足够时，使用原来的参数
    n_minobsinnode <- 10
    cv_folds <- 10
    n_trees <- 10000
    interaction_depth <- 3
  }

  # 建立初步的GBM模型
  tryCatch({
    fit <- gbm(formula = Train_label[[classVar]] ~ .,
               data = as.data.frame(Train_set),
               distribution = 'bernoulli',
               n.trees = n_trees,
               interaction.depth = interaction_depth,
               n.minobsinnode = n_minobsinnode,
               shrinkage = 0.001,
               cv.folds = cv_folds,
               n.cores = 1)  # 改为单核避免并行问题

    # 选择最优的迭代次数
    best <- which.min(fit$cv.error)
    fit <- gbm(formula = Train_label[[classVar]] ~ .,
               data = as.data.frame(Train_set),
               distribution = 'bernoulli',
               n.trees = best,
               interaction.depth = interaction_depth,
               n.minobsinnode = n_minobsinnode,
               shrinkage = 0.001,
               n.cores = 1)

    fit$subFeature = colnames(Train_set)
    if (mode == "Model") return(fit)
    if (mode == "Variable") return(ExtractVar(fit))
  }, error = function(e) {
    # 如果GBM失败，返回空结果
    warning(sprintf("GBM模型构建失败: %s", e$message))
    return(if(mode == "Model") NULL else c())
  })
}

# 定义用于运行XGBoost的函数
RunXGBoost <- function(Train_set, Train_label, mode, classVar){
  # 将标签转换为整数类型的0和1
  label_raw <- Train_label[[classVar]]
  if (is.factor(label_raw)) {
    label_int <- as.integer(label_raw) - 1L
  } else {
    label_int <- as.integer(label_raw)
    # 确保值为0和1
    if (min(label_int, na.rm = TRUE) == 1) {
      label_int <- label_int - 1L
    }
  }

  # 创建交叉验证折叠
  indexes = createFolds(as.factor(label_int), k = 5, list=T)
  # 计算每折的最优模型参数
  CV <- tryCatch({
    unlist(lapply(indexes, function(pt){
      dtrain = xgb.DMatrix(data = Train_set[-pt, , drop=FALSE],
                           label = label_int[-pt])
      dtest = xgb.DMatrix(data = Train_set[pt, , drop=FALSE],
                          label = label_int[pt])
      watchlist <- list(train=dtrain, test=dtest)

      bst <- xgb.train(data=dtrain,
                       max.depth=2, eta=1, nthread = 2, nrounds=10,
                       watchlist=watchlist,
                       objective = "reg:logistic", verbose = F)
      which.min(bst$evaluation_log$test_rmse)
    }))
  }, error = function(e) {
    return(rep(5, 5))  # 出错时返回默认值
  })

  # 使用最常用的轮数建立最终模型
  nround_tab <- table(CV)
  if (length(nround_tab) > 0) {
    nround <- as.numeric(names(which.max(nround_tab)))[1]
  } else {
    nround <- 5
  }
  if (length(nround) == 0 || is.na(nround[1]) || nround[1] < 1) nround <- 5

  # 使用xgb.train建立最终模型
  dtrain_final <- xgb.DMatrix(data = Train_set, label = label_int)
  xgb_model <- xgb.train(data = dtrain_final,
                   max.depth = 2, eta = 1, nthread = 2, nrounds = nround,
                   objective = "reg:logistic", verbose = F)

  # 保存模型的原始数据（raw格式），这样可以避免模型损坏问题
  model_raw <- xgb.save.raw(xgb_model)

  # 创建包装对象来保存模型原始数据和特征名
  fit <- list(
    model_raw = model_raw,       # 保存原始二进制数据
    subFeature = colnames(Train_set),
    nround = nround,
    max_depth = 2,
    eta = 1
  )
  class(fit) <- c("xgb_wrapper")

  if (mode == "Model") return(fit)
  if (mode == "Variable") return(colnames(Train_set))
}

# 定义用于运行朴素贝叶斯分类器的函数
RunNaiveBayes <- function(Train_set, Train_label, mode, classVar){
  # 准备数据
  data <- cbind(Train_set, Train_label[classVar])
  data[[classVar]] <- as.factor(data[[classVar]])
  # 建立朴素贝叶斯模型
  fit <- naiveBayes(eval(parse(text = paste(classVar, "~."))),
                    data = data)
  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行KNN (K近邻) 的函数
RunKNN <- function(Train_set, Train_label, mode, classVar){
  # 使用caret的train函数进行KNN建模，自动选择最优K值
  data <- as.data.frame(Train_set)
  data[[classVar]] <- as.factor(Train_label[[classVar]])

  # 使用交叉验证选择最优K值
  fit <- train(eval(parse(text = paste(classVar, "~."))),
               data = data,
               method = "knn",
               trControl = trainControl(method = "cv", number = 5),
               tuneGrid = data.frame(k = c(3, 5, 7, 9, 11)))

  fit$subFeature = colnames(Train_set)
  if (mode == "Model") return(fit)
  if (mode == "Variable") return(ExtractVar(fit))
}

# 定义用于运行AdaBoost (自适应提升) 的函数
RunAdaBoost <- function(Train_set, Train_label, mode, classVar){
  # 准备数据
  data <- as.data.frame(Train_set)
  data[[classVar]] <- as.factor(Train_label[[classVar]])

  # 建立AdaBoost模型
  tryCatch({
    fit <- ada(eval(parse(text = paste(classVar, "~."))),
               data = data,
               iter = 50,        # 迭代次数
               loss = "logistic", # 使用logistic损失函数
               type = "discrete")

    fit$subFeature = colnames(Train_set)
    if (mode == "Model") return(fit)
    if (mode == "Variable") return(ExtractVar(fit))
  }, error = function(e) {
    warning(sprintf("AdaBoost模型构建失败: %s", e$message))
    return(if(mode == "Model") NULL else c())
  })
}

# 定义用于运行QDA (二次判别分析) 的函数
RunQDA <- function(Train_set, Train_label, mode, classVar){
  # 准备数据
  data <- as.data.frame(Train_set)
  data[[classVar]] <- as.factor(Train_label[[classVar]])

  # 使用caret的train函数建立QDA模型
  tryCatch({
    fit <- train(eval(parse(text = paste(classVar, "~."))),
                 data = data,
                 method = "qda",
                 trControl = trainControl(method = "cv", number = 5))

    fit$subFeature = colnames(Train_set)
    if (mode == "Model") return(fit)
    if (mode == "Variable") return(ExtractVar(fit))
  }, error = function(e) {
    warning(sprintf("QDA模型构建失败: %s", e$message))
    return(if(mode == "Model") NULL else c())
  })
}

# 定义一个函数用于在执行过程中抑制输出
quiet <- function(..., messages=FALSE, cat=FALSE){
  if(!cat){
    sink(tempfile())  # 将输出重定向到临时文件
    on.exit(sink())  # 确保在函数退出时恢复正常输出
  }
  # 根据参数决定是否抑制消息
  out <- if(messages) eval(...) else suppressMessages(eval(...))
  out
}

# 定义一个函数用于标准化数据
standarize.fun <- function(indata, centerFlag, scaleFlag) {
  scale(indata, center=centerFlag, scale=scaleFlag)
}

# 定义一个函数用于批量处理数据的标准化
scaleData <- function(data, cohort = NULL, centerFlags = NULL, scaleFlags = NULL){
  samplename = rownames(data)  # 保存原始样本名称
  # 如果没有指定队列，将所有数据视为一个队列
  if (is.null(cohort)){
    data <- list(data); names(data) = "training"
  }else{
    data <- split(as.data.frame(data), cohort)  # 根据队列分割数据
  }

  # 如果没有提供中心化标志，默认不进行中心化
  if (is.null(centerFlags)){
    centerFlags = F; message("No centerFlags found, set as FALSE")
  }
  # 如果中心化标志是单一值，应用于所有数据
  if (length(centerFlags)==1){
    centerFlags = rep(centerFlags, length(data)); message("set centerFlags for all cohort as ", unique(centerFlags))
  }
  # 如果中心化标志没有命名，按顺序匹配
  if (is.null(names(centerFlags))){
    names(centerFlags) <- names(data); message("match centerFlags with cohort by order\n")
  }

  # 如果没有提供缩放标志，默认不进行缩放
  if (is.null(scaleFlags)){
    scaleFlags = F; message("No scaleFlags found, set as FALSE")
  }
  # 如果缩放标志是单一值，应用于所有数据
  if (length(scaleFlags)==1){
    scaleFlags = rep(scaleFlags, length(data)); message("set scaleFlags for all cohort as ", unique(scaleFlags))
  }
  # 如果缩放标志没有命名，按顺序匹配
  if (is.null(names(scaleFlags))){
    names(scaleFlags) <- names(data); message("match scaleFlags with cohort by order\n")
  }

  centerFlags <- centerFlags[names(data)]; scaleFlags <- scaleFlags[names(data)]
  # 使用mapply函数对每个数据队列应用标准化函数
  outdata <- mapply(standarize.fun, indata = data, centerFlag = centerFlags, scaleFlag = scaleFlags, SIMPLIFY = F)
  # lapply(out.data, function(x) summary(apply(x, 2, var)))
  # 将处理后的数据按原始顺序重新组合
  outdata <- do.call(rbind, outdata)
  outdata <- outdata[samplename, ]
  return(outdata)
}

# 定义一个函数用于从模型中提取重要的变量
ExtractVar <- function(fit){
  Feature <- quiet(switch(
    EXPR = class(fit)[1],
    "lognet" = rownames(coef(fit))[which(coef(fit)[, 1]!=0)], # 从Elastic Net模型中提取非零系数的变量
    "glm" = names(coef(fit)), # 从广义线性模型中提取变量
    "svm.formula" = fit$subFeature, # SVM模型中未进行变量选择，使用所有变量
    "train" = fit$coefnames, # 训练集中使用的变量 (包括KNN和QDA)
    "glmboost" = names(coef(fit)[abs(coef(fit))>0]), # 从GLMBoost模型中提取系数非零的变量
    "plsRglmmodel" = rownames(fit$Coeffs)[fit$Coeffs!=0], # 从PLSRGLM模型中提取系数非零的变量
    "rfsrc" = names(which(fit$importance[,1] > 0.01)),

    "gbm" = rownames(summary.gbm(fit, plotit = F))[summary.gbm(fit, plotit = F)$rel.inf>0], # 从GBM模型中提取重要的变量
    "xgb_wrapper" = fit$subFeature, # XGBoost包装对象中获取特征名
    "xgb.Booster" = if(!is.null(fit$subFeature)) fit$subFeature else attr(fit, "subFeature"), # XGBoost模型
    "naiveBayes" = fit$subFeature, # 朴素贝叶斯模型中使用的所有变量
    "ada" = fit$subFeature # AdaBoost模型中使用的所有变量
    # "drf" = fit$subFeature # DRF模型中使用的所有变量，当前版本已注释
  ))

  # 从提取的变量中移除截距项
  Feature <- setdiff(Feature, c("(Intercept)", "Intercept"))
  return(Feature)
}

# 定义一个函数用于计算预测得分
CalPredictScore <- function(fit, new_data, type = "lp"){
  # 检查模型是否为空
  if(is.null(fit)) {
    warning("模型为空，返回NA")
    return(setNames(rep(NA, nrow(new_data)), rownames(new_data)))
  }

  # 获取特征名（兼容xgb_wrapper和其他模型）
  model_class <- class(fit)[1]
  if (model_class == "xgb_wrapper") {
    sub_features <- fit$subFeature
    # 从raw数据重新加载XGBoost模型
    if(!is.null(fit$model_raw)) {
      actual_model <- xgb.load.raw(fit$model_raw)
    } else {
      warning("XGBoost模型raw数据为空")
      return(setNames(rep(NA, nrow(new_data)), rownames(new_data)))
    }
  } else if (model_class == "xgb.Booster") {
    sub_features <- if(!is.null(fit$subFeature)) fit$subFeature else attr(fit, "subFeature")
    actual_model <- fit
  } else {
    sub_features <- fit$subFeature
    actual_model <- fit
  }

  # 检查特征名是否存在
  if(is.null(sub_features) || length(sub_features) == 0) {
    warning("模型特征名为空，返回NA")
    return(setNames(rep(NA, nrow(new_data)), rownames(new_data)))
  }

  # 仅使用模型中涉及的变量
  new_data <- new_data[, sub_features, drop = FALSE]

  # 保存原始样本名
  original_samples <- rownames(new_data)

  # 对于SVM模型，需要确保数据格式一致
  if(model_class == "svm.formula"){
    # 检查含有NA的行
    na_rows <- apply(new_data, 1, function(x) any(is.na(x)))
    if(any(na_rows)){
      warning(sprintf("SVM预测：发现 %d 个含有NA的样本，将返回NA预测值", sum(na_rows)))
      # 只对没有NA的样本进行预测
      valid_data <- new_data[!na_rows, , drop = FALSE]
    } else {
      valid_data <- new_data
      na_rows <- rep(FALSE, nrow(new_data))
    }
  } else {
    valid_data <- new_data
    na_rows <- rep(FALSE, nrow(new_data))
  }

  # 根据模型类型使用不同的预测函数（仅对有效数据）
  RS_valid <- tryCatch({
    quiet(switch(
      EXPR = model_class,
      "lognet"      = predict(fit, type = 'response', as.matrix(valid_data)),
      "glm"         = predict(fit, type = 'response', as.data.frame(valid_data)),
      "svm.formula" = {
        if(nrow(valid_data) == 0) {
          numeric(0)
        } else {
          pred <- predict(fit, as.data.frame(valid_data), probability = TRUE)
          probs <- attr(pred, "probabilities")
          if("1" %in% colnames(probs)){
            probs[, "1"]
          } else if("0" %in% colnames(probs)){
            1 - probs[, "0"]
          } else {
            rep(NA, nrow(valid_data))
          }
        }
      },
      "train"       = predict(fit, valid_data, type = "prob")[[2]],
      "glmboost"    = predict(fit, type = "response", as.data.frame(valid_data)),
      "plsRglmmodel" = predict(fit, type = "response", as.data.frame(valid_data)),
      "rfsrc"        = predict(fit, as.data.frame(valid_data))$predicted[, "1"],
      "gbm"          = predict(fit, type = 'response', as.data.frame(valid_data)),
      "xgb_wrapper" = {
        tryCatch({
          predict(actual_model, as.matrix(valid_data))
        }, error = function(e) {
          warning(sprintf("XGBoost预测出错: %s", e$message))
          rep(NA, nrow(valid_data))
        })
      },
      "xgb.Booster" = {
        tryCatch({
          predict(actual_model, as.matrix(valid_data))
        }, error = function(e) {
          warning(sprintf("XGBoost预测出错: %s", e$message))
          rep(NA, nrow(valid_data))
        })
      },
      "naiveBayes" = predict(object = fit, type = "raw", newdata = valid_data)[, "1"],
      "ada" = predict(fit, as.data.frame(valid_data), type = "probs")[, 2],
      # 默认返回NA
      rep(NA, nrow(valid_data))
    ))
  }, error = function(e) {
    warning(sprintf("CalPredictScore预测出错 (%s): %s", model_class, e$message))
    return(rep(NA, nrow(valid_data)))
  })

  # 将预测结果转换为数值类型
  RS_valid = as.numeric(as.vector(RS_valid))

  # 创建完整的结果向量，包含NA值
  RS = rep(NA, nrow(new_data))
  # 只在有有效数据的情况下进行赋值
  if(length(RS_valid) > 0 && sum(!na_rows) > 0) {
    RS[!na_rows] = RS_valid
  }

  # 赋予原始样本名称
  names(RS) = original_samples
  return(RS)
}

# 定义一个函数用于预测类别
PredictClass <- function(fit, new_data){
  # 检查模型是否为空
  if(is.null(fit)) {
    warning("模型为空，返回NA")
    return(rep(NA_character_, nrow(new_data)))
  }

  # 获取特征名（兼容xgb_wrapper和其他模型）
  model_class <- class(fit)[1]
  if (model_class == "xgb_wrapper") {
    sub_features <- fit$subFeature
    # 从raw数据重新加载XGBoost模型
    if(!is.null(fit$model_raw)) {
      actual_model <- xgb.load.raw(fit$model_raw)
    } else {
      warning("XGBoost模型raw数据为空")
      return(rep(NA_character_, nrow(new_data)))
    }
  } else if (model_class == "xgb.Booster") {
    sub_features <- if(!is.null(fit$subFeature)) fit$subFeature else attr(fit, "subFeature")
    actual_model <- fit
  } else {
    sub_features <- fit$subFeature
    actual_model <- fit
  }

  # 检查特征名是否存在
  if(is.null(sub_features) || length(sub_features) == 0) {
    warning("模型特征名为空，返回NA")
    return(rep(NA_character_, nrow(new_data)))
  }

  # 仅使用模型中涉及的变量
  new_data <- new_data[, sub_features, drop = FALSE]

  # 保存原始样本名
  original_samples <- rownames(new_data)

  # 对于SVM模型，需要确保数据格式一致
  if(model_class == "svm.formula"){
    # 检查含有NA的行
    na_rows <- apply(new_data, 1, function(x) any(is.na(x)))
    if(any(na_rows)){
      warning(sprintf("SVM分类预测：发现 %d 个含有NA的样本，将返回NA预测值", sum(na_rows)))
      # 只对没有NA的样本进行预测
      valid_data <- new_data[!na_rows, , drop = FALSE]
    } else {
      valid_data <- new_data
      na_rows <- rep(FALSE, nrow(new_data))
    }
  } else {
    valid_data <- new_data
    na_rows <- rep(FALSE, nrow(new_data))
  }

  # 根据模型类型使用不同的分类预测函数（仅对有效数据）
  label_valid <- tryCatch({
    quiet(switch(
      EXPR = model_class,
      "lognet"      = predict(fit, type = 'class', as.matrix(valid_data)),
      "glm"         = ifelse(test = predict(fit, type = 'response', as.data.frame(valid_data))>0.5,
                             yes = "1", no = "0"),
      "svm.formula" = as.character(predict(fit, as.data.frame(valid_data))),
      "train"       = predict(fit, valid_data, type = "raw"),
      "glmboost"    = predict(fit, type = "class", as.data.frame(valid_data)),
      "plsRglmmodel" = ifelse(test = predict(fit, type = 'response', as.data.frame(valid_data))>0.5,
                              yes = "1", no = "0"),
      "rfsrc"        = predict(fit, as.data.frame(valid_data))$class,
      "gbm"          = ifelse(test = predict(fit, type = 'response', as.data.frame(valid_data))>0.5,
                              yes = "1", no = "0"),
      "xgb_wrapper" = {
        tryCatch({
          ifelse(test = predict(actual_model, as.matrix(valid_data))>0.5,
                 yes = "1", no = "0")
        }, error = function(e) {
          warning(sprintf("XGBoost分类预测出错: %s", e$message))
          rep(NA_character_, nrow(valid_data))
        })
      },
      "xgb.Booster" = {
        tryCatch({
          ifelse(test = predict(actual_model, as.matrix(valid_data))>0.5,
                 yes = "1", no = "0")
        }, error = function(e) {
          warning(sprintf("XGBoost分类预测出错: %s", e$message))
          rep(NA_character_, nrow(valid_data))
        })
      },
      "naiveBayes" = predict(object = fit, type = "class", newdata = valid_data),
      "ada" = as.character(predict(fit, as.data.frame(valid_data))),
      # 默认返回NA
      rep(NA_character_, nrow(valid_data))
    ))
  }, error = function(e) {
    warning(sprintf("PredictClass预测出错 (%s): %s", model_class, e$message))
    return(rep(NA_character_, nrow(valid_data)))
  })

  # 检查预测结果是否为空
  if(is.null(label_valid) || length(label_valid) == 0) {
    warning("预测结果为空，返回NA")
    return(setNames(rep(NA_character_, nrow(new_data)), original_samples))
  }

  # 将预测结果转换为字符类型
  label_valid = as.character(as.vector(label_valid))

  # 创建完整的结果向量，包含NA值
  label = rep(NA_character_, nrow(new_data))
  if(length(label_valid) > 0 && sum(!na_rows) > 0) {
    label[!na_rows] = label_valid
  }

  # 赋予原始样本名称
  names(label) = original_samples
  return(label)
}

# 定义一个函数用于评估模型性能
RunEval <- function(fit,
                    Test_set = NULL,
                    Test_label = NULL,
                    Train_set = NULL,
                    Train_label = NULL,
                    Train_name = NULL,
                    cohortVar = "Cohort",
                    classVar){

  # 获取特征名（兼容xgb_wrapper和其他模型）
  model_class <- class(fit)[1]
  if (model_class == "xgb_wrapper") {
    sub_features <- fit$subFeature
  } else if (model_class == "xgb.Booster") {
    sub_features <- if(!is.null(fit$subFeature)) fit$subFeature else attr(fit, "subFeature")
  } else {
    sub_features <- fit$subFeature
  }

  # 检查测试标签中是否存在队列指标
  if(!is.element(cohortVar, colnames(Test_label))) {
    stop(paste0("There is no [", cohortVar, "] indicator, please fill in one more column!"))
  }

  # 如果提供了训练集和训练标签，将它们与测试集合并
  if((!is.null(Train_set)) & (!is.null(Train_label))) {
    new_data <- rbind.data.frame(Train_set[, sub_features],
                                 Test_set[, sub_features])

    # 如果提供了训练名称，将其作为队列名称
    if(!is.null(Train_name)) {
      Train_label$Cohort <- Train_name
    } else {
      Train_label$Cohort <- "Train-GEO"
    }
    # 更新训练标签的列名，包括队列变量和类变量
    colnames(Train_label)[ncol(Train_label)] <- cohortVar
    Test_label <- rbind.data.frame(Train_label[,c(cohortVar, classVar)],
                                   Test_label[,c(cohortVar, classVar)])
    Test_label[,1] <- factor(Test_label[,1],
                             levels = c(unique(Train_label[,cohortVar]), setdiff(unique(Test_label[,cohortVar]),unique(Train_label[,cohortVar]))))
  } else {
    new_data <- Test_set[, sub_features]
  }

  # 计算预测得分
  RS <- suppressWarnings(CalPredictScore(fit = fit, new_data = new_data))

  # 准备输出数据，包括预测得分
  Predict.out <- Test_label
  Predict.out$RS <- as.vector(RS)
  # 按队列分组
  Predict.out <- split(x = Predict.out, f = Predict.out[,cohortVar])

  # 计算每个队列的AUC值
  result <- unlist(lapply(Predict.out, function(data){
    # 检查该队列是否有两个类别
    unique_classes <- unique(data[[classVar]])
    if(length(unique_classes) < 2){
      warning(sprintf("队列 '%s' 只有一个类别 (%s)，无法计算AUC，返回NA",
                      unique(data[[cohortVar]]), paste(unique_classes, collapse=",")))
      return(NA)
    }
    # 检查是否有NA值
    valid_idx <- !is.na(data$RS) & !is.na(data[[classVar]])
    if(sum(valid_idx) < 2){
      warning(sprintf("队列 '%s' 有效样本数不足，返回NA",
                      unique(data[[cohortVar]])))
      return(NA)
    }
    # 计算AUC - 确保与ROC曲线绘制时使用相同的参数
    tryCatch({
      as.numeric(auc(suppressMessages(roc(data[[classVar]][valid_idx], data$RS[valid_idx],
                                          levels = c(0, 1), direction = "<"))))
    }, error = function(e){
      warning(sprintf("队列 '%s' 计算AUC时出错: %s",
                      unique(data[[cohortVar]]), e$message))
      return(NA)
    })
  }))

  return(result)
}

# 定义一个简单的热图绘制函数
SimpleHeatmap <- function(Cindex_mat, avg_Cindex,
                          CohortCol, barCol,
                          cellwidth = 1, cellheight = 0.5,
                          cluster_columns, cluster_rows,
                          gene_counts = NULL){  # 添加基因数量参数

  # 检查输入数据的有效性
  if(is.null(Cindex_mat) || nrow(Cindex_mat) == 0 || ncol(Cindex_mat) == 0) {
    stop("Cindex_mat 为空或无效")
  }

  # 检查 avg_Cindex 的有效性
  if(is.null(avg_Cindex) || length(avg_Cindex) == 0) {
    warning("avg_Cindex 为空，使用行平均值")
    avg_Cindex <- apply(Cindex_mat, 1, mean, na.rm = TRUE)
  }

  # 确保 avg_Cindex 长度与矩阵行数匹配
  if(length(avg_Cindex) != nrow(Cindex_mat)) {
    warning("avg_Cindex 长度与矩阵行数不匹配，重新计算")
    avg_Cindex <- apply(Cindex_mat, 1, mean, na.rm = TRUE)
  }

  # 将 NA 和无穷值替换为 0
  avg_Cindex[is.na(avg_Cindex) | is.infinite(avg_Cindex)] <- 0
  avg_Cindex <- as.numeric(avg_Cindex)

  # 定义列注释
  col_ha = columnAnnotation("Cohort" = colnames(Cindex_mat),
                            col = list("Cohort" = CohortCol),
                            show_annotation_name = F)

  # 定义行注释，包括平均C指数的条形图
  row_ha = rowAnnotation(bar = anno_barplot(avg_Cindex, bar_width = 0.8, border = FALSE,
                                            gp = gpar(fill = barCol, col = NA),
                                            add_numbers = T, numbers_offset = unit(-10, "mm"),
                                            axis_param = list("labels_rot" = 0),
                                            numbers_gp = gpar(fontsize = 9, col = "white"),
                                            width = unit(3, "cm")),
                         show_annotation_name = F)

  # 如果提供了基因数量信息，修改行名以包含基因数量
  row_labels <- rownames(Cindex_mat)
  if (!is.null(gene_counts) && length(gene_counts) > 0) {
    # 确保 gene_counts 中有对应的行名
    matched_counts <- gene_counts[rownames(Cindex_mat)]
    matched_counts[is.na(matched_counts)] <- 0
    row_labels <- paste0(rownames(Cindex_mat), " (n=", matched_counts, ")")
  }

  # 根据列数调整热图宽度
  n_cols <- ncol(Cindex_mat)
  if (n_cols > 50) {
    # 当列数太多时，使用较小的单位宽度
    cellwidth_adj <- 0.3
  } else {
    cellwidth_adj <- cellwidth
  }

  # 计算合理的热图尺寸
  heatmap_width <- min(n_cols * cellwidth_adj + 2, 50)  # 限制最大宽度为50厘米
  heatmap_height <- max(nrow(Cindex_mat) * cellheight, 10)  # 最小高度为10厘米

  # 将矩阵中的NA替换为0用于显示
  Cindex_mat_display <- as.matrix(Cindex_mat)
  Cindex_mat_display[is.na(Cindex_mat_display)] <- 0

  # Nature风格配色 - 使用colorRamp2创建连续色阶
  # 经典的蓝-白-红配色，适合AUC值展示
  col_fun <- colorRamp2(
    breaks = c(0.5, 0.75, 1.0),
    colors = c("#3C5488", "#FFFFFF", "#DC0000")  # Nature配色：深蓝-白-深红
  )

  # 创建热图
  Heatmap(Cindex_mat_display, name = "AUC",
          right_annotation = row_ha,
          top_annotation = col_ha,
          col = col_fun,  # 使用Nature风格配色
          rect_gp = gpar(col = "white", lwd = 0.5), # 白色边框更简洁
          cluster_columns = cluster_columns, cluster_rows = cluster_rows,
          show_column_names = FALSE,
          show_row_names = TRUE,
          row_names_side = "left",
          row_labels = row_labels,
          row_names_gp = gpar(fontsize = 8),  # 调整行名字体大小
          width = unit(heatmap_width, "cm"),
          height = unit(heatmap_height, "cm"),
          heatmap_legend_param = list(
            title = "AUC",
            title_gp = gpar(fontsize = 10, fontface = "bold"),
            labels_gp = gpar(fontsize = 9),
            legend_width = unit(4, "cm"),  # 横向显示时使用宽度
            direction = "horizontal"  # 图例横向排列
          ),
          cell_fun = function(j, i, x, y, w, h, col) {
            val <- Cindex_mat_display[i, j]
            if(!is.na(val) && val != 0) {
              # 根据背景色自动调整文字颜色
              text_col <- ifelse(val > 0.85 | val < 0.65, "white", "black")
              grid.text(label = format(val, digits = 3, nsmall = 3),
                        x, y, gp = gpar(fontsize = 7, col = text_col))
            }
          }
  )
}

# ============================
# 读取训练集数据
# ============================
cat("\n========================================\n")
cat("读取训练集数据...\n")

train_file <- train_files[1]  # 使用第一个训练集文件
Train_data <- read.table(train_file, header = T, sep = ",", check.names=F, row.names=1, stringsAsFactors=F)

# 从文件名提取数据集ID
train_dataset_id <- gsub("^train_|\\.csv$", "", train_file, ignore.case = TRUE)
cat(sprintf("训练集: %s (%d 样本, %d 特征)\n", train_dataset_id, nrow(Train_data), ncol(Train_data)-1))

# 样本类型识别函数 - 根据样本名称后缀识别
# _con 后缀 = 对照组 (Type=0)
# _tra 后缀 = 治疗组 (Type=1)
identify_sample_type <- function(sample_ids){
  types <- rep(NA, length(sample_ids))
  for(i in seq_along(sample_ids)){
    if(grepl("_con$", sample_ids[i], ignore.case = TRUE)){
      types[i] <- 0  # 对照组样本
    } else if(grepl("_tra$", sample_ids[i], ignore.case = TRUE)){
      types[i] <- 1  # 治疗组样本
    }
  }
  return(types)
}

# 自动识别训练集样本类型
sample_names <- rownames(Train_data)
auto_types <- identify_sample_type(sample_names)

# 检查是否成功识别
if(all(!is.na(auto_types))){
  # 替换Type列
  Train_data[, ncol(Train_data)] <- auto_types
  cat(sprintf("  样本类型识别: %d个疾病组(Type=1), %d个对照组(Type=0)\n",
              sum(auto_types == 1), sum(auto_types == 0)))
} else if(any(!is.na(auto_types))){
  # 部分识别成功
  Train_data[!is.na(auto_types), ncol(Train_data)] <- auto_types[!is.na(auto_types)]
  cat(sprintf("  部分识别: %d个疾病组, %d个对照组, %d个未识别(使用原Type值)\n",
              sum(auto_types == 1, na.rm = TRUE),
              sum(auto_types == 0, na.rm = TRUE),
              sum(is.na(auto_types))))
} else {
  cat("  使用文件中的Type列\n")
}

# 提取表达量数据和类别标签
Train_expr=Train_data[,1:(ncol(Train_data)-1),drop=F]
Train_class=Train_data[,ncol(Train_data),drop=F]

# ============================
# 读取所有测试集数据
# ============================
cat("\n读取测试集数据...\n")

Test_expr_list <- list()
Test_class_list <- list()
test_dataset_ids <- c()

for(i in seq_along(test_files)) {
  test_file <- test_files[i]

  # 从文件名提取数据集ID
  test_dataset_id <- gsub("^test_|\\.csv$", "", test_file, ignore.case = TRUE)
  test_dataset_ids <- c(test_dataset_ids, test_dataset_id)

  # 读取测试集
  Test_data <- read.table(test_file, header=T, sep=",", check.names=F, row.names=1, stringsAsFactors = F)
  cat(sprintf("测试集%d: %s (%d 样本, %d 特征)\n", i, test_dataset_id, nrow(Test_data), ncol(Test_data)-1))

  # 自动识别测试集样本类型
  test_sample_names <- rownames(Test_data)
  test_auto_types <- identify_sample_type(test_sample_names)

  # 检查是否成功识别
  if(all(!is.na(test_auto_types))){
    Test_data[, ncol(Test_data)] <- test_auto_types
    cat(sprintf("    样本类型识别: %d个疾病组(Type=1), %d个对照组(Type=0)\n",
                sum(test_auto_types == 1), sum(test_auto_types == 0)))
  } else if(any(!is.na(test_auto_types))){
    Test_data[!is.na(test_auto_types), ncol(Test_data)] <- test_auto_types[!is.na(test_auto_types)]
    cat(sprintf("    部分识别: %d个疾病组, %d个对照组, %d个未识别\n",
                sum(test_auto_types == 1, na.rm = TRUE),
                sum(test_auto_types == 0, na.rm = TRUE),
                sum(is.na(test_auto_types))))
  } else {
    cat("    使用文件中的Type列\n")
  }

  # 提取表达量数据和类别标签
  Test_expr_list[[test_dataset_id]] <- Test_data[,1:(ncol(Test_data)-1),drop=F]

  # 创建测试集标签，包含队列信息
  test_class_df <- data.frame(
    Cohort = rep(test_dataset_id, nrow(Test_data)),
    Type = Test_data[,ncol(Test_data)],
    row.names = rownames(Test_data),
    stringsAsFactors = FALSE
  )
  Test_class_list[[test_dataset_id]] <- test_class_df
}

# 合并所有测试集
Test_expr <- do.call(rbind, Test_expr_list)
Test_class <- do.call(rbind, Test_class_list)

cat(sprintf("\n合并后的测试集: %d 样本, %d 特征\n", nrow(Test_expr), ncol(Test_expr)))
cat(sprintf("测试集队列: %s\n", paste(unique(Test_class$Cohort), collapse = ", ")))

# ============================
# 交叉验证训练集和测试集的共同基因
# ============================
comgene <- intersect(colnames(Train_expr), colnames(Test_expr))
cat(sprintf("\n共同基因数量: %d\n", length(comgene)))

# 根据共同基因过滤数据
Train_expr <- as.matrix(Train_expr[,comgene])
Test_expr <- as.matrix(Test_expr[,comgene])

# 对数据进行标准化处理
Train_set = scaleData(data=Train_expr, centerFlags=T, scaleFlags=T)
Test_set = scaleData(data=Test_expr, cohort=Test_class$Cohort, centerFlags=T, scaleFlags=T)

# 在标准化后添加微小噪声（可选，用于避免数值问题）
set.seed(123)
noise <- matrix(rnorm(n = nrow(Train_set) * ncol(Train_set), mean = 0, sd = 0.01),
                nrow = nrow(Train_set), ncol = ncol(Train_set))
Train_set <- Train_set + noise

noise_test <- matrix(rnorm(n = nrow(Test_set) * ncol(Test_set), mean = 0, sd = 0.01),
                     nrow = nrow(Test_set), ncol = ncol(Test_set))
Test_set <- Test_set + noise_test

# 读取机器学习方法列表
methodRT <- read.table("refer.txt", header=T, sep="\t", check.names=F)
methods=methodRT$Model
methods <- gsub("-| ", "", methods) # 清理方法名称中的连字符和空格

# 准备运行机器学习模型的参数
classVar = "Type"         # 设置类变量的名称
Variable = colnames(Train_set)
preTrain.method =  strsplit(methods, "\\+") # 分解方法名称中的组合
preTrain.method = lapply(preTrain.method, function(x) rev(x)[-1]) # 反转并移除第一个元素
preTrain.method = unique(unlist(preTrain.method)) # 去除重复的方法名称

###################### 根据训练数据运行机器学习模型 ######################
cat("\n========================================\n")
cat("开始训练机器学习模型...\n")
cat("========================================\n\n")

# 第一阶段：使用机器学习方法选择变量
preTrain.var <- list()       # 初始化保存变量选择结果的列表
set.seed(seed = 123)         # 设置随机种子以保证结果的可重复性
for (method in preTrain.method){
  preTrain.var[[method]] = RunML(method = method,              # 指定机器学习方法
                                 Train_set = Train_set,        # 提供训练数据
                                 Train_label = Train_class,    # 提供类别标签
                                 mode = "Variable",            # 设置模式为变量选择
                                 classVar = classVar)          # 指定类变量
}
preTrain.var[["simple"]] <- colnames(Train_set) # 将简单模型的变量也保存下来
# 第二阶段：使用选定的变量建立机器学习模型
model <- list()            # 初始化保存模型结果的列表
set.seed(seed = 123)       # 再次设置随机种子
Train_set_bk <- Train_set  # 备份原始训练集

for (method in methods) {
  cat(match(method, methods), ":", method, "\n")

  # 拆分 simple+算法 这种组合
  parts <- strsplit(method, "\\+")[[1]]
  if (length(parts) == 1) parts <- c("simple", parts)

  # 1) 拿到这一 combo 选中的变量列表
  vars <- preTrain.var[[ parts[1] ]]

  # 2) 如果选中变量太少，跳过
  if (length(vars) <= min.selected.var) {
    message("  SKIP ", parts[1], " → only ", length(vars), " variables\n")
    next
  }

  # 3) 如果选中变量太多(超过20个)，只保留前20个
  if (length(vars) > max.selected.var) {
    message("  LIMIT ", parts[1], " → ", length(vars), " variables, keeping first ", max.selected.var, "\n")
    vars <- vars[1:max.selected.var]
  }

  # 4) 建立模型
  ts <- Train_set_bk[, vars, drop = FALSE]
  fit <- RunML(method      = parts[2],
               Train_set   = ts,
               Train_label = Train_class,
               mode        = "Model",
               classVar    = classVar)

  # 5) 检查模型最终使用的变量数量
  final_vars <- ExtractVar(fit)
  if (length(final_vars) <= min.selected.var) {
    message("  DROP ", method, " → only ", length(final_vars), " vars after modelling\n")
  } else if (length(final_vars) > max.selected.var) {
    message("  DROP ", method, " → ", length(final_vars), " vars after modelling (exceeds max ", max.selected.var, ")\n")
  } else {
    model[[ method ]] <- fit
  }
}

# 恢复原始训练集并清理备份
Train_set <- Train_set_bk
rm(Train_set_bk)

# 使用训练好的模型计算每个样本的风险分数
methodsValid <- names(model)                     # 获取有效的模型名称
# 计算预测的风险分数
RS_list <- list()
for (method in methodsValid){
  RS_list[[method]] <- CalPredictScore(fit = model[[method]], new_data = rbind.data.frame(Train_set,Test_set))
}
riskTab=as.data.frame(t(do.call(rbind, RS_list)))
riskTab=cbind(id=row.names(riskTab), riskTab)
write.table(riskTab, "model.riskMatrix.csv", sep=",", row.names=F, quote=F)

# 使用保存的模型预测每个样本的类别
Class_list <- list()
for (method in methodsValid){
  Class_list[[method]] <- PredictClass(fit = model[[method]], new_data = rbind.data.frame(Train_set,Test_set))
}
Class_mat <- as.data.frame(t(do.call(rbind, Class_list)))
classTab=cbind(id=row.names(Class_mat), Class_mat)
write.table(classTab, "model.classMatrix.csv", sep=",", row.names=F, quote=F)

# 提取每个有效模型选择的变量
fea_list <- list()
for (method in methodsValid) {
  tryCatch({
    vars <- ExtractVar(model[[method]])
    if(!is.null(vars) && length(vars) > 0) {
      fea_list[[method]] <- vars
    } else {
      fea_list[[method]] <- character(0)
    }
  }, error = function(e) {
    warning(sprintf("提取特征出错 (%s): %s", method, e$message))
    fea_list[[method]] <- character(0)
  })
}

# 构建特征数据框
fea_df_list <- lapply(names(fea_list), function(method){
  vars <- fea_list[[method]]
  if(length(vars) > 0) {
    data.frame(features = vars, algorithm = method, stringsAsFactors = FALSE)
  } else {
    NULL
  }
})
fea_df <- do.call(rbind, fea_df_list[!sapply(fea_df_list, is.null)])
if(!is.null(fea_df) && nrow(fea_df) > 0) {
  write.table(fea_df, file="model.genes.csv", sep = ",", row.names = F, col.names = T, quote = F)
}

# 计算每个模型的AUC值
cat("\n========================================\n")
cat("计算模型AUC值...\n")
cat("========================================\n")

AUC_list <- list()
for (method in methodsValid){
  AUC_list[[method]] <- RunEval(fit = model[[method]],      # 使用机器学习模型
                                Test_set = Test_set,        # 提供测试数据
                                Test_label = Test_class,    # 提供测试标签
                                Train_set = Train_set,      # 提供训练数据
                                Train_label = Train_class,  # 提供训练标签
                                Train_name = train_dataset_id,  # 使用训练集数据集ID
                                cohortVar = "Cohort",       # 指定队列变量
                                classVar = classVar)        # 指定类变量
}
AUC_mat <- do.call(rbind, AUC_list)
aucTab=cbind(Method=row.names(AUC_mat), AUC_mat)
write.table(aucTab, "model.AUCmatrix.csv", sep=",", row.names=F, quote=F)

############################## 绘制AUC热图 ##############################
cat("\n绘制AUC热图...\n")

# 准备热图的数据 - 直接使用内存中的AUC_mat，不重新读取CSV
# 将AUC_list转换为矩阵格式
AUC_mat <- do.call(rbind, AUC_list)

# 移除全为NA的行
AUC_mat <- AUC_mat[apply(AUC_mat, 1, function(x) !all(is.na(x))), , drop=FALSE]

# 检查是否有有效数据
if(nrow(AUC_mat) == 0) {
  cat("警告：没有有效的AUC数据，跳过热图绘制\n")
} else {
  # 将NA替换为0以便计算平均值
  AUC_mat_calc <- AUC_mat
  AUC_mat_calc[is.na(AUC_mat_calc)] <- 0

  # 计算并排序AUC的平均值
  avg_AUC <- apply(AUC_mat_calc, 1, mean, na.rm = TRUE)
  avg_AUC <- sort(avg_AUC, decreasing = T)
  AUC_mat <- AUC_mat[names(avg_AUC), , drop=FALSE]

  # 获取最佳模型的变量选择结果
  if(length(fea_list) > 0 && rownames(AUC_mat)[1] %in% names(fea_list)) {
    fea_sel <- fea_list[[rownames(AUC_mat)[1]]]
  } else {
    fea_sel <- character(0)
  }

  # 保存带名称的avg_AUC用于后续ROC曲线绘制
  avg_AUC_named <- avg_AUC
  # 转换为数值格式用于热图显示
  avg_AUC <- as.numeric(format(avg_AUC, digits = 3, nsmall = 3))

  # 计算每个模型的基因数量（处理NULL和缺失值）
  gene_counts <- sapply(rownames(AUC_mat), function(method) {
    if(method %in% names(fea_list) && !is.null(fea_list[[method]])) {
      length(fea_list[[method]])
    } else {
      0
    }
  })

  # 定义热图的颜色和注释 - Nature风格配色
  # Nature常用配色方案
  nature_colors <- c(
    "#E64B35",  # 红色 (Nature Red)
    "#4DBBD5",  # 青色 (Nature Cyan)
    "#00A087",  # 绿色 (Nature Green)
    "#3C5488",  # 蓝色 (Nature Blue)
    "#F39B7F",  # 橙色 (Nature Orange)
    "#8491B4",  # 紫灰色
    "#91D1C2",  # 浅绿色
    "#DC0000",  # 深红色
    "#7E6148",  # 棕色
    "#B09C85"   # 米色
  )

  n_cohorts <- ncol(AUC_mat)
  if (n_cohorts <= length(nature_colors)) {
    CohortCol <- nature_colors[1:n_cohorts]
  } else {
    CohortCol <- colorRampPalette(nature_colors)(n_cohorts)
  }
  names(CohortCol) <- colnames(AUC_mat)

  # 绘制热图
  cellwidth = 1; cellheight = 0.5
  hm <- SimpleHeatmap(Cindex_mat = AUC_mat,
                      avg_Cindex = avg_AUC,
                      CohortCol = CohortCol,
                      barCol = "#3C5488",  # Nature蓝色
                      cellwidth = cellwidth, cellheight = cellheight,
                      cluster_columns = F, cluster_rows = F,
                      gene_counts = gene_counts)

  # 保存热图为PDF文件
  pdf(file="model.AUCheatmap.pdf", width=cellwidth * ncol(AUC_mat) + 6, height=cellheight * nrow(AUC_mat) * 0.45+5)
  draw(hm, heatmap_legend_side="top", annotation_legend_side="top")
  dev.off()
  cat("AUC热图已保存到 model.AUCheatmap.pdf\n")
}

############################## 绘制ROC曲线 ##############################
cat("\n开始绘制ROC曲线...\n")

# 检查是否有有效的模型可以绘制ROC曲线
if(!exists("avg_AUC_named") || length(avg_AUC_named) == 0) {
  cat("警告：没有有效的模型AUC数据，跳过ROC曲线绘制\n")
} else {
  # 创建ROC曲线保存文件夹
  roc_dir <- "ROC_Curves"
  if(!dir.exists(roc_dir)){
    dir.create(roc_dir)
    cat(sprintf("已创建文件夹: %s\n", roc_dir))
  }

  # 选择排名前N的模型（按平均AUC排序）
  top_models <- names(avg_AUC_named)[1:min(top.models.for.roc, length(avg_AUC_named))]
  # 过滤掉不存在的模型
  top_models <- top_models[top_models %in% names(model)]

  if(length(top_models) == 0) {
    cat("警告：没有有效的模型可以绘制ROC曲线\n")
  } else {
    cat(sprintf("将绘制排名前 %d 个模型的ROC曲线\n", length(top_models)))
    cat(sprintf("选中的模型: %s\n", paste(top_models, collapse = ", ")))

    # 准备数据：合并训练集和测试集
    All_set <- rbind.data.frame(Train_set, Test_set)
    All_class <- rbind.data.frame(
      data.frame(Cohort = train_dataset_id, Type = Train_class$Type),
      Test_class
    )

    # 为每个top模型绘制ROC曲线
    for(i in seq_along(top_models)){
      method <- top_models[i]
      cat(sprintf("  [%d/%d] 正在绘制 %s 的ROC曲线...\n", i, length(top_models), method))

      # 检查模型是否存在
      if(is.null(model[[method]])) {
        cat(sprintf("    跳过：模型 %s 不存在\n", method))
        next
      }

      # 计算风险评分
      RS <- tryCatch({
        CalPredictScore(fit = model[[method]], new_data = All_set)
      }, error = function(e) {
        warning(sprintf("计算风险评分出错 (%s): %s", method, e$message))
        return(rep(NA, nrow(All_set)))
      })

      # 检查RS是否全为NA
      if(all(is.na(RS))) {
        cat(sprintf("    跳过：模型 %s 的预测结果全为NA\n", method))
        next
      }

      # 准备绘图数据
      plot_data <- All_class
      plot_data$RS <- RS
      plot_data_list <- split(plot_data, plot_data$Cohort)

      # 设置颜色 - Nature风格配色
      roc_nature_colors <- c(
        "#E64B35",  # 红色
        "#4DBBD5",  # 青色
        "#00A087",  # 绿色
        "#3C5488",  # 蓝色
        "#F39B7F",  # 橙色
        "#8491B4",  # 紫灰色
        "#91D1C2",  # 浅绿色
        "#DC0000",  # 深红色
        "#7E6148",  # 棕色
        "#B09C85"   # 米色
      )
      n_cohorts <- length(plot_data_list)
      if (n_cohorts <= length(roc_nature_colors)) {
        colors <- roc_nature_colors[1:n_cohorts]
      } else {
        colors <- colorRampPalette(roc_nature_colors)(n_cohorts)
      }

      # 创建PDF文件
      pdf(file = file.path(roc_dir, paste0(gsub("[+\\[\\]]", "_", method), ".ROC.pdf")),
          width = 8, height = 8)

      # 设置绘图参数
      par(mar = c(5, 5, 4, 2))

      # 初始化绘图区域
      plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
           xlab = "False Positive Rate (1 - Specificity)",
           ylab = "True Positive Rate (Sensitivity)",
           main = paste0("ROC Curves - ", method),
           cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4)

      # 添加对角线
      abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)

      # 存储每个队列的ROC对象和AUC值
      roc_list <- list()
      auc_values <- c()

      # 为每个队列绘制ROC曲线
      for(j in seq_along(plot_data_list)){
        cohort_name <- names(plot_data_list)[j]
        cohort_data <- plot_data_list[[j]]

        # 检查是否有两个类别
        if(length(unique(cohort_data$Type)) < 2){
          warning(sprintf("队列 %s 只有一个类别，跳过ROC曲线绘制", cohort_name))
          next
        }

        # 检查是否有NA值
        valid_idx <- !is.na(cohort_data$RS) & !is.na(cohort_data$Type)
        if(sum(valid_idx) < 2){
          warning(sprintf("队列 %s 有效样本数不足，跳过ROC曲线绘制", cohort_name))
          next
        }

        # 计算ROC曲线
        tryCatch({
          roc_obj <- roc(cohort_data$Type[valid_idx], cohort_data$RS[valid_idx],
                         quiet = TRUE, levels = c(0, 1), direction = "<")

          # 绘制ROC曲线
          lines(1 - roc_obj$specificities, roc_obj$sensitivities,
                col = colors[j], lwd = 3)

          # 保存ROC对象
          roc_list[[cohort_name]] <- roc_obj

          # 使用已保存的AUC矩阵中的值（确保与热图一致）
          if(method %in% rownames(AUC_mat) && cohort_name %in% colnames(AUC_mat)) {
            auc_values[cohort_name] <- AUC_mat[method, cohort_name]
          } else {
            # 如果找不到已保存的值，则计算新值
            auc_values[cohort_name] <- as.numeric(auc(roc_obj))
          }
        }, error = function(e){
          warning(sprintf("队列 %s 绘制ROC曲线时出错: %s", cohort_name, e$message))
        })
      }

      # 添加图例
      if(length(roc_list) > 0){
        # 计算平均AUC值
        mean_auc <- mean(auc_values)

        # 构建图例文本
        legend_text <- paste0(names(auc_values), " (AUC = ",
                             sprintf("%.3f", auc_values), ")")
        # 添加平均AUC
        legend_text <- c(legend_text,
                         paste0("─────────────────"),
                         paste0("Average AUC = ", sprintf("%.3f", mean_auc)))

        # 准备图例颜色（最后两行没有颜色线）
        legend_colors <- c(colors[1:length(roc_list)], NA, NA)
        legend_lwd <- c(rep(3, length(roc_list)), NA, NA)

        legend("bottomright", legend = legend_text,
               col = legend_colors, lwd = legend_lwd,
               bty = "n", cex = 1.1)
      }

      # 添加网格
      grid(col = "gray90", lty = 1)

      dev.off()
      cat(sprintf("    已保存: %s\n", file.path(roc_dir, paste0(gsub("[+\\[\\]]", "_", method), ".ROC.pdf"))))
    }

    cat(sprintf("\nROC曲线已全部保存到文件夹: %s\n", roc_dir))
    cat("ROC曲线绘制完成!\n\n")
  }
}

cat("\n===== 机器学习分析完成 =====\n")
cat(sprintf("训练集: %s\n", train_dataset_id))
cat(sprintf("测试集: %s\n", paste(test_dataset_ids, collapse = ", ")))
cat("========================================\n")

# 请确保安装并加载 prROC
# install.packages("prROC")
library(PRROC)

cat("\n开始绘制PR曲线...\n")
pr_dir <- "PR_Curves"
if(!dir.exists(pr_dir)) dir.create(pr_dir)

# 假设 top_models 和 All_set 等变量在你的 ROC 代码部分已经生成
for(i in seq_along(top_models)){
  method <- top_models[i]
  
  # 获取模型预测分数
  RS <- CalPredictScore(fit = model[[method]], new_data = All_set)
  
  plot_data <- All_class
  plot_data$RS <- RS
  plot_data_list <- split(plot_data, plot_data$Cohort)
  
  # 创建 PDF 保存 PR 曲线
  pdf(file = file.path(pr_dir, paste0(gsub("[+\\[\\]]", "_", method), ".PRC.pdf")), width = 8, height = 8)
  
  # 初始化绘图
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Recall (Sensitivity)", ylab = "Precision (Positive Predictive Value)",
       main = paste0("Precision-Recall Curves - ", method),
       cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4)
  
  # Nature 配色（与你原脚本一致）
  pr_colors <- c("#E64B35", "#4DBBD5", "#00A087") 
  legend_text <- c()
  auc_pr_values <- c()
  
  for(j in seq_along(plot_data_list)){
    cohort_name <- names(plot_data_list)[j]
    cohort_data <- plot_data_list[[j]]
    valid_idx <- !is.na(cohort_data$RS) & !is.na(cohort_data$Type)
    
    # prROC 包需要分别传入阳性样本得分和阴性样本得分
    fg_scores <- cohort_data$RS[valid_idx & cohort_data$Type == 1] # 阳性 (RA)
    bg_scores <- cohort_data$RS[valid_idx & cohort_data$Type == 0] # 阴性 (HC)
    
    # 计算 PR 曲线对象
    pr_obj <- pr.curve(scores.class0 = fg_scores, scores.class1 = bg_scores, curve = TRUE)
    
    # 绘制曲线
    lines(pr_obj$curve[,1], pr_obj$curve[,2], col = pr_colors[j], lwd = 3)
    
    auc_pr_values[cohort_name] <- pr_obj$auc.integral
    legend_text <- c(legend_text, paste0(cohort_name, " (AUC-PR = ", sprintf("%.3f", pr_obj$auc.integral), ")"))
  }
  
  legend("bottomleft", legend = legend_text, col = pr_colors[1:length(plot_data_list)], lwd = 3, bty = "n", cex = 1.1)
  grid(col = "gray90", lty = 1)
  dev.off()
}
cat("PR曲线绘制完成并已保存!\n")