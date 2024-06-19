#' sense
#'
#' @param df A data frame with features and target.
#' @param target_feat String. Name of the numeric feature for the regression task.
#' @param benchmarking Positive integer. Number of base learners to stack. Default: "all".
#' @param super String. Super learner of choice among the available learners. Default: "avg".
#' @param algos String vector. Available learners are: "glmnet", "ranger", "xgboost", "rpart", "kknn", "svm".
#' @param sampling_rate Positive numeric. Sampling rate before applying the stacked ensemble. Default: 1.
#' @param metric String. Evaluation metric for outer and inner cross-validation. Default: "mae".
#' @param collapse_char_to Positive integer. Conversion of characters to  factors with predefined maximum number of levels. Default: 10.
#' @param num_preproc String. Options for scalar pre-processing: "scale" or "range". Default: "scale".
#' @param fct_preproc String. Options for factor pre-processing: "encodeimpact", "encodelmer", "one-hot", "treatment", "poly", "sum", "helmert". Default: "one-hot".
#' @param impute_num String. Options for missing imputation in case of numeric: "sample" or "hist". Default: "sample". For factor the default mode is Out-Of-Range.
#' @param missing_fusion String. Adding missing indicator features. Default: "FALSE".
#' @param inner String. Cross-validation inner cycle: "holdout", "cv", "repeated_cv", "subsampling". Default: "holdout".
#' @param outer String. Cross-validation outer cycle: "holdout", "cv", "repeated_cv", "subsampling". Default: "holdout".
#' @param folds Positive integer. Number of repetitions used in "cv" and "repeated_cv". Default: 3.
#' @param repeats Positive integer. Number of repetitions used in "subsampling" and "repeated_cv". Default: 3.
#' @param ratio Positive numeric. Percentage value for "holdout" and "subsampling". Default: 0.5.
#' @param selected_filter String. Filters available for regression tasks: "carscore", "cmim", "correlation", "find_correlation", "information_gain", "relief", "variance". Default: "information_gain".
#' @param selected_n_feats Positive integer. Number of features to select through the chosen filter. Default: NULL.
#' @param tuning String. Available options are "random_search" and "grid_search". Default: "random_search".
#' @param budget Positive integer. Maximum number of trials during random search. Default: 30.
#' @param resolution Positive integer. Grid resolution for each hyper-parameter. Default: 5.
#' @param n_evals Positive integer. Number of evaluation for termination. Default: 30.
#' @param minute_time Positive integer. Maximum run time before termination. Default: 10.
#' @param patience Positive numeric. Percentage of stagnating evaluations before termination. Default: 0.3.
#' @param min_improve Positive numeric. Minimum error improvement required before termination. Default: 0.01.
#' @param java_mem Positive integer. Memory allocated to Java. Default: 64.
#' @param decimals Positive integer. Decimal format of prediction. Default: 2.
#' @param seed Positive integer. Default: 42.
#'
#' @author Giancarlo Vercellino \email{giancarlo.vercellino@gmail.com}
#'
#'@return This function returns a list including:
#' \itemize{
#'\item benchmark_error: comparison between the base learners
#'\item resampled_model: mlr3 standard description of the analytic pipeline.
#'\item plot: mlr3 standard graph of the analytic pipeline.
#'\item selected_n_feats: selected features and score according to the filtering method used.
#'\item model_error: error measure for outer cycle of cross-validation.
#'\item testing_frame: data set used for calculating the test metrics.
#'\item test_metrics: metrics reported are mse, rmse, mae, mape, mdae, rae, rse, rrse, smape.
#'\item model_predict: prediction function to apply to new data on the same scheme.
#'\item time_log: computation time.
#' }
#'
#' @export
#'
#' @import mlr3
#' @import mlr3learners
#' @import mlr3filters
#' @import mlr3pipelines
#' @import mlr3viz
#' @import paradox
#' @import mlr3tuning
#' @import bbotk
#'
#' @import tictoc
#' @import forcats
#' @import readr
#' @import lubridate
#' @import purrr
#' @importFrom  data.table setDF
#' @import Metrics
#' @import visNetwork
#'
#'@examples
#'\dontrun{
#'sense(benchmark, "y", algos = c("glmnet", "rpart"))
#'
#'}
#'
sense <- function(df, target_feat, benchmarking = "all",
                               super = "avg", algos = c("glmnet", "ranger", "xgboost", "rpart", "kknn", "svm"),
                               sampling_rate = 1, metric = "mae", collapse_char_to = 10, num_preproc = "scale", fct_preproc = "one-hot", impute_num = "sample", missing_fusion = FALSE,
                               inner = "holdout", outer = "holdout", folds = 3, repeats = 3, ratio = 0.5, selected_filter = "information_gain", selected_n_feats = NULL,
                               tuning = "random_search", budget = 30, resolution = 5,
                               n_evals = 30, minute_time = 10, patience = 0.3, min_improve = 0.01,
                               java_mem = 64, decimals = 2, seed = 42)
{
  tic.clearlog()
  tic("sense")

  options(java.parameters = paste0("-Xmx", java_mem, "g"))

  ###PRESET
  set.seed(seed)

  supported <- c("glmnet", "ranger", "xgboost", "rpart", "kknn", "svm")

  if(length(algos)<2){stop("need at least 2 base learners")}
  if(any(!(algos %in% supported))){stop("algos not supported")}
  if(!(super %in% c(supported, "avg"))){stop("algos not supported")}

  if(is.null(selected_n_feats)){selected_n_feats <- ncol(df)-1}
  n_objects <- nrow(df)
  any_negative <- any(df[, target_feat] < 0)
  any_double <- any(df[, target_feat]%%1 > 0)

  algos <- setdiff(algos, super)

  ###TASK & PIPEOPS
  df <- TaskRegr$new(id = "df", backend = as.data.table(df), target = target_feat)

  lgl_conversion <- PipeOpSelect$new(id = "lgl_selector",  param_vals = list(selector = selector_type(c("logical")))) %>>% PipeOpColApply$new(id = "lgl_apply", param_vals = list(applicator = as.factor))
  char_conversion <- PipeOpSelect$new(id = "char_selector",  param_vals = list(selector = selector_type(c("character")))) %>>% PipeOpColApply$new(id = "char_apply", param_vals = list(applicator = as.factor)) %>>% PipeOpCollapseFactors$new(id = "collapsefactors", param_vals = list(target_level_count = collapse_char_to))

  if(missing_fusion==TRUE){missind <- po("missind", which = "all", type = "factor")}
  fct_imputation <- po("imputeoor")

  if(!(impute_num %in% c("sample", "hist"))){stop("impute_num options are 'sample' or 'hist'")}
  num_imputation <- po(paste0("impute", impute_num))###PARAMS: "sample" or "hist"
  if(num_preproc != "nop"){num_transformation <- po(num_preproc)}###SCALE,RANGE
  if(num_preproc == "nop"){num_transformation <- PipeOpNOP$new(id = paste0("num_preproc_", num_preproc))}

  if(!(fct_preproc %in% c("encodeimpact", "encodelmer", "one-hot", "treatment", "poly", "sum", "helmert"))){stop("fct_preproc options are 'encodeimpact', 'encodelmer', 'one-hot', 'treatment', 'poly', 'sum', 'helmert'")}
  if(fct_preproc == "encodeimpact"){fct_encoding <- po("fixfactors") %>>% PipeOpEncodeImpact$new(id = "preproc_encodeimpact", param_vals = list(impute_zero = TRUE))}###DONE THE LONG WAY TO AVOID ID CONFLICTS
  if(fct_preproc == "encodelmer"){fct_encoding <- po("fixfactors") %>>% PipeOpEncodeLmer$new(id = "preproc_encodelmer")}
  if(fct_preproc %in% c("one-hot", "treatment", "poly", "sum", "helmert")){fct_encoding <- po("fixfactors") %>>% PipeOpEncode$new(id = "preproc_encode", param_vals = list(method = fct_preproc))}
  if(fct_preproc == "nop"){fct_encoding <- po("fixfactors") %>>% PipeOpNOP$new(id = paste0("fct_preproc", fct_preproc))}

  removing_const <- po("removeconstants")

  if(!(selected_filter %in% c("carscore", "cmim", "correlation", "find_correlation", "information_gain", "relief", "variance"))){stop("selected_filter options are 'carscore', 'cmim', 'correlation', 'find_correlation', 'information_gain', 'relief', 'variance'")}
  filtering <- po("filter", filter = flt(selected_filter), filter.nfeat = selected_n_feats)
  subsampling <- po("subsample", frac = sampling_rate, replace = FALSE)
  cleaner <- po("select", selector = selector_invert(selector_type(c("character", "logical"))))

  ###FROM PIPELINES TO GRAPH
  conversion <- gunion(list(cleaner, lgl_conversion, char_conversion)) %>>% PipeOpFeatureUnion$new(innum = 0, id = "conversion_fusion")

  imputation <- num_imputation %>>% fct_imputation

  if(missing_fusion==TRUE)
  {
    missing_fusion <- PipeOpFeatureUnion$new(innum = 0, id = "missing_fusion")
    imputation <-  gunion(list(imputation, missind)) %>>% missing_fusion
  }

  transformation <- num_transformation %>>% fct_encoding %>>% removing_const

  preprocessing <- conversion %>>% imputation %>>% transformation %>>% filtering %>>% subsampling

  if(!(inner %in% c("cv", "repeated_cv", "holdout", "subsampling"))){stop("inner options are 'cv','holdout', 'repeated_cv', 'subsampling'")}
  if(inner == "cv"){inner_rsmp_strat <- rsmp(inner, folds = folds)}
  if(inner == "holdout"){inner_rsmp_strat <- rsmp(inner, ratio = ratio)}
  if(inner == "repeated_cv"){inner_rsmp_strat <- rsmp(inner, repeats = repeats, folds = folds)}
  if(inner == "subsampling"){inner_rsmp_strat <- rsmp(inner, repeats = repeats, ratio = ratio)}

  selected <- algos
  n_selected <- length(selected)
  if(benchmarking == "all" | benchmarking == n_selected){base_lrns <- purrr::map(selected, ~ po("learner_cv", learner = lrn(paste0("regr.", .x), predict_type = "response")))}

  if(is.numeric(benchmarking) & benchmarking > n_selected){stop("benchmarking requires more algorithms")}

  metric_strat <- msr(paste0("regr.", metric))

  benchmark_error <- NULL
  if(benchmarking >= 1 & benchmarking < n_selected)
  {
    preprocessed <- TaskRegr$new(id = "preprocessed", backend = preprocessing$train(input = df)[[1]]$data(), target = target_feat)
    bench_lrns <- purrr::map(selected, ~ lrn(paste0("regr.", .x), predict_type = "response"))
    design <- benchmark_grid(tasks = list(preprocessed), learners = bench_lrns, resamplings = list(rsmp("holdout", ratio = ratio)))
    bmr <- benchmark(design, store_models = TRUE)
    benchmark_error <- setDF(as.data.table(bmr$aggregate(metric_strat)))[,4:(6 + length(metric))]
    selected <- benchmark_error$learner_id[order(benchmark_error[,3], decreasing = FALSE)][1:benchmarking]###ORDERING IN RESPECT TO THE FIRST METRIC
    selected <- gsub("regr.", "", selected)
    message(paste0("selecting ", paste(selected, collapse = ", "), collapse = ""))
    base_lrns <- purrr::map(selected, ~ po("learner_cv", learner = lrn(paste0("regr.", .x), predict_type = "response")))
  }

  n_algos <- length(base_lrns)

  if(super != "avg"){super_lrn <-  po("learner", learner = lrn(paste0("regr.", super), predict_type = "response"))}
  if(super == "avg"){super_lrn <- po("learner", learner = LearnerRegrAvg$new(id="regr.avg"), predict_type = "response")}

  graph <- preprocessing %>>% gunion(list(base_lrns)) %>>% po("featureunion", innum = n_algos) %>>% super_lrn
  plot <- graph$plot(html=TRUE)

  ensemble <- GraphLearner$new(graph)

  ###HYPERPARAMS
  param_range <- NULL

  if("rpart" %in% c(selected, super))
  {
    param_range <- c(param_range,
                     regr.rpart.cp = p_dbl(lower = 0, upper = 1),
                     regr.rpart.minsplit = p_int(lower = 1, upper = selected_n_feats),
                     regr.rpart.maxdepth = p_int(lower = 1, upper = 10))
  }

  if("glmnet" %in% c(selected, super))
  {
    param_range <- c(param_range,
                     regr.glmnet.alpha = p_dbl(lower = 0, upper = 1),
                     regr.glmnet.s = p_dbl(lower = 1, upper = 1000))
  }

  if("kknn" %in% c(selected, super))
  {
    param_range <- c(param_range,
                     regr.kknn.k = p_int(lower = 1, upper = round(n_objects/10)),
                     regr.kknn.kernel = p_fct(levels = c("rectangular","triangular", "epanechnikov", "biweight", "triweight","cos", "gaussian", "optimal", "inv")))
  }

  if("xgboost" %in% c(selected, super))
  {
    param_range <- c(param_range,
                     regr.xgboost.booster = p_fct(levels = c("gbtree", "gblinear")),
                     regr.xgboost.alpha = p_dbl(lower = 0, upper = 1000, depends = quote(regr.xgboost.booster == "gblinear")),
                     regr.xgboost.eta = p_dbl(lower = 0, upper = 1, depends = quote(regr.xgboost.booster == "gbtree")),
                     regr.xgboost.gamma = p_dbl(lower = 0, upper = 1000, depends = quote(regr.xgboost.booster == "gbtree")),
                     regr.xgboost.lambda = p_dbl(lower = 0, upper = 1000, depends = quote(regr.xgboost.booster == "gblinear")))
  }

  if("ranger" %in% c(selected, super))
  {
    param_range <- c(param_range,
                     regr.ranger.splitrule = p_fct(levels = c("variance", "extratrees", "maxstat")),
                     regr.ranger.alpha = p_dbl(lower = 0, upper = 1, depends = quote(regr.ranger.splitrule == "maxstat")),
                     regr.ranger.min.node.size = p_int(lower = 1, upper = 30),
                     regr.ranger.mtry = p_int(lower = 0, upper = selected_n_feats))
  }

  if("svm" %in% c(selected, super))
  {
    param_range <- c(param_range,
                     regr.svm.kernel = p_fct(levels = c("linear", "polynomial", "radial", "sigmoid")),
                     regr.svm.type = p_fct(levels = c("eps-regression", "nu-regression")),
                     regr.svm.nu = p_dbl(lower = 0, upper = 1, depends = quote(regr.svm.type == "nu-regression")),
                     regr.svm.gamma = p_dbl(lower = 1, upper = 1000, depends = quote(regr.svm.kernel %in% c("polynomial", "radial", "sigmoid"))),
                     regr.svm.coef0 = p_dbl(lower = -1000, upper = 1000, depends = quote(regr.svm.kernel %in% c("polynomial", "sigmoid"))))
  }

  param_strat <- do.call(ps, param_range)

  ###FINE TUNING
  term_strat <- trm("combo", list(trm("stagnation", iters = ceiling(n_evals * patience), threshold = min_improve), trm("evals", n_evals = n_evals), trm("run_time", secs = minute_time * 60)), any = TRUE)

  if(!(tuning %in% c("random_search", "grid_search"))){stop("tuning options are 'random_search', 'grid_search'")}
  if(tuning == "random_search"){tune_strat = tnr("random_search", batch_size = budget)}###MAY NOT WORK BECAUSE OF NAME CONFLICT WITH MLR3_MEASURES
  if(tuning == "grid_search"){tune_strat = tnr("grid_search", resolution = resolution, batch_size = budget)}

  autotuned_learner <- AutoTuner$new(learner = ensemble, resampling = inner_rsmp_strat, measure = metric_strat, terminator = term_strat, tuner = tune_strat, search_space = param_strat)

  autotuned_learner$train(task = df)

  if(!(outer %in% c("cv", "repeated_cv", "holdout", "subsampling"))){stop("inner options are 'cv','holdout', 'repeated_cv', 'subsampling'")}
  if(outer == "cv"){outer_rsmp_strat <- rsmp(outer, folds = folds)}
  if(outer == "holdout"){outer_rsmp_strat <- rsmp(outer, ratio = ratio)}
  if(outer == "repeated_cv"){outer_rsmp_strat <- rsmp(outer, repeats = repeats, folds = folds)}
  if(outer == "subsampling"){outer_rsmp_strat <- rsmp(outer, repeats = repeats, ratio = ratio)}

  resampled_model <- resample(task = df, learner = autotuned_learner, resampling = outer_rsmp_strat, store_models = TRUE)
  resampling_measures <- setDF(as.data.table(resampled_model$score(metric_strat)))
  resampling_measures <- resampling_measures[, c(6, 7, 9)]
  model_error <- list(resampling_measures = resampling_measures, aggregation = resampled_model$aggregate(metric_strat))

  resampled_learner <- as.data.table(resampled_model)$learner[[1]]
  resampled_task <- as.data.table(resampled_model)$task[[1]]

  testing_frame <- setDF(as.data.table(resampled_model$prediction()))[, c(2, 3)]
  colnames(testing_frame) <- c("truth", "prediction")
  testing_frame$prediction <- round(testing_frame$prediction, decimals)

  test_metrics <- unlist(purrr::map(list(mse, rmse, mae, mape, mdae, rae, rse, rrse, smape), ~ .x(actual = testing_frame$truth, predicted = testing_frame$prediction)))
  names(test_metrics) <- c("mse", "rmse", "mae", "mape", "mdae", "rae", "rse", "rrse", "smape")

  selected_n_feats <- list(feats = resampled_model$learners[[1]]$model$learner$state$model[[selected_filter]]$features, score = resampled_model$learners[[1]]$model$learner$state$model[[selected_filter]]$score)

  model_predict <- function(data)
  {
    prediction <- resampled_learner$predict_newdata(newdata = data, task = resampled_task)
    prediction <- setDF(as.data.table(prediction))[, 3, drop = FALSE]
    colnames(prediction) <- target_feat
    prediction[, target_feat] <- round(prediction[, target_feat], decimals)
    return(prediction)
  }

  toc(log = TRUE)
  time_log<-seconds_to_period(parse_number(unlist(tic.log())))

  outcome <- list(benchmark_error = benchmark_error, resampled_model = resampled_model, plot = plot,
                  selected_n_feats = selected_n_feats, model_error = model_error, testing_frame = testing_frame,
                  test_metrics = test_metrics, model_predict = model_predict, time_log = time_log)

  return(outcome)

}
