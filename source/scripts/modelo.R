# FEATURE ENGINEERING -----------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())


#INSTALACAO E/OU CARREGAMENTO DOS PACOTES
pacotes = c("dplyr", "lubridate", "stringr", "tictoc", "ggplot2", "ggeasy", "ggpubr", "tidymodels", "probably",
            "ranger", "doSNOW", "xgboost", "abjutils", "forcats", "themis", "tidyr", "kernlab", "gridExtra")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

#CARREGANDO A BASE
load("data/data.Rdata", verbose = T)

#CRIACAO DE VARIAVEIS
data_new_var = salaries_internet %>%
  dplyr::select(-c(Broadband.Speed.Rank, Mobile.Speed.Rank, As.of, work_year, remote_ratio, Country)) %>%
  dplyr::mutate(target = dplyr::case_when(target=='SIM' ~ '1',
                                          TRUE ~ '0'))%>%
  dplyr::mutate_if(is.integer, as.numeric)%>%
  dplyr::mutate_if(is.character, as_factor)


# PRE PROCESS -----------------------------------------------------------

#DEFININDO SEMENTE
set.seed(364221)

#DIVIDINDO A POPULACAO EM TREINO E TESTE
data_split = rsample::initial_split(data = data_new_var, prop = 0.7, strata = target)

#BASE DE TREINO E TESTE
data_train = rsample::training(x = data_split)
data_test = rsample::testing(data_split)

#LIBERANDO ESPAÇO REMOVENDO O OBJETO DATA
remove(data_new_var)

#DATA PREPOCESSING
data_recipe = data_train %>%
  recipes::recipe(target ~ .) %>%
  recipes::step_relevel(target, ref_level = "1") %>%
  #themis::step_downsample(target, under_ratio = 1) %>%
  recipes::step_impute_knn(recipes::all_numeric_predictors(), -recipes::all_outcomes()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors(), -recipes::all_outcomes()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors(), -recipes::all_outcomes()) %>% 
  recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.7, method = "pearson") %>%   
  recipes::step_nzv(recipes::all_numeric_predictors(), -recipes::all_outcomes()) #%>% 
  #recipes::step_pca(recipes::all_numeric_predictors(), threshold = .8)

#VERIFICACAO DA BASE DE TREINO POS RECIPES
train = data_recipe %>%
  recipes::prep() %>%
  recipes::juice()

#LINHAS E COLUNAS
dim(train)

#DISTRIBUICAO DAS CLASSES
table(train$target)

#VERIFICACAO DA BASE DE TESTE POS RECIPES
test = data_recipe %>%
  recipes::prep() %>%
  recipes::bake(rsample::testing(data_split))

#LINHAS E COLUNAS
dim(test)

#DISTRIBUICAO DAS CLASSES
table(test$target)/nrow(test)

#CROSS VALIDATION
cv = rsample::vfold_cv(data = data_train, v = 3, repeats = 2, strata = target)


# RANDOM FOREST -----------------------------------------------------------

#ESPECIFICANDO O MODELO
rf_spec = parsnip::rand_forest() %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
rf_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(rf_spec)

#SALVANDO RDATA DO WORKFLOW
save(rf_workflow, file = "Data/rf4_workflow.Rdata")

#GRID DE PARAMETROS
rf_grid = expand.grid(mtry = c(3, 4, 5))

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#AJUSTE DO MODELO
rf_fit = rf_workflow %>%
  tune::tune_grid(resamples = cv, 
                  grid = rf_grid,
                  metrics = yardstick::metric_set(yardstick::recall, 
                                                  yardstick::spec,
                                                  yardstick::f_meas, 
                                                  yardstick::accuracy, 
                                                  yardstick::kap, 
                                                  yardstick::roc_auc),
                  control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(rf_fit, file = "Data/rf4_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O MODELO
load(file = "Data/rf4_model.Rdata", verbose = T)

#COLETANDO METRICAS
rf_fit %>% 
  tune::collect_metrics(summarize = T) %>% 
  dplyr::filter(.metric %in% c('accuracy', 'recall', 'spec')) %>%
  dplyr::select(-n, -std_err, -.config, -.estimator)

#MOSTRANDO O MELHOR MODELO NO TREINO
rf_fit %>% tune::show_best(metric = "recall")

#SELECIONANDO O MODELO COM OS MELHORES PARAMETROS
rf_best_param = rf_fit %>% 
  tune::select_best(metric = "recall")

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/rf4_workflow.Rdata", verbose = T)

#FINALIZANDO O workflow COM OS MELHORES PARAMETROS
rf_best_workflow = rf_workflow %>% 
  tune::finalize_workflow(rf_best_param)

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#VALIDACAO 
rf_last_fit = tune::last_fit(rf_best_workflow, 
                             split = data_split,
                             metrics = yardstick::metric_set(yardstick::recall, 
                                                             yardstick::spec,
                                                             yardstick::f_meas, 
                                                             yardstick::accuracy, 
                                                             yardstick::kap,
                                                             yardstick::roc_auc)
)

#SALVANDO OBJETO DO MODEL EVALUATION
save(rf_last_fit, file = "Data/rf4_last_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/rf4_last_model.Rdata", verbose = T)

#COLETANDO METRICAS
rf_last_fit %>% 
  tune::collect_metrics(summarize = T) %>% 
  dplyr::filter(.metric %in% c('accuracy', 'recall', 'spec')) %>%
  dplyr::select(-c(.estimator, .config))

#CONFUSION MATRIX
rf_conf_mat = rf_last_fit %>% 
  tune::collect_predictions() %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Matriz de Confusão - Random Forest (.threshold = 0,5)") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/rf4_conf_mat.png", plot = rf_conf_mat, height = 5, width = 5.5)

#CURVA ROC
rf_roc_curve = rf_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  tune::autoplot() +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "AUC - Random Forest") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/rf4_roc_curve.png", plot = rf_roc_curve, height = 5, width = 5.5)

#DEFININDO PONTO DE CORTE OTIMO
rf_new_threshold = rf_last_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  dplyr::filter(specificity >= 0.5) %>% 
  dplyr::arrange(desc(sensitivity)) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::select(.threshold)

#RECALCULANDO AS PREDICOES COM O NOVO PONTO DE CORTE
rf_new_pred = rf_last_fit %>% 
  tune::collect_predictions() %>% 
  dplyr::mutate(.pred_class = probably::make_two_class_pred(.pred_1, levels(target), threshold = rf_new_threshold$.threshold))

#LISTANDO NOVAS METRICAS
rf_metrics = yardstick::metric_set(yardstick::accuracy,
                                   yardstick::recall, 
                                   yardstick::spec)

#CALCULANDO NOVAS METRICAS
rf_new_pred %>% 
  rf_metrics(truth = target, estimate = .pred_class) %>%
  dplyr::select(-.estimator)

#NOVA MATRIZ DE CONFUSAO
rf_new_conf_mat = rf_new_pred %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Matriz de Confusão - Random Forest (.threshold = 0,451)") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/rf4_new_conf_mat.png", plot = rf_new_conf_mat, height = 5, width = 5.5)


# LOGISTIC REGRESSION -----------------------------------------------------

#ESPECIFICANDO O MODELO
log_spec = parsnip::logistic_reg() %>% 
  parsnip::set_engine(engine = "glm") %>%
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
log_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(log_spec)

#SALVANDO RDATA DO WORKFLOW
save(log_workflow, file = "Data/log4_workflow.Rdata")

#GRID DE PARAMETROS
log_grid = expand.grid(mtry = c(0.1, 1, 5))

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#AJUSTE DO MODELO
log_fit = log_workflow %>% 
  tune::fit_resamples(resamples = cv, 
                      grid = log_grid,
                      metrics = yardstick::metric_set(yardstick::recall,
                                                      yardstick::spec,
                                                      yardstick::f_meas, 
                                                      yardstick::accuracy, 
                                                      yardstick::kap, 
                                                      yardstick::roc_auc),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(log_fit, file = "Data/log4_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O MODELO
load(file = "Data/log4_model.Rdata", verbose = T)

#COLETANDO METRICAS
log_fit %>% 
  tune::collect_metrics(summarize = T) %>% 
  dplyr::filter(.metric %in% c('accuracy', 'recall', 'spec'))

#MOSTRANDO O MELHOR MODELO NO TREINO
log_fit %>% tune::show_best(metric = "recall")

#SELECIONANDO O MODELO COM OS MELHORES PARAMETROS
log_best_param = log_fit %>% 
  tune::select_best(metric = "recall")

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/log4_workflow.Rdata", verbose = T)

#FINALIZANDO O workflow COM OS MELHORES PARAMETROS
log_best_workflow = log_workflow %>% 
  tune::finalize_workflow(log_best_param)

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#RANDOM FOREST 
log_last_fit = tune::last_fit(log_best_workflow, 
                              split = data_split,
                              metrics = yardstick::metric_set(yardstick::recall,
                                                              yardstick::spec,
                                                              yardstick::f_meas, 
                                                              yardstick::accuracy, 
                                                              yardstick::kap,
                                                              yardstick::roc_auc)
)

#SALVANDO OBJETO DO MODEL EVALUATION
save(log_last_fit, file = "Data/log4_last_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/log4_last_model.Rdata", verbose = T)

#COLETANDO METRICAS
log_last_fit %>% 
  tune::collect_metrics(summarize = T) %>%
  dplyr::filter(.metric %in% c('accuracy', 'recall', 'spec')) %>%
  dplyr::select(-c(.estimator, .config))

#CONFUSION MATRIX
log_conf_mat = log_last_fit %>% 
  tune::collect_predictions() %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Matriz de Confusão - Regrssão Logística (.threshold = 0,5)") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/log4_conf_mat.png", plot = log_conf_mat, height = 5, width = 5.5)

#CURVA ROC
log_roc_curve = log_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  tune::autoplot() +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "AUC - Regressão Logística") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/log4_roc_curve.png", plot = log_roc_curve, height = 5, width = 5.5)

#OTIMIZANDO O PONTO DE CORTE
log_new_threshold = log_last_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  dplyr::filter(specificity >= 0.5) %>% 
  dplyr::arrange(desc(sensitivity)) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::select(.threshold)

#RECALCULANDO AS PREDICOES COM O NOVO PONTO DE CORTE
log_new_pred = log_last_fit %>% 
  tune::collect_predictions() %>% 
  dplyr::mutate(.pred_class = probably::make_two_class_pred(.pred_1, levels(target), threshold = log_new_threshold$.threshold))

#LISTANDO NOVAS METRICAS
log_metrics = yardstick::metric_set(yardstick::accuracy,
                                    yardstick::recall, 
                                    yardstick::spec)

#CALCULANDO NOVAS METRICAS
log_new_pred %>% 
  log_metrics(truth = target, estimate = .pred_class) %>%
  dplyr::select(-.estimator)

#NOVA MATRIZ DE CONFUSAO
log_new_conf_mat = log_new_pred %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Matriz de Confusão - Regrssão Logística (.threshold = 0,478)") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/log4_new_conf_mat.png", plot = log_new_conf_mat, height = 5, width = 5.5)

# XGBOOST -----------------------------------------------

#ESPECIFICANDO O MODELO
xgb_spec = parsnip::boost_tree() %>%
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
xgb_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(xgb_spec)

#SALVANDO RDATA DO WORKFLOW
save(xgb_workflow, file = "Data/xgb4_workflow.Rdata")

#GRID DE PARAMETROS
xgb_grid = expand.grid(subsample = 0.5, 
                       colsample_bytree = 0.5,
                       max_depth = 3,
                       min_child = seq(1), 
                       eta = c(0.1)
)

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#AJUSTE DO MODELO
xgb_fit = xgb_workflow %>% 
  tune::tune_grid(resamples = cv, 
                  grid = xgb_grid,
                  metrics = yardstick::metric_set(yardstick::recall,
                                                  yardstick::spec,
                                                  yardstick::f_meas, 
                                                  yardstick::accuracy, 
                                                  yardstick::kap, 
                                                  yardstick::roc_auc),
                  control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(xgb_fit, file = "Data/xgb4_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O MODELO
load(file = "Data/xgb4_model.Rdata", verbose = T)

#COLETANDO METRICAS
xgb_fit %>% 
  tune::collect_metrics(summarize = T) %>% 
  dplyr::filter(.metric %in% c('accuracy', 'recall', 'spec'))

#MOSTRANDO O MELHOR MODELO NO TREINO
xgb_fit %>% tune::show_best(metric = "recall")

#SELECIONANDO O MODELO COM OS MELHORES PARAMETROS
xgb_best_param = xgb_fit %>% 
  tune::select_best(metric = "recall")

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/xgb4_workflow.Rdata", verbose = T)

#FINALIZANDO O workflow COM OS MELHORES PARAMETROS
xgb_best_workflow = xgb_workflow %>% 
  tune::finalize_workflow(xgb_best_param)

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#RANDOM FOREST 
xgb_last_fit = tune::last_fit(xgb_best_workflow, 
                              split = data_split,
                              metrics = yardstick::metric_set(yardstick::recall,
                                                              yardstick::spec,
                                                              yardstick::f_meas, 
                                                              yardstick::accuracy, 
                                                              yardstick::kap,
                                                              yardstick::roc_auc)
)

#SALVANDO OBJETO DO MODEL EVALUATION
save(xgb_last_fit, file = "Data/xgb4_last_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/xgb4_last_model.Rdata", verbose = T)

#COLETANDO METRICAS
xgb_last_fit %>% 
  tune::collect_metrics(summarize = T) %>% 
  dplyr::filter(.metric %in% c('accuracy', 'recall', 'spec'))

#CONFUSION MATRIX
xgb_conf_mat = xgb_last_fit %>% 
  tune::collect_predictions() %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Matriz de Confusão - Xgboost (.threshold = 0,5)") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/xgb4_conf_mat.png", plot = xgb_conf_mat, height = 5, width = 5.5)

#CURVA ROC
xgb_roc_curve = xgb_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  tune::autoplot() +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "AUC - Xgboost") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/xgb4_roc_curve.png", plot = xgb_roc_curve, height = 5, width = 5.5)

#OTIMIZANDO O PONTO DE CORTE
xgb_new_threshold = xgb_last_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  dplyr::filter(specificity >= 0.5) %>% 
  dplyr::arrange(desc(sensitivity)) %>%
  dplyr::filter(dplyr::row_number() == 1)

#RECALCULANDO AS PREDICOES COM O NOVO PONTO DE CORTE
xgb_new_pred = xgb_last_fit %>% 
  tune::collect_predictions() %>% 
  dplyr::mutate(.pred_class = probably::make_two_class_pred(.pred_1, levels(target), threshold = xgb_new_threshold$.threshold))

#LISTANDO NOVAS METRICAS
xgb_metrics = yardstick::metric_set(yardstick::accuracy,
                                    yardstick::recall, 
                                    yardstick::spec)

#CALCULANDO NOVAS METRICAS
xgb_new_pred %>% 
  xgb_metrics(truth = target, estimate = .pred_class) %>%
  dplyr::select(-.estimator)

#NOVA MATRIZ DE CONFUSAO
xgb_new_conf_mat = xgb_new_pred %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Matriz de Confusão - Xgboost (.threshold = 0,469)") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/xgb4_new_conf_mat.png", plot = xgb_new_conf_mat, height = 5, width = 5.5)


# COMPARANDO MODELOS ------------------------------------------------------

#MODEL PREDICTIONS
rf_compare = rf_fit %>% 
  tune::collect_predictions(summarise = TRUE) %>%
  dplyr::mutate(model = "Random Forest") %>%
  as.data.frame()

log_compare = log_fit %>% 
  tune::collect_predictions(summarise = TRUE) %>%
  dplyr::mutate(model = "Logistic Regression") %>%
  as.data.frame()

xgb_compare = xgb_fit %>% 
  tune::collect_predictions(summarise = TRUE) %>%
  dplyr::mutate(model = "XGBoost") %>%
  as.data.frame()

model_compare = dplyr::bind_rows(
  log_compare,
  rf_compare,
  xgb_compare,
) 

#GRAFICO AUC PARA COMPARACAO DE MODELOS
compare_roc_curve = model_compare %>%
  dplyr::filter(id2 == "Fold1") %>%
  dplyr::group_by(model) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  tune::autoplot() +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "AUC - Comparação de Modelos") + 
  ggeasy::easy_center_title()

#SALVANDO O PLOT
ggsave(filename = "plots/compare_roc_curve.png", plot = compare_roc_curve, height = 5, width = 5.5)