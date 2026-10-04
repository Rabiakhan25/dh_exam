# Random forest classifier for diabetes status.

#' Fit a random forest on a class-balanced sample of the data.
train_model <- function(data, seed = SEED) {
  training <- balance_classes(data, seed) |>
    dplyr::select(dplyr::all_of(MODEL_FEATURES), diabetic_status)

  set.seed(seed)
  randomForest::randomForest(
    diabetic_status ~ .,
    data       = training,
    ntree      = MODEL_PARAMS$ntree,
    mtry       = MODEL_PARAMS$mtry,
    importance = TRUE
  )
}

#' Load the cached model from disk, training and caching it on first use.
load_or_train_model <- function(data, path = MODEL_PATH) {
  if (file.exists(path)) {
    # Registers randomForest's predict() method for the deserialised object.
    loadNamespace("randomForest")
    return(readRDS(path))
  }
  message("No cached model found; training a new one (this takes a moment)...")
  model <- train_model(data)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(model, path)
  model
}

#' Out-of-bag accuracy of a fitted random forest.
model_accuracy <- function(model) {
  1 - model$err.rate[model$ntree, "OOB"]
}

#' Predict diabetes status for a single respondent.
#'
#' @param profile Named list with the entries in MODEL_FEATURES.
#' @return List with the predicted `class` and a data frame of `probabilities`.
predict_status <- function(model, profile) {
  newdata <- as.data.frame(lapply(profile[MODEL_FEATURES], as.numeric))
  probs   <- predict(model, newdata, type = "prob")[1, STATUS_LEVELS]

  list(
    class         = STATUS_LEVELS[which.max(probs)],
    probabilities = data.frame(
      status      = factor(STATUS_LEVELS, levels = STATUS_LEVELS),
      probability = unname(probs)
    )
  )
}
