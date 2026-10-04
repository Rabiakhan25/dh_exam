# Train the random forest and cache it to models/rf_model.rds.
#
# Usage (from the project root): Rscript scripts/train_model.R
# The app trains the model automatically on first launch if no cache exists;
# run this script to rebuild it after changing MODEL_FEATURES or MODEL_PARAMS.

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

health_data <- load_health_data()
model       <- train_model(health_data)

dir.create(dirname(MODEL_PATH), showWarnings = FALSE, recursive = TRUE)
saveRDS(model, MODEL_PATH)

print(model)
cat(sprintf("\nOOB accuracy: %.1f%%\nSaved to %s\n", 100 * model_accuracy(model), MODEL_PATH))
