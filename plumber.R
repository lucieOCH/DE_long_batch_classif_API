if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("xgboost", quietly = TRUE) || packageVersion("xgboost") != "1.7.11.1") {
  devtools::install_version("xgboost", version = "1.7.11.1", repos = "https://cran.r-project.org")
}

library(plumber)
library(xgboost)
library(dplyr)


# Load the saved model
model <- readRDS("model.rds")

#* @post /predict
#* @serializer json
function(req, res) {
  input <- data.frame(jsonlite::fromJSON(req$postBody))
  
  # Debug logging — shows in Render logs
  print("---- Incoming data ----")
  print(input[c(1:10),])
  
  
  newdata = data.frame(
    "F9a.1_1"=input$Q1_1,
    "F9a.2_1"=input$Q1_2,
    "F8.1_1"=input$Q2_1,
    "F8.2_1"=input$Q2_2,
    "F8.3_1"=input$Q2_3,
    "F2.4_1"=input$Q3,
    "F2.6_1"=input$Q4,
    "F2.7_1"=input$Q5,
    "F2.8_1"=input$Q6,
    "F2.10_1"=input$Q7,
    "F5.1_1"=input$Q8,
    "F5.13_1"=input$Q9,
    "D1.1_1"=input$Q10
  )

  #newdata = as.data.frame(lapply(newdata,function(x) as.numeric(x)))
  
  fi_pred =  stats::predict(model, as.matrix(newdata))
  
  test_prediction  <- matrix(fi_pred, nrow = 7,
                             ncol=length(fi_pred)/7) %>%
    t() %>%
    data.frame() %>%
    dplyr::mutate(max_prob = max.col(., "last"))
  
  
  list(segments=(as.character(test_prediction$max_prob)))
}
