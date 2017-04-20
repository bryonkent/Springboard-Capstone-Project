library(kernlab)
data <- dfTraining
formula <- Endpoint ~ Age + Education2003Revision + 
  Race + MaritalStatus
m <- ksvm(
  formula, 
  data, 
  kernel = "tanhdot", 
  C = 100, 
  kpar = list(scale = 0.01, offset = -1.2)
)
