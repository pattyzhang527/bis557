#transfer the ridge_train and ridge_test csv data file into .rda data file
ridge_train<-read.csv("ridge_train.csv")
save(ridge_train,file = "../data/ridge_train.rda")

ridge_test<-read.csv("ridge_test.csv")
save(ridge_test,file = "../data/ridge_test.rda")
