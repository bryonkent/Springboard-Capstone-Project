master <- Model

row <- nrow(master)
new <- c('Age','Education2003Revision','Race','MaritalStatus','Sex','Cases','Cases2')
new <- master[1,]

Endpoint <- "Endpoint"
Endpoint <- as.data.frame(Endpoint)
colnames(Endpoint) <- c('Endpoint')
new <- cbind(new,Endpoint)
master <- cbind(master,Endpoint)

colnames(new) <- c('Age','Education2003Revision','Race','MaritalStatus','Sex','Cases','Cases2','Endpoint')
na.omit(new)

master <- as.data.frame(master)
master[,8] <- sapply(master[,8], as.integer)
new <- master[1,]

counter = 0
for (i in 1:row){
  
  Cases <- master[i,6]
  Cases2  <- master[i,7]
  print(c("Cases", Cases,"Cases2", Cases2))
  for (p in 1:Cases){ 
    counter = counter +1
    master[i,8] <- 0
    new <- rbind(new,master[i,])
    new[counter,8] <- 1
  }
  
  for (t in 1:Cases2){ 
    counter = counter +1
    master[i,8] <- 0
    new <- rbind(new,master[i,])
    new[counter,8] <- 0
  }
  
}