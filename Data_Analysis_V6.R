set.seed(7)

#Setting working directory
setwd("")

#Loading our built-in PCA/related functions
source("PCA_Functions2.R")

#Loading Data
X_raw <- read.csv("Data_v3.csv", header = TRUE)
X_raw <- X_raw[, 1:6]
X <- X_raw

#Converting dates from character to DateTime format
X$DateTime <- as.POSIXct(X$DateTime,format="%m/%d/%Y %H:%M", tz="EST")

#Imputing missing data
X$P1 <- na_interpolation(X$P1, option = "linear", maxgap = 100)
X$P2 <- na_interpolation(X$P2, option = "linear", maxgap = 100)
X$Net.Flow.Rate <- na_interpolation(X$Net.Flow.Rate, option = "linear", maxgap = 100)

#Appending the derivatives of all variables
X$P1.dot <- 0; X$P2.dot <- 0; X$Net.Flow.Rate.dot <- 0
X$P1.dot[2:nrow(X)] <- diff(X$P1); X$P2.dot[2:nrow(X)] <- diff(X$P2)
X$Net.Flow.Rate.dot[2:nrow(X)] <- diff(X$Net.Flow.Rate)
X <- X[2:nrow(X), ]

#Extracting a subsample of normal data to train a detection algorithm
X.Normal <- X[X$Fault == "",]
X.Normal <- X.Normal[,c("P1","P2","Net.Flow.Rate","P1.dot","P2.dot","Net.Flow.Rate.dot")]
ind <- sample(1:2, nrow(X.Normal), replace = TRUE, prob = c(0.9,0.1))
X.Normal.Train <- X.Normal[ind==1,]; #X.Normal.Test <- X.Normal[1500:1800,] 
X.Normal.Test <- X.Normal[sample(1:nrow(X.Normal),100),]

#Training a PCA-based detection model
Detection.Model <- PCA_training(X.Normal.Train)
b <- Detection.Model$b; Sig <- Detection.Model$Sig; a <- Detection.Model$a 
P <- Detection.Model$P; T2a <- Detection.Model$T2a; Sigma_a <- Detection.Model$Sigma_a
Qa <- Detection.Model$Qa

#Isolating faulty samples
X.Faulty.raw <- X[X$Fault != "",]
X.Faulty <- X.Faulty.raw[,c("P1","P2","Net.Flow.Rate","P1.dot","P2.dot","Net.Flow.Rate.dot",
                            "Fault","Severity")]

#Separating types of faults
type.of.fault <- unique(X.Faulty.raw$Fault)

X.Faulty.1 <- X.Faulty[X.Faulty.raw$Fault == type.of.fault[1],]
X.Faulty.2 <- X.Faulty[X.Faulty.raw$Fault == type.of.fault[2],]
X.Faulty.3 <- X.Faulty[X.Faulty.raw$Fault == type.of.fault[3],]
X.Faulty.4 <- X.Faulty[X.Faulty.raw$Fault == type.of.fault[4],]

X.Faulty.1.test <- X.Faulty.1[sample(1:nrow(X.Faulty.1), 
                                     round(0.5*nrow(X.Faulty.1), digits=0)),]
X.Faulty.4.test <- X.Faulty.4[sample(1:nrow(X.Faulty.4), 
                                     round(0.5*nrow(X.Faulty.4), digits=0)),]
X.Faulty.2.test <- X.Faulty.2; X.Faulty.3.test <- X.Faulty.3
X.Faulty.1.train <- X.Faulty.1[sample(1:nrow(X.Faulty.1), 
                                      round(0.5*nrow(X.Faulty.1), digits=0)),]
X.Faulty.4.train <- X.Faulty.4[sample(1:nrow(X.Faulty.4), 
                                      round(0.5*nrow(X.Faulty.4), digits=0)),]
X.Faulty.2.train <- X.Faulty.2; X.Faulty.3.train <- X.Faulty.3

X.Faulty.test <- rbind(X.Faulty.1.test,X.Faulty.2.test,X.Faulty.3.test,
                       X.Faulty.4.test)
X.Faulty.train <- rbind(X.Faulty.1.train,X.Faulty.2.train,X.Faulty.3.train,
                        X.Faulty.4.train)

#Assessing FAR with normal data
outputs.normal <-  PCA_detection(b, Sig, a, P, T2a, Sigma_a, X.Normal.Test[,1:6], Qa)
T2.normal <- outputs.normal$T2; Threshold.normal <- outputs.normal$Threshold
Q.normal <- outputs.normal$Q; Threshold_Q.normal <- outputs.normal$Threshold_Q
FAR <- 100*sum(T2.normal>=Threshold.normal | Q.normal>=Threshold_Q.normal)/nrow(X.Normal.Test)

#Attempting to detect the faults
outputs.1 <- PCA_detection(b, Sig, a, P, T2a, Sigma_a, X.Faulty.1.test[,1:6], Qa)
T2.1 <- outputs.1$T2; Threshold.1 <- outputs.1$Threshold
Q.1 <- outputs.1$Q; Threshold_Q.1 <- outputs.1$Threshold_Q

outputs.2 <- PCA_detection(b, Sig, a, P, T2a, Sigma_a, X.Faulty.2.test[,1:6], Qa)
T2.2 <- outputs.2$T2; Threshold.2 <- outputs.2$Threshold
Q.2 <- outputs.2$Q; Threshold_Q.2 <- outputs.2$Threshold_Q

outputs.3 <- PCA_detection(b, Sig, a, P, T2a, Sigma_a, X.Faulty.3.test[,1:6], Qa)
T2.3 <- outputs.3$T2; Threshold.3 <- outputs.3$Threshold
Q.3 <- outputs.3$Q; Threshold_Q.3 <- outputs.3$Threshold_Q

outputs.4 <- PCA_detection(b, Sig, a, P, T2a, Sigma_a, X.Faulty.4.test[,1:6], Qa)
T2.4 <- outputs.4$T2; Threshold.4 <- outputs.4$Threshold
Q.4 <- outputs.4$Q; Threshold_Q.4 <- outputs.4$Threshold_Q

DR.1 <- 100*sum(T2.1>=Threshold.1 | Q.1>=Threshold_Q.1)/nrow(X.Faulty.1.test)
DR.2 <- 100*sum(T2.2>=Threshold.2 | Q.2>=Threshold_Q.2)/nrow(X.Faulty.2.test)
DR.3 <- 100*sum(T2.3>=Threshold.3 | Q.3>=Threshold_Q.3)/nrow(X.Faulty.3.test)
DR.4 <- 100*sum(T2.4>=Threshold.4 | Q.4>=Threshold_Q.4)/nrow(X.Faulty.4.test)

T2 <- c(T2.normal,T2.1,T2.2,T2.3,T2.4)
Threshold = c(Threshold.normal,Threshold.1,Threshold.2,Threshold.3,Threshold.4)
Q <- c(Q.normal, Q.1, Q.2, Q.3, Q.4)
Threshold_Q <- c(Threshold_Q.normal,Threshold_Q.1,Threshold_Q.2,Threshold_Q.3,Threshold_Q.4)

n.normal <- nrow(X.Normal.Test); n.F1 <- nrow(X.Faulty.1.test)
n.F2 <- nrow(X.Faulty.2.test); n.F3 <- nrow(X.Faulty.3.test)
n.F4 <- nrow(X.Faulty.4.test)

plot(T2, xlab = "Sample", ylab = "T2", col = "blue")
lines(Threshold, col = "red")
legend("topleft", legend=c("T^2", "Threshold"), col=c("blue","red"), lty = 1:1, cex=0.8, 
       bty = "n")
abline(v = c(n.normal,(n.normal+n.F1),(n.normal+n.F1+n.F2),
             (n.normal+n.F1+n.F2+n.F3),(n.normal+n.F1+n.F2+n.F3+n.F4)), 
       col = "Magenta", lty = 2)

plot(Q, xlab = "Sample", ylab = "Q", col = "blue")
lines(Threshold_Q, col = "red")
legend("topleft", legend=c("Q", "Threshold"), col=c("blue","red"), lty = 1:1, cex=0.8, 
       bty = "n")
abline(v = c(n.normal,(n.normal+n.F1),(n.normal+n.F1+n.F2),
             (n.normal+n.F1+n.F2+n.F3),(n.normal+n.F1+n.F2+n.F3+n.F4)), 
       col = "Magenta", lty = 2)

#Diagnose type of fault with Random Forest
library("randomForest")
RF.mod <- randomForest(x=X.Faulty.train[,1:6], y=as.factor(X.Faulty.train$Fault), ntree=500)
Preds <- predict(RF.mod, newdata = X.Faulty.test)
Diagnosis.error <- 100*sum(X.Faulty.test$Fault!=as.character(Preds))/length(Preds)

#Summary of performance metrics
FAR
c(DR.1, DR.2, DR.3, DR.4)
Diagnosis.error
