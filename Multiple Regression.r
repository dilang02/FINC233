graphics.off()
# Question 1
salary <- function(GPA,IQ,Gender){
  result <- 50+20*GPA+0.07*IQ+35*Gender+0.01*GPA*IQ-10*GPA*Gender
}

cat("Salary: $",salary(4.0,110,1))

salaryF_results <- c()
salaryM_results <- c()
GPA_range <- c(1:4)
i <- 1
for (i in 1:4){
  salaryF_results <- append(salaryF_results,salary(i,110,1))
  salaryM_results <- append(salaryM_results,salary(i,110,0))
}
plot(GPA_range,salaryF_results,main="Question 1")
abline(lm(salaryM_results~GPA_range),col="blue")
abline(lm(salaryF_results~GPA_range),col="pink")

# Question 3
library(ISLR)
data(Auto)
plot(Auto,main="Question 3 Scatterplot Matrix")
cor_matrix <- cor(Auto[1:8])

lm_model <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
show(summary(lm_model))

lm_model_int <- lm(mpg~cylinders*displacement*horsepower*weight*acceleration*year*origin,data=Auto)
show(summary(lm_model_int))

Auto$weight2 <- Auto$weight * Auto$weight
lm_model_opt <- lm(mpg~weight+year+origin+weight2,data=Auto)
show(summary(lm_model_opt))

# Question 4
data(Carseats)
carseats_model <- lm(Sales~Price+Urban+US,data=Carseats)
show(summary(carseats_model))
carseats_model_optimal <- lm(Sales~Price+US,data=Carseats)
show(summary(carseats_model_optimal))
show(confint(carseats_model_optimal))

# Question 5
library(MASS)
data(Boston)
int_results <- matrix(NA,13,3)
beta_results <- matrix(NA,13,3)
colnames(int_results)<-c("int_est","t_stat_int_est","p_val_int_est")
colnames(beta_results)<-c("b_est","t_stat_b_est","p_val_b_est")
for (i in 2:14) {
  # Compute the regression results for each portfolios
  temp=summary(lm(crim~Boston[,i],data=Boston))
  # Save coefficient, t-statistic and p-value for alphas
  int_results[i-1,]<-c(temp$coefficients[1,1],
                        temp$coefficients[1,3],
                        temp$coefficients[1,4])
  # Save coefficient, t-statistic and p-values for betas
  beta_results[i-1,]<-c( temp$coefficients[2,1],
                        temp$coefficients[2,3],
                        temp$coefficients[2,4])
  #plot(Boston[,i],Boston[,1],main="Question 5",xlab=names(Boston[i]),ylab="crim")
}
rownames(int_results) <- names(Boston[2:14])
rownames(beta_results) <- names(Boston[2:14])

boston_lm <- lm(crim~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,data=Boston)
boston_lm_coef <- c()
for (i in 1:13){
  boston_lm_coef <- append(boston_lm_coef,summary(boston_lm)$coefficients[i+1,1])
}
plot(beta_results[,1],boston_lm_coef)

intp_results <- matrix(NA,12,3)
betap_results <- matrix(NA,12,3)
betap2_results <- matrix(NA,12,3)
betap3_results <- matrix(NA,12,3)
colnames(intp_results)<-c("int_est","t_stat_int_est","p_val_int_est")
colnames(betap_results)<-c("b_est","t_stat_b_est","p_val_b_est")
colnames(betap2_results)<-c("int_est","t_stat_int_est","p_val_int_est")
colnames(betap3_results)<-c("b_est","t_stat_b_est","p_val_b_est")
Boston_q <- Boston[,-4]
for (i in 2:13) {
  # Compute the regression results for each portfolios
  temp=summary(lm(crim~poly(Boston_q[,i],3,raw=TRUE),data=Boston_q))
  # Save coefficient, t-statistic and p-value for alphas
  intp_results[i-1,]<-c(temp$coefficients[1,1],
                       temp$coefficients[1,3],
                       temp$coefficients[1,4])
  # Save coefficient, t-statistic and p-values for betas
  betap_results[i-1,]<-c(temp$coefficients[2,1],
                         temp$coefficients[2,3],
                         temp$coefficients[2,4])
  
  betap2_results[i-1,]<-c(temp$coefficients[3,1],
                          temp$coefficients[3,3],
                          temp$coefficients[3,4])
  
  betap3_results[i-1,]<-c(temp$coefficients[4,1],
                          temp$coefficients[4,3],
                          temp$coefficients[4,4])

}
rownames(intp_results) <- names(Boston_q[2:13])
rownames(betap_results) <- names(Boston_q[2:13])
rownames(betap2_results) <- names(Boston_q[2:13])
rownames(betap3_results) <- names(Boston_q[2:13])
