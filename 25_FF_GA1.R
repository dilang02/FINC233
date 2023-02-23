###########################################################################
# This Code Computes CAPM regressions for the 2 by 25 size and            #
# book-to-market fama-french portfolios                                   #
#                                                                         #
# Author: Group 2                                                         #
###########################################################################
rm(list=ls())                                       # Clearing the workspace


# Loading monthly data for portfolio returns
load("25_FF_Portfolios.Rdata")

# Isolating the portfolio returns under consideration
port_ret<-data[,2:26]

# Isolating the vector of market excess returns
mkt_exc<-data[,27]

# Isolating the risk-free rate vector
rf<-data[,28]

# Constructing excess returns for the portfolios under consideration
port_exc_ret<-port_ret-rf

# Obtaining the number of observations for the market returns
n<-dim(port_exc_ret)[1]

# Conducting the OLS analysis on the first portfolio 
# (SMALL and Low Book-to-Market)
# Perform simple check using the lm function
results=summary(lm(port_exc_ret[,1]~mkt_exc))
results
plot(mkt_exc,port_exc_ret[,1])

# Computing the confidence interval for the first portfolio
conf_int=confint(lm(port_exc_ret[,1]~mkt_exc),level=0.95)

# Reporting the annualized alpha
print(results$coefficients[1,1]*12*100)
# Reporting the t-statistic
print(results$coefficients[1,3])
# Reporting the p-value
print(results$coefficients[1,4])
# Reporting the confidence Interval
print(conf_int[1,])

# This part of the code creates a loop to conduct estimates on the 6
# portfolios contained in the dataset all at once 
# We first define a matrix of missing values for the 
# quantities we will estimate. 
# Results for alpha 
alphas_results<-matrix(NA,dim(port_exc_ret)[2],3)
colnames(alphas_results)<-c("a_est","t_stat_a_est","p_val_a_est")

# Results for beta 
betas_results<-matrix(NA,dim(port_exc_ret)[2],3)
colnames(betas_results)<-c("b_est","t_stat_b_est","p_val_b_est")

# Confidence interval for alpha 
alpha_conf_int<-matrix(NA,dim(port_exc_ret)[2],2)

# We run the loop that, in each iteration, fills in the empty matrices we 
# just created
for (i in seq(1,dim(port_exc_ret)[2])) {
  # Compute the regression results for each portfolios
  temp=summary(lm(port_exc_ret[,i]~mkt_exc))
  # Save coefficient, t-statistic and p-value for alphas
  alphas_results[i,]<-c(temp$coefficients[1,1],
                        temp$coefficients[1,3],
                        temp$coefficients[1,4])
  # Save coefficient, t-statistic and p-values for betas
  betas_results[i,]<-c( temp$coefficients[2,1],
                        temp$coefficients[2,3],
                        temp$coefficients[2,4])
  # Compute confidence intervals for alphas
  alpha_conf_int[i,]=confint(lm(port_exc_ret[,i]~mkt_exc),level=0.95)[1,]
}

# Compute Annualized Alphas
alphas_results[,1]=alphas_results[,1]*12*100

# Round results to the third decimal point
alphas_results=round(alphas_results,3)
betas_results=round(betas_results,3)

# Generate a scatterplot for the relation between betas and mean 
# excess returns for the various portfolios
plot(betas_results[,1], colMeans(port_exc_ret)*12*100,type="p", 
     col="blue",main="Question 6",xlab="Beta", ylab="Average Returns",lwd = 4)
q6<-lm(colMeans(port_exc_ret)*12*100~betas_results[,1],data)
abline(q6,col="red")


# Adding row names
rownames(alphas_results)<-names(data[2:26])

# Exporting the results as a .csv file
write.csv(alphas_results,'alpha_results.csv')
           

# Adding row names
rownames(betas_results)<-names(data[2:26])

# Exporting the results as a .csv file
write.csv(betas_results,'betas_results.csv')








