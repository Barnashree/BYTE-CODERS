library(shinybusy)
library(shinycssloaders)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  output$corplot<-renderPlot({
    data<-read.csv("total.csv",header = T)
    data = model.matrix(~., data=data)
    data = data[,-1]
    
    library(corrplot)
    data1<-data.frame(data)
    library(dplyr)
    library(tidyverse)
    
    #data1<-dplyr::select(data1,input$cor)
    #data1<-dplyr::select(data1,get({{input$cor}}))
    #colSym <- rlang::sym(input$cor)
    #data1<-dplyr::select(data1,!!colSym)
    data<-dplyr::select(data1,subjectPortuguese)
    for(i in input$cor){
      cat(i)
      data<-cbind.data.frame(data,dplyr::select(data1,starts_with(i)))
    }
    data1<-data
    
    M = cor(data1)
    
    if(input$pmethod=="corrplot_number"){
      corrplot(M, method = 'number')  
    }
    if(input$pmethod=="corrplot_color_alphabet"){
      corrplot(M, method = 'color', order = 'alphabet')  
    }
    if(input$pmethod=="corrplot_default"){
      corrplot(M)  
    }
    if(input$pmethod=="corrplot_AOE"){
      corrplot(M, order = 'AOE')  
    }
    if(input$pmethod=="corrplot_shade_AOE"){
      corrplot(M, method = 'shade', order = 'AOE', diag = FALSE)  
    }
    if(input$pmethod=="corrplot_square_FPC_lower"){
      corrplot(M, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)  
    }
    if(input$pmethod=="corrplot_ellipse_AOE_upper"){
      corrplot(M, method = 'ellipse', order = 'AOE', type = 'upper')  
    }
    if(input$pmethod=="corrplot_mixed_AOE"){
      corrplot.mixed(M, order = 'AOE')  
    }
    if(input$pmethod=="corrplot_mixed_shade_pie_hclust"){
      corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust')  
    }
    if(input$pmethod=="Heat map"){
      heatmap(M) 
    }
    
    output$cormat<-renderTable({
      
      as.data.frame(as.table(M))
    })
    
    
    
    
    
    
    
    
    
  })
  
  output$mse <- renderText({
    show_spinner()
    library(ggplot2)
    library(dplyr)
    library(MASS)
    library(glmnet)
    
    
        
    
    
    
    
    
    d1=read.table(input$subject,sep=";",header=TRUE)
    
    #########***#########   DATA PREPROCESSING   #########***#########
    
    ### create response variable
    if(input$grade=="G1"){
      y = d1$G1  
    }
    if(input$grade=="G2"){
      y = d1$G2  
    }
    if(input$grade=="G3"){
      y = d1$G3  
    }
    
    
    n = length(y)
    
    ### create model design matrix (without intercept)
    X = model.matrix(~., data=d1[,1:30])
    X = X[,-1]
    
    ### check size of X
    output$dim <- renderPrint({
      dim(X)
    })
    
    
    
    #########***#########   OLS REGRESSION   #########***#########
    
    ### fit linear regression model via OLS
    olsmod = lm(y ~ ., data=data.frame(X))
    
    ### summarize model results
    olsmod.sum = summary(olsmod)
    olsmod.sum
    
    ### extract model coefficients
    olscoef = coef(olsmod)
    olscoef
    
    #########***#########   P = 0.05 SELECTION   #########***#########
    
    ### find significant coefficients at 0.05 level
    ix = which(olsmod.sum$coefficients[-1,4] < 0.05)
    
    ### refit model with significant coefficients
    pvalmod.05 = lm(y ~ ., data=data.frame(X[,ix]))
    
    ### extract model coefficients
    pvalcoef.05 = as(matrix(0, length(olscoef), 1), "dgCMatrix")
    ix = match(names(coef(pvalmod.05)), names(olscoef))
    pvalcoef.05[ix] = coef(pvalmod.05)
    rownames(pvalcoef.05) = names(olscoef)
    
    
    #########***#########   P = 0.15 SELECTION   #########***#########
    
    ### find significant coefficients at 0.15 level
    ix = which(olsmod.sum$coefficients[-1,4] < 0.15)
    
    ### refit model with significant coefficients
    pvalmod.15 = lm(y ~ ., data=data.frame(X[,ix]))
    
    ### extract model coefficients
    pvalcoef.15 = as(matrix(0, length(olscoef), 1), "dgCMatrix")
    ix = match(names(coef(pvalmod.15)), names(olscoef))
    pvalcoef.15[ix] = coef(pvalmod.15)
    rownames(pvalcoef.15) = names(olscoef)
    
    
    #########***#########   STEPWISE REGRESSION (AIC)   #########***#########
    
    ### AIC selection and direction="both" by default
    stepmod.aic = step(olsmod, trace=0)
    
    ### get stepwise coefficients
    stepcoef.aic = as(matrix(0, length(olscoef), 1), "dgCMatrix")
    ix = match(names(coef(stepmod.aic)), names(olscoef))
    stepcoef.aic[ix] = coef(stepmod.aic)
    rownames(stepcoef.aic) = names(olscoef)
    
    
    #########***#########   STEPWISE REGRESSION (BIC)   #########***#########
    
    ### BIC selection (and direction="both" by default)
    stepmod.bic = step(olsmod, trace=0, k=log(n))
    
    ### get stepwise coefficients
    stepcoef.bic = as(matrix(0, length(olscoef), 1), "dgCMatrix")
    ix = match(names(coef(stepmod.bic)), names(olscoef))
    stepcoef.bic[ix] = coef(stepmod.bic)
    rownames(stepcoef.bic) = names(olscoef)
    
    
    #########***#########   RIDGE REGRESSION   #########***#########
    
    ### fit ridge regression model using GCV to select lambda
    lamseq = seq(0,300,length=1000)
    ridgemod = lm.ridge(y ~ ., data=data.frame(X), lambda=lamseq)
    
    ### plot the ridge trace (to ensure we found minimum)
    #quartz(width=8,height=4)
    output$plot1<-renderPlot({
    plot(ridgemod$lambda, ridgemod$GCV, xlab="Lambda", ylab="GCV")
    lines(rep(lamseq[which.min(ridgemod$GCV)],2), range(ridgemod$GCV), lty=3)
    })
    #dev.copy2pdf(file="~/Desktop/psych-penreg/ridge-gcv.pdf")
    
    ### find lambda that minimizes GCV
    gcvmin = which.min(ridgemod$GCV)
    
    ### extract model coefficients
    ridgecoef.min = coef(ridgemod)[gcvmin,]
    
    
    #########***#########   LASSO REGRESSION   #########***#########
    
    ### create fold assignments for 10-fold CV
    set.seed(1)
    foldid = sample(rep(1:10, length.out=n))
    
    ### 10-fold CV to estimate lambda
    cvlasso = cv.glmnet(X, y, foldid=foldid, alpha=1)
    
    ### plot results
    #quartz(width=8,height=4)
    output$plot2<-renderPlot({
    plot(cvlasso)
    })
    #dev.copy2pdf(file="~/Desktop/psych-penreg/lasso-mse.pdf")
    
    ### get the coefficients
    lassocoef.min = coef(cvlasso, s="lambda.min")
    lassocoef.1se = coef(cvlasso, s="lambda.1se")
    
    
    #########***#########   ELASTIC NET REGRESSION   #########***#########
    
    ### create fold assignments for 10-fold CV
    set.seed(1)
    foldid = sample(rep(1:10, length.out=n))
    
    ### 10-fold CV to estimate lambda and alpha
    alphaseq = seq(0,1,length=21)
    cvlist = vector("list",length(alphaseq))
    for(k in 1:length(alphaseq)){
      cvlist[[k]] = cv.glmnet(X, y, foldid=foldid, alpha=alphaseq[k])
    }
    
    ### plot alphaseq vs CV-MSE
    #quartz(width=8,height=8)
    par(mfrow=c(2,1))
    mincv = sapply(cvlist, function(x) min(x$cvm))
    output$plot3<-renderPlot({
    plot(alphaseq, mincv, xlab="Alpha", ylab="Mean-Squared Error", type="b")
    })
    ### get the minimum
    minid = which.min(mincv)
    minid
    alphaseq[minid]
    
    ### plot results for minimum
    output$plot4<-renderPlot({
    plot(cvlist[[minid]])
    })
    #dev.copy2pdf(file="~/Desktop/psych-penreg/enet-mse.pdf")
    
    ### get the coefficients
    enetcoef.min = coef(cvlist[[minid]], s="lambda.min")
    enetcoef.1se = coef(cvlist[[minid]], s="lambda.1se")
    
    
    #########***#########   TABLE OF COEFFICIENTS   #########***#########
    
    ### unpenalized coefficients (table 3)
    utab = round(cbind(olscoef,pvalcoef.05,pvalcoef.15,stepcoef.aic,stepcoef.bic),3)
    colnames(utab) = c("ols","p0.05","p0.15","step.aic","step.bic")
    output$utab <- renderPrint({
    utab
    })
    ### penalized coefficients (table 4)
    ptab = round(cbind(ridgecoef.min,lassocoef.min,lassocoef.1se,enetcoef.min,enetcoef.1se),3)
    colnames(ptab) = c("ridgecoef","lasso.min","lasso.1se","enet.min","enet.1se")
    output$ptab <- renderPrint({
    ptab
    })
    
    
    #########***#########   MSPE SIMULATION   #########***#########
    show_spinner()
    nrep = input$rep
    methods = factor(c("ols","p0.05","p0.15","step.aic","step.bic",
                       "ridge","lasso.min","lasso.1se","enet.min","enet.1se"),
                     ordered=TRUE, levels=c("ols","p0.05","p0.15","step.aic","step.bic",
                                            "ridge","lasso.min","lasso.1se","enet.min","enet.1se"))
    
    lamseq = seq(0, 300, length=1000)
    alphaseq = seq(0, 1, length=21)
    
    msetab<-NULL
    
    set.seed(55455)
    for(i in 1:nrep){
      
      # print progress
      cat("rep:",i,"\n")
      
      # create training and testing data
      testID = sample.int(n, n*(1-input$train/100)) # 80 X 20 Training testing
      ytest = y[testID]
      Xtest = X[testID,]
      
      ytrain = y[-testID]
      Xtrain = X[-testID,]
      
      # ols regression
      if(input$method=="ols"){
        olsmod = lm(ytrain ~ ., data=data.frame(Xtrain))
        output$coefficient <- renderPrint({
          olsmod
        })
        msetab =c(msetab, mean( (ytest - cbind(1,Xtest) %*% coef(olsmod))^2 )  )
      }
      
      
      # p = 0.05
      if(input$method=="p0.05"){
      olsmod.sum = summary(olsmod)
      ix = which(olsmod.sum$coefficients[-1,4] < 0.05)
      p05mod = lm(ytrain ~ ., data=data.frame(Xtrain[,ix]))
      output$coefficient <- renderPrint({
        p05mod
      })
      msetab = c(msetab,mean( (ytest - cbind(1,Xtest[,ix]) %*% coef(p05mod))^2 ))
      }
      
      
      # p = 0.15
      if(input$method=="p0.15"){
      ix = which(olsmod.sum$coefficients[-1,4] < 0.15)
      p15mod = lm(ytrain ~ ., data=data.frame(Xtrain[,ix]))
      output$coefficient <- renderPrint({
        p15mod
      })
      msetab =c(msetab, mean( (ytest - cbind(1,Xtest[,ix]) %*% coef(p15mod))^2 ))
      }
      
      # stepwise regression (aic)
      if(input$method=="step.aic"){
      stepmod = step(olsmod, trace=0)
      output$coefficient <- renderPrint({
        stepmod
      })
      ix = match(names(stepmod$coefficients),names(olsmod$coefficients))
      msetab =c(msetab, mean( (ytest - cbind(1,Xtest)[,ix] %*% coef(stepmod))^2 ))
      }
      
      
      # stepwise regression (bic)
      if(input$method=="step.bic"){
      stepmod = step(olsmod, trace=0, k=log(length(ytrain)))
      output$coefficient <- renderPrint({
        stepmod
      })
      ix = match(names(stepmod$coefficients),names(olsmod$coefficients))
      msetab =c(msetab, mean( (ytest - cbind(1,Xtest)[,ix] %*% coef(stepmod))^2 ))
      }
      
      
      # ridge regression
      if(input$method=="ridge"){
      ridgemod = lm.ridge(ytrain ~ ., data=data.frame(Xtrain), lambda=lamseq)
      output$coefficient <- renderPrint({
        ridgemod
      })
      gcvmin = which.min(ridgemod$GCV)
      msetab =c(msetab, mean( (ytest - cbind(1,Xtest) %*% coef(ridgemod)[gcvmin,])^2 ))
      }
      
      # get folds for lasso and elastic net
      
      foldid = sample(rep(1:10, length.out=length(ytrain)))
      
      # lasso regression
      cvlasso = cv.glmnet(Xtrain, ytrain, foldid=foldid, alpha=1)
      if(input$method=="lasso.min"){
        output$coefficient <- renderPrint({
          cvlasso
        })
      msetab =c(msetab, mean( (ytest - cbind(1,Xtest) %*% coef(cvlasso, s="lambda.min"))^2 ))
      }
      
      if(input$method=="lasso.1se"){
        output$coefficient <- renderPrint({
          cvlasso
        })
      msetab = c(msetab, mean( (ytest - cbind(1,Xtest) %*% coef(cvlasso, s="lambda.1se"))^2 ))
      }
      
      # elastic net regression
      cvlist = vector("list",length(alphaseq))
      for(k in 1:length(alphaseq)){
        cvlist[[k]] = cv.glmnet(Xtrain, ytrain, foldid=foldid, alpha=alphaseq[k])
      }
      minid = which.min(sapply(cvlist, function(x) min(x$cvm)))
      if(input$method=="enet.min"){
        output$coefficient <- renderPrint({
          cvlist[[minid]]
        })
      msetab = c(msetab, mean( (ytest - cbind(1,Xtest) %*% coef(cvlist[[minid]], s="lambda.min"))^2 ))
      }
      if(input$method=="enet.1se"){
        output$coefficient <- renderPrint({
          cvlist[[minid]]
        })
      msetab =c(msetab, mean( (ytest - cbind(1,Xtest) %*% coef(cvlist[[minid]], s="lambda.1se"))^2 ))
      }
      
    }
    data<-msetab
    data<-data.frame(data)
    
    output$mseplot<-renderPlot({
      fun_mean <- function(x){
        return(data.frame(y=mean(x),label=round(mean(x,na.rm=T),2)))}
      fun_mean1 <- function(x){
        return(data.frame(y=mean(x),label=paste0("\u00B1",round((1.96*sd(x,na.rm=T))/sqrt(length(x)),2))))}
      
      ggplot(data,aes(data, fill="red"))+xlab("Mean-squared error")+geom_boxplot()+ggtitle("Box plots of mean-squared prediction error across random splits of the data")
      
    })
    
    output$mses <- renderPrint({
      msetab
    })
    
    output$msesd <- renderText({
      sd(msetab)
    })
    
    
    
    
    output$plotmath<-renderPlot({
    data<-read.csv("math_results.csv",header = T)
    
    fun_mean <- function(x){
      return(data.frame(y=mean(x),label=round(mean(x,na.rm=T),2)))}
    fun_mean1 <- function(x){
      return(data.frame(y=mean(x),label=paste0("\u00B1",round((1.96*sd(x,na.rm=T))/sqrt(length(x)),2))))}
    
    ggplot(data,aes(method, results,fill=method))+geom_boxplot()+xlab("Methods")+
      ylab("Mean-Squared Error")+ggtitle("Box plots of mean-squared prediction error across 100 random splits of the data")+
      stat_summary(fun.y=mean, colour="red", geom="point",shape=18, size=2,show_guide = FALSE) +
      stat_summary(fun.data = fun_mean, geom="text", col="black",vjust=-4.0)+
      stat_summary(fun.data = fun_mean1, geom="text", col="black",vjust=-3.0)+coord_cartesian(ylim = c(6, 25))+
      scale_fill_brewer(palette="RdYlBu")+
      scale_color_brewer(palette="RdYlBu")+
      ##geom_text(data = means, aes(label = round(eer,4), y = eer + 0.08))+
      theme(legend.background = element_rect(fill = "white"),legend.position="bottom",panel.background = element_rect(fill = "white", colour = "grey50"), axis.ticks.x = element_blank())
    })
    
    
    
    output$plotpor<-renderPlot({
      data<-read.csv("por_results.csv",header = T)
      
      fun_mean <- function(x){
        return(data.frame(y=mean(x),label=round(mean(x,na.rm=T),2)))}
      fun_mean1 <- function(x){
        return(data.frame(y=mean(x),label=paste0("\u00B1",round((1.96*sd(x,na.rm=T))/sqrt(length(x)),2))))}
      
      ggplot(data,aes(method, results,fill=method))+geom_boxplot()+xlab("Methods")+
        ylab("Mean-Squared Error")+ggtitle("Box plots of mean-squared prediction error across 100 random splits of the data")+
        stat_summary(fun.y=mean, colour="red", geom="point",shape=18, size=2,show_guide = FALSE) +
        stat_summary(fun.data = fun_mean, geom="text", col="black",vjust=-4.0)+
        stat_summary(fun.data = fun_mean1, geom="text", col="black",vjust=-3.0)+coord_cartesian(ylim = c(6, 13))+
        scale_fill_brewer(palette="RdYlBu")+
        scale_color_brewer(palette="RdYlBu")+
        ##geom_text(data = means, aes(label = round(eer,4), y = eer + 0.08))+
        theme(legend.background = element_rect(fill = "white"),legend.position="bottom",panel.background = element_rect(fill = "white", colour = "grey50"), axis.ticks.x = element_blank())
    })
    
    
    output$dataset<-renderPlot({
    d1=read.table("student-mat.csv",sep=";",header=TRUE)
    d2=read.table("student-por.csv",sep=";",header=TRUE)
    
    d3=rbind.data.frame(d1,d2)
    sub<-c(rep("Math",nrow(d1)),rep("Por",nrow(d2)))
    
    data<-dplyr::mutate(data.frame(d3),subject=sub)
    ggplot(data,aes(age,fill=sex))+geom_bar()+facet_grid(school~subject)+
      scale_fill_brewer(palette="Dark2")+
      scale_color_brewer(palette="Dark2")+
      ##geom_text(data = means, aes(label = round(eer,4), y = eer + 0.08))+
      theme(legend.background = element_rect(fill = "white"),legend.position="bottom",panel.background = element_rect(fill = "white", colour = "grey50"), axis.ticks.x = element_blank())
    })
    
    hide_spinner()
    mean(msetab)  
  })
   
}