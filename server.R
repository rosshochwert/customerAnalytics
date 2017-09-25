library(shiny)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  source("churnModels.R", local=TRUE)
  source("countModels.R", local=TRUE)
  source("trialModels.R", local=TRUE)
  dataInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath)
    
  })
  
  countInput <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })

  trialInput <- reactive({
    inFile <- input$file3
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  output$trial_forecast <- renderPlot({
    if (!is.null(trialInput())){
      data = trialInput()
      size = input$sample
      nt = input$nt
      if(input$trial_model=='Exponential'){
        model.Exponential = optim(par=c(.5,.1),Exponential.Log_Likelihood,data=data,size=size,nt=nt,lower=c(0.00001,.000001),upper=c(.9999,.99999), method="L-BFGS-B")
        Exponential.graph(model.Exponential$par,data,size,nt=nt)
        output$trial_parameters <- renderUI({ 
          lambda = paste("lambda: ", round(model.Exponential$par[1],3))
          ll = paste("log-likelihood: ", round(model.Exponential$value,3))
          p = ""
          if(nt){
            p = paste("p: ", round(model.Exponential$par[2],3))
          }
          HTML(paste(lambda,p,ll,sep="<br/>"))
        })
      } else if (input$trial_model=='Exponential_Gamma'){
        model.Exponential.Gamma = optim(par=c(.5,.5,.1),Exponential.Gamma.Log_Likelihood,data=data,size=size,nt=nt,lower=c(0.00001,.000001,.000001),upper=c(Inf,Inf,.999999), method="L-BFGS-B")
        Exponential.Gamma.graph(model.Exponential.Gamma$par,data,size,nt=nt)
        output$trial_parameters <- renderUI({ 
          alpha = paste("alpha: ", round(model.Exponential.Gamma$par[1],3))
          r = paste("r: ", round(model.Exponential.Gamma$par[2],3))
          ll = paste("log-likelihood: ", round(model.Exponential.Gamma$value,3))
          p = ""
          if(nt){
            p = paste("p: ", round(model.Exponential.Gamma$par[3],3))
          }
          HTML(paste(alpha,r,p,ll,sep="<br/>"))
        })
      } else if (input$trial_model=='Weibull_Gamma'){
        model.Weibull.Gamma = optim(par=c(.5,.5,1,.1),Weibull.Gamma.Log_Likelihood,data=data,size=size,nt=nt,lower=c(0.00001,.000001,.00001,.00001),upper=c(Inf,Inf,Inf,.99999), method="L-BFGS-B")
        Weibull.Gamma.graph(model.Weibull.Gamma$par,data,size,nt=nt)
        output$trial_parameters <- renderUI({ 
          alpha = paste("alpha: ", round(model.Weibull.Gamma$par[1],3))
          r = paste("r: ", round(model.Weibull.Gamma$par[2],3))
          c = paste("c: ", round(model.Weibull.Gamma$par[3],3))
          ll = paste("log-likelihood: ", round(model.Weibull.Gamma$value,3))
          p = ""
          if(nt){
            p = paste("p: ", round(model.Weibull.Gamma$par[4],3))
          }
          HTML(paste(alpha,r,c,p,ll,sep="<br/>"))
        })
      }
    }
  })
  output$barplot <- renderPlot({
    if (!is.null(countInput())){
      data = countInput()
      if(input$count_model=='NBD'){
        model.NBD = optim(par=c(0.1,0.1),NBD.log_likelihod,data = data,lower=c(0.00001,0.000001), method="L-BFGS-B")
        NBD.histogram(model.NBD$par,data)
        output$count_parameters <- renderUI({ 
          r = paste("r: ", round(model.NBD$par[1],3))
          alpha = paste("alpha: ", round(model.NBD$par[2],3))
          ll = paste("log-likelihood:", round(model.NBD$value,3))
          HTML(paste(r,alpha,ll,sep="<br/>"))
        })
      } else if (input$count_model=='ZNBD'){
        model.ZNBD = optim(par=c(0.1,0.1,0.1),ZNBD.log_likelihod,data = data,lower=c(0.00001,0.000001,0.000001), method="L-BFGS-B")
        ZNBD.histogram(model.ZNBD$par,data)
        output$count_parameters <- renderUI({ 
          r = paste("r: ", round(model.ZNBD$par[1],3))
          alpha = paste("alpha: ", round(model.ZNBD$par[2],3))
          spike = paste("spike at 0: ", round(model.ZNBD$par[3],3))
          ll = paste("log-likelihood:", round(model.ZNBD$value,3))
          HTML(paste(r,alpha,spike,ll,sep="<br/>"))
        })
      }
    }
  })
  
  
  output$churnGraph <- renderPlot({
    if (!is.null(dataInput())){
      data = dataInput()
      if(input$model=='sBG'){
        model.sBG = optim(par=c(1,1),sBG.log.likelihood, data = data, lower=c(0.00001,0.000001), method="L-BFGS-B")
        sBG.graph(model.sBG$par, data = dataInput())
        output$parameters <- renderUI({ 
          alpha = paste("alpha: ", round(model.sBG$par[1],3))
          beta = paste("beta: ", round(model.sBG$par[2],3))
          ll = paste("log-likelihood:", round(model.sBG$value,3))
          HTML(paste(alpha,beta,ll,sep="<br/>"))
        })
        output$table <- renderTable({
          sBG.table(model.sBG$par, data = data)
        },include.rownames=FALSE)
      } else if (input$model=='BdW'){
        model.BdW = optim(par=c(1,1,1),BdW.log.likelihood,data = data, lower=c(0.00001,0.000001,0.0000001), method="L-BFGS-B")
        BdW.graph(model.BdW$par, data)
        output$parameters <- renderPrint({ 
          alpha = paste("alpha: ", round(model.BdW$par[1],3))
          beta = paste("beta: ", round(model.BdW$par[2],3))
          c = paste("c: ", round(model.BdW$par[3],3))
          ll = paste("log-likelihood:", round(model.BdW$value,3))
          HTML(paste(alpha,beta,c,ll,sep="<br/>"))
        })
        output$table <- renderTable({
          BdW.table(model.BdW$par, data = data)
        },include.rownames=FALSE)
      } else if (input$model=='dW'){
        model.dW = optim(par=c(.5,1),dW.log.likelihood,data = data, lower=c(0.00001,0.000001), upper=c(.9999), method="L-BFGS-B")
        dW.graph(model.dW$par, data = data)
        output$parameters <- renderPrint({ 
          theta = paste("theta: ", round(model.dW$par[1],3))
          c = paste("c: ", round(model.dW$par[2],3))
          ll = paste("log-likelihood:", round(model.dW$value,3))
          HTML(paste(theta,c,ll,sep="<br/>"))
        })
        output$table <- renderTable({
          dW.table(model.dW$par, data = data)
        },include.rownames=FALSE)
      }
    }

  })
})