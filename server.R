# example from http://shiny.rstudio.com/gallery/kmeans-example.html

library(shiny)
library(rjson)
library(jsonlite)
source("fastmoments.R")


logreg <- setRefClass("logreg",
  fields = list(
    new_price = "numeric",
    old_price = "numeric",
    pct_change = "numeric",
    n = "numeric",
    pred = "numeric",
    times = "list",
    moments = "list",
    prices = "list",
    sample = "vector",
    w = "vector",
    eg = "vector",
    y = "numeric",
    alpha = "numeric",
    accuracy = "numeric",
    correct_pred = "numeric",
    nn = "numeric"
  ),

  methods = list(
    initialize = function() {
      new_price <<- 0.0
      update_price()
      n <<- 0
      nn <<- 0
      old_price <<- new_price * 1.0000000001 # starting value for calculations
      pct_change <<- 0.0

      moments[[1]] <<- new(moving_moments, 3)
      moments[[2]] <<- new(moving_moments, 5)
      moments[[3]] <<- new(moving_moments, 10)
      moments[[4]] <<- new(moving_moments, 20)
      moments[[5]] <<- new(moving_moments, 50)

      sample <<- rep(0, 21)
      eg <<- rep(0, 21)
      #w <<- runif(21)
      w <<- readRDS("model.rds")
      y <<- 0.0
      alpha <<- 0.01
      accuracy <<- 0.0
      correct_pred <<- 0.0
    },

    update_price = function() {
      #path <- "https://api.blockchain.com/v3/exchange/tickers/BTC-USD"
      r <- fromJSON("https://api3.binance.com/api/v3/ticker/price?symbol=BTCUSDT")
      new_price <<-  as.double(r[[2]])
      #return (price)
    },

    update_times = function() {
      times <<- append(times, as.character(format(Sys.time(),"%H:%M:%S")), 0)
      prices <<- append(prices, new_price, 0)
      if (length(times) > 100){
        prices[length(prices)] <<- NULL
        times[length(times)] <<- NULL
      }

    },

    fetch = function() {
      update_price()
      if (new_price == old_price) {
        return (FALSE)
      } else {
        update_times()
        #pct_change <<- pct_change + (new_price - old_price) / old_price
        pct_change <<- (new_price - old_price) / old_price
        update_training_vector()
        update_y()
        pred <<- sig(sample, w)[1]
        sagd()

        old_price <<- new_price
        get_accuracy()
        print(pred)
        save_model()
        return (TRUE)
      }
    },

    sig = function(X, W){
      return (1 / (1 + exp(-(W %*% X))))
    },

    sagd = function() {
      n <<- n + 1
     # eg <<- eg + ((sig(sample, w)[1] - y)*sample - eg) / n
      eg <<- 0.2 *(sig(sample, w)[1] - y)*sample + 0.8*eg
      w <<- w - alpha * eg
    },

    update_training_vector = function() {
      x <- vector()
      for (i in 1:length(moments)) {
        moments[[i]]$push(pct_change)
        x <- c(x, moments[[i]]$mean, moments[[i]]$variance, moments[[i]]$skewness, moments[[i]]$kurtosis)
      }
      sample <<- ifelse(is.nan(c(1, x)), 0,c(1, x)) #c(1, x)
    },

    update_y = function() {
      if (new_price > old_price) {
        y <<- 1.0
      } else {
        y <<- 0.0
      }
    },

    get_accuracy = function() {
      if (pred > 0.67) {
        nn <<- nn + 1
        print("BUY")

        if (y == 1) {
          correct_pred <<- correct_pred + 1
        }
        accuracy <<- correct_pred / nn
      }
      if (pred < 0.33) {
        nn <<- nn + 1
        print("SELL")

        if (y == 0) {
          correct_pred <<- correct_pred + 1
        }
        accuracy <<- correct_pred / nn
      }
    },

    save_model = function() {
      saveRDS(w, file = "model.rds")
    }

  )
)


HTML_moments <- setRefClass("HTML_moments",
    fields = list(
      old_ = "numeric"
    ),

    methods = list(
      initialize = function() {
        old_ <<- 0.0
      },
      update = function(value){
        renderText({
          if (value > old_){
            old_ <<- value
            return(paste(sprintf("<span style=\"color:green\">%s%%</span>", round(value*100, 3) )))
          } else {
            old_ <<- value
            return(paste(sprintf("<span style=\"color:red\">%s%%</span>", round(value*100, 3) )))
          }
        })
      }
    )
)

BUYORSELL <- setRefClass("BUYORSELL",
  fields = list(
    old_ = "numeric"
  ),

  methods = list(
    initialize = function() {
      old_ <<- 0.0
    },
    update = function(value){
      renderText({
        if (value > 0.67){
          old_ <<- value
          return(paste(sprintf("<span style=\"color:green\">%s</span>", "BUY" )))
        } else if (value < 0.33){
          old_ <<- value
          return(paste(sprintf("<span style=\"color:red\">%s</span>", "SELL" )))
        } else {
          return(paste(sprintf("<span style=\"color:black\">%s</span>", "Wait..." )))
        }
      })
    }
  )
)

shinyServer(function(input,output,session) {
  values <- reactiveValues(series=NULL,
                           x = 0.0,
                           tracker = logreg(),
                           accuracy = HTML_moments(),
                           prediction = BUYORSELL(),
                           reg = 0
  )

  observe({
    invalidateLater(600)

    isolate({
      if (values$tracker$fetch()) {
        values$x <- (unlist(values$tracker$prices))
      }
    })
  })

  output$scatterplot <- renderPlot({
    output$pred <- values$prediction$update(values$tracker$pred)
    output$accuracy <- values$accuracy$update(values$tracker$accuracy)

    plot(rev(values$x), col="black", xlab= "" , ylab= "", main="Last 100 Binance BTC/USD Prices", type ="o", xaxt='n')
    lines(predict(lm(rev(values$x)~c(1:length(values$x)))),col='RoyalBlue3')
    lines(predict(lm(rev(values$x)~log(c(1:length(values$x))))), col='HotPink4')

    axis(1, at = 1:length(values$tracker$times), labels = rev(values$tracker$times), cex.axis = .7)
  })
}
)
