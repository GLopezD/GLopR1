#' GLopR1 package
#'
#' @description Given a vector dataset, the GLopR1 package creates its histogram, boxplot,
#' Q-Q plot, and descriptive statistics summary.
#' @export
#' @param  x numeric variable
#' @examples
#' x=runif(100,1,2) # Creating a vector dataset.
#' f1(x)            # Apply the function to create histogram, boxplot,
#'                  # Q-Q plot and descriptive statistics summary of the dataset.
#' @importFrom stats qnorm
#' @references 1. Wickham, Hadley (2015). R Packages. O'Reilly Media, Inc. ISBN: 9781491910597.
#' @references 2. Agresti, Alan. Statistical Methods for the Social Sciences 5th Edition. Pearson, ISBN-13:978-0134507101.
f1<- function(x){

  par(mfrow = c(1,3))  # Setting the 1 row and 3 columns in the plot window

  histogram =hist(x, main="Histogram", xlab = "Data", ylab="Frequency",
                  col=rainbow(20))      # Plotting a histogram

  boxplot = boxplot(x, main="Boxplot", col='blue')  # plot a Boxplot

  # Calculating whether the data comes from a normal population manually

  a=as.matrix(x,length(as.matrix(x)),1)
  a1=matrix(sort(a),length(a),1)
  m=matrix(0,length(a),2)
  for(i in 1:length(a)){
    m[i,2]=a1[i,1]
    m[i,1]=qnorm((i-0.5)/length(a),lower.tail=TRUE)
  }

  plot1= plot(m[,1],m[,2],xlab="Theoretical Quantiles",
              ylab="Sorted Data",main="Normal Q-Q Plot")
  plot2= qqline(a[,1])

  # Creating a descriptive statistics summary of the data

  r=3  # decimal round
  descrip_stat = data.frame(min=round(min(x),r),
                            median=round(median(x),r),
                            max=round(max(x),r),
                            mean=round(mean(x),r),
                            sd=round(sd(x),r))
  return(list(histogram, boxplot, plot1, plot2, descrip_stat))
  }

