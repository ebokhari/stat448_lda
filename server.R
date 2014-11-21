# server file
# app for visual demonstration of how the positive and negative predictive values 
# change across fixed sensitivity and specificity values when the base rate changes

library(ggplot2); library(grid)
library(MASS)

shinyServer(
     function(input, output) {    
          
          # AUC 
          AUC <- reactive({
               pnorm(input$mu2/2)
          })
          
          # true positive values
          tpr <- reactive({
               1 - pnorm(seq(-5, 5 + input$mu2, .01), input$mu2)
          })
          
          # false positive values
          fpr <- reactive({
               1 - pnorm(seq(-5, 5 + input$mu2, .01))
          })
          
          # random sample of points from ROC curve
          s <- reactive({
               sort(sample(seq(10, nrow(unique(round(cbind(tpr(), fpr()),2))) - 10, 7), 1), decreasing = T)
          })
          
          # data frame of sensitivity and specificity values
          dfR <- reactive({
               return(data.frame(tpr = tpr(), fpr = fpr()))     
          }) 
          
          # cutscore
          Xc <- reactive({
               .5*(input$mu2 - (log(10^(input$cost) * ((1-input$p1)/input$p1)))*(1/input$mu2))
          })
          
          # create plot of ROC curve
          output$rocPlot <- renderPlot({
               
               # plot ROC curve
               rocPlot = 
                    ggplot(dfR(), aes(x = fpr, y = tpr)) + geom_line(size = 2) +
                    geom_ribbon(aes(x = fpr, ymax = tpr), ymin = 0, alpha = .5) + 
                    scale_x_continuous(name = 'False Postive Rate (1 - Specificity)') +
                    scale_y_continuous(name = 'True Postive Rate (Sensitvity)') +
                    theme(axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17)) +
                    annotate('text', label = paste('AUC =', round(AUC(), 2)), x = .6, y = .25, 
                             size = 8, colour = 'white') +
                    geom_point(data = data.frame(x = seq(0, 1, .01), y = seq(0, 1, .01)), 
                               aes(x = x, y = y), type = 2, size = 1) +
                    geom_point(data = data.frame(tpr = 1 - pnorm(Xc(), input$mu2), 
                                                 fpr = 1 - pnorm(Xc())), aes(x = fpr, y = tpr), 
                               size = 5, color = 'red') +
                    ggtitle('ROC Curve') + 
                    theme(plot.title = element_text(lineheight = .8, face = 'bold', size = 25))
               
               print(rocPlot)
               
          })
          
          # create plot of signal and noise distributions
          output$distPlot <- renderPlot({ 
               
               
               env = environment()
               # data frame with densities
               dfN = data.frame(x = seq(-5, 5 + input$mu2, .01), pop1 = dnorm(seq(-5, 5 + input$mu2, .01)),
                                pop2 = dnorm(seq(-5, 5 + input$mu2, .01), input$mu2))
               dfP = data.frame(xc = Xc(), m2 = input$mu2)
               # plot densities
               distPlot = 
                    ggplot(dfN) + geom_line(aes(x = x, y = pop1), col = 'red', size = 1) +
                    geom_line(aes(x = x, y = pop2), col = 'blue', size = 1) + 
                    geom_ribbon(data = dfN[dfN$x >= Xc(),], aes(x, ymax = pop1), fill = 'red', ymin = 0, alpha = .5) + 
                    geom_ribbon(data = dfN[dfN$x <= Xc(),], aes(x, ymax = pop2), fill = 'blue', ymin = 0, alpha = .5) + 
                    scale_x_continuous(name = expression(x), breaks = c(0, Xc(), input$mu2), 
                                       labels = c(expression(mu[1]), expression(X[c]), expression(mu[2])),
                                       limits = c(-5, 5 + input$mu2)) +
                    scale_y_continuous(name = expression(Phi(x)), limits = c(0, .5)) + 
                    theme(axis.title.x = element_text(size = 20, hjust = -.01, vjust = 4), 
                          axis.title.y = element_text(size = 20),
                          axis.text.x  = element_text(face = 'bold', size = 20)) +
                    geom_text(x = -.75, y = .4, label = 'pi[1]', parse = T, size = 8) +
                    geom_text(x = input$mu2 + .75, y = .4, label = 'pi[2]', parse = T, size = 8) +
                    geom_segment(data = dfP, aes(x = m2 + 4, y = .1, xend = ifelse(max(xc, m2) == xc, xc + .1, 
                                                                                   mean(c(xc, m2))), yend = .01), 
                                 arrow = arrow(length = unit(0.5, 'cm'))) +
                    geom_segment(data = dfP, aes(x = -4, y = .1, xend = ifelse(min(xc, 0) == xc, xc - .1, 
                                                                               mean(c(xc, 0))), yend = .01), 
                                 arrow = arrow(length = unit(0.5, 'cm'))) + 
                    geom_text(x = input$mu2 + 3.95, y = .11, label = 'alpha', parse = T) +
                    geom_text(x = -4.05, y = .11, label = 'beta', parse = T)
               
               print(distPlot)
          })
          
          # theoretical misclassification probabilities in 2 x 2 contingency table
          output$rawPlot <- renderPlot({
               
               # alpha
               alpha = 1 - pnorm(Xc())
               # beta
               beta = pnorm(Xc(), input$mu2)
               
               # Contingency table
               plot(0:1, 0:1, type = 'n', ylim = c(0,1), xlim = c(0,1), axes = F, ylab = '', xlab = '')
               abline(h = -.04); abline(h = .4); abline(h = .8); abline(v = .3); 
               lines(x = c(.7, .7), y = c(-.04, .9)); abline(v = 1.04)
               text(.12, .85, 'Predicted', cex = 1.4)
               text(.475, .85, expression(pi[1]), cex = 1.5)
               text(.875, .85, expression(pi[2]), cex = 1.5)
               text(.68, 1, 'Truth', cex = 1.5)
               text(.12, .6, expression(hat(pi)[1]), cex = 1.5)
               text(.12, .2, expression(hat(pi)[2]), cex = 1.5)     
               text(.475, .65, expression(1 - alpha), cex = 1.5)
               text(.475, .55, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', 1 - alpha)), cex = 1.5) 
               text(.475, .25, expression(alpha), cex = 1.5)
               text(.475, .15, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', alpha)), cex = 1.5)
               text(.875, .65, expression(beta), cex = 1.5)
               text(.875, .55, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', beta)), cex = 1.5)     
               text(.875, .25, expression(1 - beta), cex = 1.5)
               text(.875, .15, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', 1 - beta)), cex = 1.5) 
               
          })
          
          # random sample of 100 observations from each distribution
          randsamp <- reactive({
               # create dependency on action button
               input$rsample
               isolate(return(data.frame(X = c(rnorm(round(input$N1*input$p1)), 
                                               rnorm(round(input$N1*(1-input$p1)), input$mu2)), 
                                         pop = rep(c('S1', 'S2'), times = c(round(input$N1*input$p1), 
                                                                            round(input$N1*(1-input$p1)))))))
          })
          
          # random sample of 100 observations from each distribution
          output$rSamplePlot <- renderPlot({
               # create dependency on action button
               input$rsample          
               
               densPlot = isolate(ggplot() + 
                                       geom_density(data = subset(randsamp(), pop == 'S1'), aes(x = X), 
                                                    fill = 'red', alpha = .5) +
                                       geom_density(data = subset(randsamp(), pop == 'S2'), aes(x = X), 
                                                    fill = 'blue', alpha = .5) + 
                                       geom_rug(data = subset(randsamp(), pop == 'S1'), aes(x = X), color = 'red') +
                                       geom_rug(data = subset(randsamp(), pop == 'S2'), aes(x = X), color = 'blue') +
                                       scale_x_continuous(name = expression(x), 
                                                          breaks = c(mean(randsamp()[randsamp()$pop == 'S1',1]), Xc(), 
                                                                     mean(randsamp()[randsamp()$pop == 'S2',1])),
                                                          labels = c(expression(bar(X)[1]), expression(X[c]), expression(bar(X)[1])), 
                                                          limits = c(-5, 5 + input$mu2)) + 
                                       geom_vline(xintercept = Xc()) + 
                                       theme(axis.title.x = element_text(size = 20, hjust = -.01, vjust = 4), 
                                             axis.title.y = element_text(size = 20),
                                             axis.text.x  = element_text(face = 'bold', size = 20))) 
               
               print(densPlot)
          })
          
          # observed contingency table
          output$obsPlot <- renderPlot({
               # create dependency on action button
               input$rsample 
               
               # true negatives
               tn = isolate(mean(subset(randsamp(), pop == 'S1')$X <= Xc()))
               # true positives
               tp = isolate(mean(subset(randsamp(), pop == 'S2')$X > Xc()))
               
               # Contingency table
               plot(0:1, 0:1, type = 'n', ylim = c(0,1), xlim = c(0,1), axes = F, ylab = '', xlab = '')
               abline(h = -.04); abline(h = .4); abline(h = .8); abline(v = .3); 
               lines(x = c(.7, .7), y = c(-.04, .9)); abline(v = 1.04)
               text(.12, .85, 'Predicted', cex = 1.4)
               text(.475, .85, expression(pi[1]), cex = 1.5)
               text(.875, .85, expression(pi[2]), cex = 1.5)
               text(.68, 1, 'Truth', cex = 1.5)
               text(.12, .6, expression(hat(pi)[1]), cex = 1.5)
               text(.12, .2, expression(hat(pi)[2]), cex = 1.5)     
               text(.475, .6, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', tn)), cex = 1.5)
               text(.475, .2, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', 1 - tn)), cex = 1.5)
               text(.875, .6, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', 1 - tp)), cex = 1.5)     
               text(.875, .2, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', tp)), cex = 1.5)
               
          })
          
          # random sample of 100 observations from each distribution
          randbisamp <- reactive({
               # create dependency on action button
               input$rbisample
               isolate(return(data.frame(cbind(
                    X = rbind(mvrnorm(round(input$N2*(1 - input$p1)), mu = c(0, 0), 
                                      Sigma = matrix(c(1, input$corr, input$corr, 1), 2)),
                              mvrnorm(round(input$N2*input$p1), 
                                      mu = c(sqrt(input$mu2/2), sqrt(input$mu2/2)), 
                                      Sigma = matrix(c(1, input$corr, input$corr, 1), 2))),
                    pop = factor(rep(c('1', '2'), times = c(round(input$N2*input$p1), round(input$N2*(1-input$p1)))))))))
          })
          
          # random sample of 100 observations from each distribution
          output$rbiSamplePlot <- renderPlot({
               # create dependency on action button
               input$rbisample
               # Population covariance (1 and 2) and means (2)
               Sigma = isolate(matrix(c(1, input$corr, input$corr, 1), 2))
               mu2 = isolate(c(sqrt(input$mu2/2), sqrt(input$mu2/2)))
               
               # slope discriminant function
               m = isolate(-(solve(Sigma)%*%mu2)[1]/(solve(Sigma)%*%mu2)[2])
               # intercept
               b = isolate((mu2%*%solve(Sigma)%*%mu2/2 - 
                                 log((10^(input$cost)*(1-input$p1))/input$p1))/(solve(Sigma)%*%mu2)[2])
               
               scatPlot = 
                    isolate(ggplot() + 
                                 geom_point(data = subset(randbisamp(), pop == '1'), aes(x = V1, y = V2), color = 'red') +
                                 geom_point(data = subset(randbisamp(), pop == '2'), aes(x = V1, y = V2), color = 'blue') +
                                 geom_abline(int = b + .02, slope = m, size = 1.2, col = 'blue') +
                                 geom_abline(int = b - .02, slope = m, size = 1.2, col = 'red') +
                                 scale_x_continuous(name = expression(X[1])) + 
                                 scale_y_continuous(name = expression(X[2])) + 
                                 theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20)))
               print(scatPlot)
          })
          
          # observed contingency table
          output$obsbiPlot <- renderPlot({
               # create dependency on action button
               input$rbisample 
               
               # Population covariance (1 and 2) and means (2)
               Sigma = isolate(matrix(c(1, input$corr, input$corr, 1), 2))
               mu2 = isolate(c(sqrt(input$mu2/2), sqrt(input$mu2/2)))
               
               # discriminant functions
               delta1 = isolate(rep(mu2%*%solve(Sigma)%*%mu2/2, sum(randbisamp()$pop == '1')) 
                                - as.matrix(subset(randbisamp(), pop == '1')[,1:2])%*%solve(Sigma)%*%mu2
                                - log((10^(input$cost)*(1-input$p1))/input$p1))
               # discriminant functions
               delta2 = isolate(rep(mu2%*%solve(Sigma)%*%mu2/2, sum(randbisamp()$pop == '2')) 
                                - as.matrix(subset(randbisamp(), pop == '2')[,1:2])%*%solve(Sigma)%*%mu2
                                - log((10^(input$cost)*(1-input$p1))/input$p1))
               
               # true negatives
               tn = isolate(mean(delta1 >= 0))
               # true positives
               tp = isolate(mean(delta2 < 0))
               
               # Contingency table
               plot(0:1, 0:1, type = 'n', ylim = c(0,1), xlim = c(0,1), axes = F, ylab = '', xlab = '')
               abline(h = -.04); abline(h = .4); abline(h = .8); abline(v = .3); 
               lines(x = c(.7, .7), y = c(-.04, .9)); abline(v = 1.04)
               text(.12, .85, 'Predicted', cex = 1.4)
               text(.475, .85, expression(pi[1]), cex = 1.5)
               text(.875, .85, expression(pi[2]), cex = 1.5)
               text(.68, 1, 'Truth', cex = 1.5)
               text(.12, .6, expression(hat(pi)[1]), cex = 1.5)
               text(.12, .2, expression(hat(pi)[2]), cex = 1.5)     
               text(.475, .6, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', tn)), cex = 1.5)
               text(.475, .2, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', 1 - tn)), cex = 1.5)
               text(.875, .6, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', 1 - tp)), cex = 1.5)     
               text(.875, .2, sub('^(-)?0[.]', '\\1.', sprintf('%.2f', tp)), cex = 1.5)
               
          })
          
          
     })
