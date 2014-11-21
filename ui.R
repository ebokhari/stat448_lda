# ui file
# app for visual demonstration of how the positive and negative predictive values 
# change across fixed sensitivity and specificity values when the base rate changes

shinyUI(
     fluidPage(
          title = 'Stat 448: Linear Discriminant Analysis',
          
          sidebarPanel(
               
               # For LaTeX
               tags$head(tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                         tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')),
               # message
               h3('Demonstration of linear discriminant analysis', align = 'center'),
               br(), 
               p('Linear discriminant analysis for the univariate case. Two populations, ', HTML('$\\pi_1$'), 
                 ' and ', HTML('$\\pi_2$,'), 'follow normal distributions with means ', HTML('$\\mu_1$'), 
                 'and', HTML('$\\mu_2$'), 'and common variance', HTML('$\\sigma^2$.'), 'The user can select 
                 the difference between the means,', HTML('$\\mu_2 - \\mu_1$,'), 'the prior probability for 
                 population 1,', HTML('$p_1$,'), 'where', HTML('$p_1 + p_2 = 1$,'), 'and the cost ratio of 
                 false negatives to false positives,', HTML('$\\frac{c_{1|2}}{c_{2|1}}$,'), 'where observations 
                 from', HTML('$\\pi_1$'), 'are considered the negative cases and observations from', 
                 HTML('$\\pi_2$'), 'are the positive cases. Note that the inputted cost is on the ',
                 HTML('$\\log_{10}$-'), 'scale and the actual ratio of cost ranges from approximately .05
                 to 20.'),
               
               br(), 
               
               # input value of mean differences
               sliderInput('mu2', label = h5(c('Difference in population means:', HTML('$\\mu_2 - \\mu_1$'))),
                           min = .1, max = 5, value = 1, step = .1),
               
               br(),
               
               # input prior probability
               sliderInput('p1', label = h5(c('Prior probability of population 1:', HTML('$ p_1$'))),
                           min = .01, max = .99, value = .5, step = .01),
               
               br(),
               
               # input costs
               sliderInput('cost', label = h5(c('Ratio of cost of false negatives to false positives:', 
                                                HTML('$\\log\\left(\\frac{c_{1|2}}{c_{2|1}}\\right)$'))),
                           min = -1.3, max = 1.3, value = 0, step = .01),
               
               br(), br(),
               
               conditionalPanel(
                    condition = "input.condPanels == 'T'",
                    p('The ', em('Theoretical'), ' tab displays the theorectical normal distributions with a cutscore,', HTML('$X_c$,'),
                      'obtained from the user-inputted differences in means, prior probabilities, and costs. The tab also displays a',
                      em(a('Receiver Operating Characteristic', href = 'https://en.wikipedia.org/wiki/Receiver_operating_characteristic')), 
                      'curve with the associated', em('Area Under the Curve'), 'and the cutscore marked with a ', span('red', style = 'color:red'),
                      ' dot on the curve. Finally, a 2x2 contingency table is provided that is obtained using the cutscore,', HTML('$X_c$.'))),
               conditionalPanel(
                    condition = "input.condPanels == 'R'",
                    p('The ', em('Random Sampling'), ' tab allows the user to randomly select ', HTML('$N$'), 'observations total from the 
                      two populations, where ', HTML('$N$'), ' is set by the user and between 50 and 100,000. The proportion of observations
                      from each population is given by the priors ', HTML('$p_1$'), ' and ', HTML('$p_2$.'), '.The tab displays the 
                      results in a figure with the estimated density plots and a 2x2 contingency table.')),
               conditionalPanel(
                    condition = "input.condPanels == 'B'",
                    p('The ', em('Bivariate Example'), 'tab extends the random sampling to the bivariate case. Here the inputted difference 
                      in means is the Euclidean distance of the two population mean vectors. The user randomly selects ', HTML('$N$'), 
                      'observations total from the two populations, where ', HTML('$N$'), ' is set by the user and between 50 and 100,000. 
                      The proportion of observations from each population is given by the priors ', HTML('$p_1$'), ' and ', HTML('$p_2$.'), 
                      'The user also specifies the correlation between ', HTML('$X_1$'), ' and ', HTML('$X_2$.'), 'The tab displays the 
                      results in a scatterplot with the "true" discriminant line and a 2x2 contingency table of the sample results.'))
               
          ),
          
          mainPanel(
               tabsetPanel(
                    tabPanel('Theoretical',
                             
                             fluidRow(
                                  plotOutput('distPlot')
                             ),
                             
                             fluidRow(
                                  column(6, 
                                         plotOutput('rocPlot')
                                  ),
                                  
                                  column(6, 
                                         plotOutput('rawPlot')
                                  )
                             ), 
                             value = 'T'
                    ),
                    
                    tabPanel('Random Sampling',
                             
                             actionButton('rsample', 'Random Sample'),
                             
                             br(),
                             
                             numericInput('N1', label = 'Sample Size', value = 100, min = 50, max = 100000, step = 50),
                             
                             plotOutput('rSamplePlot'),
                             plotOutput('obsPlot'),
                             
                             value = 'R'
                    ),
                    
                    tabPanel('Bivariate Example',
                             
                             actionButton('rbisample', 'Random Sample'),
                             
                             br(),
                             
                             fluidRow(
                                  column(6,
                                         numericInput('N2', label = 'Sample Size', value = 100, min = 50, max = 100000, step = 50)
                                  ),
                                  column(6,
                                         numericInput('corr', label = paste('Correlation of', HTML('$X_1$'), 'and', HTML('$X_2$')), 
                                                      value = 0, min = -.95, max = .95, step = .05)
                                  )
                             ),
                             
                             plotOutput('rbiSamplePlot'),
                             plotOutput('obsbiPlot'),
                             
                             value = 'B'
                    ),
                    tabPanel('App Information',
                             fluidRow(
                                  h4('Designed for Stat 448 class at the University of Illinois at Urbana-Champaign'), 
                                  h4('by Ehsan Bokhari.'),
                                  br(),
                                  img(src = 'http://perception.csl.illinois.edu/logos/UIUC_logo.gif', width = 400, height = 600)
                             )
                    ),
                    id = 'condPanels'
               )
          )
          
     )
)
