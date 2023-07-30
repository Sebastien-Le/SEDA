#' Correlation Analysis

#' @import dplyr
#' @import corrplot
#' @export

# This file is a generated template, your changes will not be overwritten


cartoClass = if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "cartoClass",
    inherit = cartoBase,
    private = list(

    #---------------------------------------------  
    #### Init + run functions ----

            .init = function() {
            if (is.null(self$data) | is.null(self$options$hedo)) {
              if (self$options$tuto==TRUE){
                self$results$instructions$setVisible(visible = TRUE)
              }
            }
            
            self$results$instructions$setContent(
            "<html>
            <head>
            </head>
            <body>
            <div class='justified-text'>
            <p><b>What you need to know before analysing hedonic data in jamovi</b></p>
            <p>______________________________________________________________________________</p>

            <p> External Preference Mapping is a technique used to understand the relationships between sensory attributes of a set of products and consumer preferences.
            It is a part of consumer research in sensory science and is commonly employed in the food and beverage industry and other product development fields.</p>
            
            <p> The goal of External Preference Mapping is to explore and quantify the sensory characteristics of products that drive consumer liking or preferences.</p>

            <p> In the SEDA module, to obtain a preference map, you need two elements: a two-dimensional representation space, on the one hand, hedonic scores, on the other hand.</p>

            <p> Open the <b>senso_hedo_cocktail</b> dataset. With the <b>MEDA module</b>, run a PCA on the sensory attributes (from color intensity to thickness) and make sure to save the 2 first components. Once you have 
            saved the representation space, you can now use it to represent your hedonic scores.</p>

            <p> Come back to the external preference mapping analysis. The X-axis and Y-axis of your representation space are the 2 first components that
            you have previously saved. The liking variables are the columns from O to DJ.</p>

            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },

      .run = function() {
        if (is.null(self$options$coox)|| is.null(self$options$cooy)|| is.null(self$options$hedo)){
          return()
        }
        else {
          data = private$.buildData()
          
          imagecol = self$results$plotcarto
          imagecol$setState(data)
          }
        },
      
      .plotcartograph= function(image, ...){
        if (is.null(self$options$coox)|| is.null(self$options$cooy)|| is.null(self$options$hedo)){
          return()
        }
        else if (length(self$options$hedo) < 2) {
          jmvcore::reject("The number of Liking Variables is too low for the Representation of the Products")
          return()
        }
        else {
          data=image$state
          n=ncol(data)
          regtype_gui <- self$options$regtype
          regtype_gui <- as.numeric(regtype_gui)
          colbelow_gui <- self$options$colbelow
          colabove_gui <- self$options$colabove
          
          plot=private$.carto_jamovi(data[,1:2],data[,3:n], main = 'External Preference Mapping', regmod = regtype_gui, col.min = colbelow_gui, col.max = colabove_gui)
          print(plot)
          TRUE
        }
      },
      
      .buildData = function() {
        
        datacoox=data.frame(self$data[,self$options$coox])
        colnames(datacoox)=self$options$coox
        
        datacooy=data.frame(self$data[,self$options$cooy])
        colnames(datacooy)=self$options$cooy
        
        datanote=data.frame(self$data[,self$options$hedo])
        colnames(datanote)=self$options$hedo
        
        data=data.frame(datacoox, datacooy, datanote)
        
        if (is.null(self$options$individus)==FALSE) {
          rownames(data)=self$data[[self$options$individus]]
        }
        else
          rownames(data)=c(1:nrow(data))
        
        return(data)
      },
      
      .carto_jamovi = function (Mat, MatH, level = 0, regmod = 1, coord = c(1, 2), 
                                asp = 1, cex = 1.3, col = "steelblue4", font = 2, clabel = 0.8, 
                                label.j = FALSE, resolution = 200, nb.clusters = 0, graph.tree = TRUE, 
                                graph.corr = TRUE, graph.carto = TRUE, main = NULL, col.min = 7.5, 
                                col.max = 0) 
      {
        cm.colors2 = function(n, alpha = 1) {
          if ((n = as.integer(n[1L])) > 0) {
            even.n = n%%2 == 0
            k = n%/%2
            l1 = k + 1 - even.n
            l2 = n - k + even.n
            c(if (l1 > 0) hsv(h = col.min/12, s = seq.int(0.8, 
                                                          ifelse(even.n, 0.5/k, 0), length.out = l1), v = 1, 
                              alpha = alpha), if (l2 > 1) hsv(h = col.max/12, 
                                                              s = seq.int(0, 0.8, length.out = l2)[-1L], v = 1, 
                                                              alpha = alpha))
          }
          else character(0L)
        }
        predire = function(n1, n2, coeff) {
          coeff[1] + coeff[2] * n1 + coeff[3] * n2 + coeff[4] * 
            n1 * n1 + coeff[5] * n2 * n2 + coeff[6] * n1 * n2
        }
        if (!is.data.frame(MatH)) 
          stop("Non convenient selection for MatH")
        if (any(is.na(MatH))) {
          missing = which(is.na(MatH))
          MatH[missing] = (matrix(rep(apply(MatH, 1, mean, na.rm = T), 
                                       ncol(MatH)), ncol = ncol(MatH)) + matrix(rep(apply(MatH, 
                                                                                          2, mean, na.rm = T), each = nrow(MatH)), ncol = ncol(MatH)) - 
                              matrix(rep(mean(MatH, na.rm = TRUE), ncol(MatH) * 
                                           nrow(MatH)), ncol = ncol(MatH)))[missing]
        }
        matrice = cbind(row.names(MatH), Mat[rownames(MatH), ], 
                         MatH)
        
        
        matrice[, 4:ncol(matrice)] = scale(matrice[, 4:ncol(matrice)], 
                                            center = TRUE, scale = FALSE)[, ]
        nbconso = ncol(matrice) - 3
        x1 = matrice[, 2]
        x2 = matrice[, 3]
        x12 = scale(x1, center = TRUE, scale = FALSE)[, ]^2
        x22 = scale(x2, center = TRUE, scale = FALSE)[, ]^2
        x12plusx22 = x12 + x22
        x3 = scale(x1, center = TRUE, scale = FALSE)[, ] * scale(x2, 
                                                                  center = TRUE, scale = FALSE)[, ]
        XX = cbind(x1, x2, x12, x22, x3)
        etendue.x1 = diff(range(x1))
        etendue.x2 = diff(range(x2))
        pas = max(etendue.x1, etendue.x2)/resolution
        f1 = seq((min(x1) - etendue.x1 * 0.05), (max(x1) + etendue.x1 * 
                                                    0.05), pas)
        f2 = seq((min(x2) - etendue.x2 * 0.05), (max(x2) + etendue.x2 * 
                                                    0.05), pas)
        depasse = matrix(0, nrow = length(f1), ncol = length(f2))
        abscis = NULL
        ordon = NULL
        for (i in 1:nbconso) {
          if (regmod == 1) 
            coeff = lm(matrice[, i + 3] ~ XX[, 1] + XX[, 2] + 
                          XX[, 3] + XX[, 4] + XX[, 5], na.action = na.omit)$coef
          if (regmod == 2) {
            coeff = lm(matrice[, i + 3] ~ XX[, 1] + XX[, 2], 
                        na.action = na.omit)$coef
            coeff = c(coeff, 0, 0, 0)
          }
          if (regmod == 3) {
            coeff = lm(matrice[, i + 3] ~ x1 + x2 + x12plusx22, 
                        na.action = na.omit)$coef
            coeff = c(coeff, coeff[4], 0)
          }
          if (regmod == 4) {
            coeff = lm(matrice[, i + 3] ~ XX[, 1] + XX[, 2] + 
                          XX[, 3] + XX[, 4], na.action = na.omit)$coef
            coeff = c(coeff, 0)
          }
          predites = outer(f1, f2, predire, coeff)
          if (sd(as.vector(predites), na.rm = TRUE) != 0) 
            predites = (predites - mean(predites, na.rm = TRUE))/sd(as.vector(predites), 
                                                                     na.rm = TRUE)
          depasse = depasse + matrix(as.numeric(predites > level), 
                                      nrow = length(f1), ncol = length(f2))
          abscis = c(abscis, f1[rev(order(predites))[1] - length(f1) * 
                                   as.integer((rev(order(predites))[1] - 0.5)/length(f1))])
          ordon = c(ordon, f2[as.integer(1 + (rev(order(predites))[1] - 
                                                 0.5)/length(f1))])
        }
        nb.depasse = depasse
        depasse = round(depasse/nbconso * 100)
        dimnames(depasse) = list(as.character(f1), as.character(f2))
        if (graph.carto) {
          if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            #   dev.new()
            col = cm.colors2(100)
          if (is.null(main)) 
            main = "Preference mapping"
          image(f1, f2, depasse, col = col, xlab = colnames(Mat)[1], ylab = colnames(Mat)[2], main = main, 
                font.main = font, , cex.main = cex, asp = asp)
          contour(f1, f2, depasse, nlevels = 9, levels = c(20, 
                                                           30, 40, 50, 60, 70, 80, 90, 95), add = TRUE, labex = 0)
          for (i in 1:nrow(matrice)) {
            points(matrice[i, 2], matrice[i, 3], pch = 15)
            text(matrice[i, 2], matrice[i, 3], matrice[i, 1], 
                 pos = 4, offset = 0.2, )
          }
          #points(abscis, ordon, pch = 20)
        }
        res = list()
        res$nb.depasse = nb.depasse
        res$f1 = f1
        res$f2 = f2
        res$abscis = abscis
        res$ordon = ordon
        res$matrice = matrice
        return(res)
      }
    )
)




