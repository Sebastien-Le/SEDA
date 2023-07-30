# This file is a generated template, your changes will not be overwritten

JARClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "JARClass",
  inherit = JARBase,
  private = list(       
        
    #---------------------------------------------  
    #### Init + run functions ----
      
      .init = function() {
            if (is.null(self$data) | is.null(self$options$sensoatt)) {
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
            <p><b>What you need to know before analysing JAR data in jamovi</b></p>
            <p>______________________________________________________________________________</p>

            <p> <I>Just About Right</I> (JAR) data is a type of sensory evaluation data used in the sensory science and consumer research. 
            It is a specific scaling method that aims to determine the ideal or optimal level of a sensory attribute in a stimulus/product, as perceived by consumers or sensory panelists.</p>
            <p> In a JAR test, participants are presented with stimuli that vary in the intensity or level of a particular sensory attribute (<I>e.g.</I>, sweetness, saltiness, spiciness, etc.). 
            Participants are then asked to rate each sample on a scale that typically ranges from <I>Too Little</I> to <I>Just About Right</I> to <I>Too Much</I>.</p>
            <p> The JAR scale is designed to capture the point at which the sensory attribute is perceived as being neither too weak nor too strong but is at an ideal or preferred level. 
            The Just About Right point represents the optimal level of the attribute that best satisfies the consumers' preferences or expectations for that specific stimulus/product.<p>
            <p> JAR data is valuable for product development and optimization, as it helps product developers and marketers understand the target sensory attributes that consumers find most appealing and desirable. 
            By identifying the Just About Right levels of sensory attributes, manufacturers can fine-tune their products to meet consumer preferences, leading to better consumer acceptance and satisfaction. 
            Additionally, JAR tests can be used to compare different product formulations or variations to find the optimal sensory profile that aligns with consumer preferences.</p>
            
            <p> In the SEDA module, the focus is on the defects. Therefore, the interface provides a contingency table crosses stimuli and defects, a description 
            of the stimuli according to the defects, a representation of the products and their respective defects. Penalties are calculated relatively 
            to the whole set of stimuli, then a representation of the penalties for the <b>first</b> stimulus is provided.</p>

            <p> <b>Warning</b>: the first stimulus is the one corresponding to the first level of the stimulus variable. The value of the levels can be modified
            to obtain a representation of another stimulus. To do so, click on the Data tab, then on the stimulus variable: by clicking on the 
            arrows next the levels field, you can change the order of the levels, and therefore of the stimuli.</p>

            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },

    .run = function() {
      
      ready <- TRUE
      if (is.null(self$options$prodeff) || is.null(self$options$paneff) || is.null(self$options$likvar) || is.null(self$options$sensoatt)){
        return()
        ready <- FALSE
      }
      
      if (ready) {      
      
      data <- private$.buildData()
      res.jar <- private$.JAR(data)
      res.freq <- private$.JARCA(data)
      
      imagecol = self$results$plotjar
      imagecol$setState(res.freq)
      
      self$results$frequencebrut$setContent(res.freq$Frequency)
      self$results$frequencebrutdes$setContent(res.freq$res.descfreq)
      self$results$penalty$setContent(res.jar$penalty2)
      self$results$frequence$setContent(res.jar$Frequency)
      
      imagecol = self$results$plotpen
      imagecol$setState(res.jar)
      
      }
    },
    
    .JAR = function(data) {

      jarlev=self$options$jarmod
      res.jar <- SensoMineR::JAR(data, col.p=1, col.j=2, col.pref=3, jarlevel = jarlev)
      # res <- res.jar$penalty2
      # colnames(res) <- c("Penalty", "Std. Error", "Pr(>|t|)")
      colnames(res.jar$penalty2) <- c("Penalty", "Std. Error", "Pr(>|t|)")
      #print(res)
      return(res.jar)
    },
    
    .JARCA = function(data) {
      
      x=data
      col.p=1
      col.j=2
      col.pref=3
      jarlevel="jar"
      
      fct.delete.first <- function(x) {
        x[-1]
      }
      
      ind.jar <- (1:ncol(x))[-c(col.p, col.j, col.pref)]
      nbjar <- length(ind.jar)
      for (i in ind.jar) {
        x[, i] = relevel(x[, i], jarlevel)
      }
      
      nbmod = rep(0, ncol(x))
      for (j in ind.jar) {
        nbmod[j] = nlevels(x[, j]) - 1
      }
      nbmodtot = sum(nbmod)
      
      varesid = rep(1, nbjar + 1)
      nommod = rep("a", nbmodtot)

      ifin = 0
      for (j in ind.jar) {
        ideb = ifin + 1
        ifin = ideb + nbmod[j] - 1
        npar = nbmod[j] + 1
        nommod[c(ideb:ifin)] = levels(x[, j])[2:npar]
      }
      
      Frequency <- matrix(NA, nrow = nbmodtot, ncol = nlevels(x[, 
                                                                col.p]))
      for (j in 1:ncol(Frequency)) Frequency[, j] <- unlist(lapply(lapply(x[x[, 
                                                                              col.p] == levels(x[, col.p])[j], ind.jar], table), fct.delete.first))
      rownames(Frequency) = nommod
      colnames(Frequency) = levels(x[, col.p])
      
      res.descfreq <- descfreq(t(Frequency), proba = 0.5)
      
      result = list() 
      result$Frequency = Frequency
      result$res.descfreq = res.descfreq

      return(result)
    },
    
    .plotboth= function(image, ...){
      
      if (is.null(self$options$sensoatt)) return()
      
      else {
          res.freq=image$state #Really important line
          res.ca <- FactoMineR::CA(res.freq$Frequency)
          plot=plot.CA(res.ca, title = "Representation of the Products According to Defects")
        
        print(plot)
        TRUE
      }
    },

    .plotpenalty= function(image, ...){
      
      if (is.null(self$options$sensoatt)) return()
      
      else {
        res.jar=image$state
        x = res.jar
        i <- 1

        name.prod=colnames(x$Frequency)[i]
        confidence = TRUE
        level = 0.05
        penal <- x$penalty2
        
        nbmodtot <- nrow(penal)
        coord = matrix(NA, nrow = nbmodtot, ncol = 6)
        colnames(coord) <- c("V1","V2","V3","V4","V5","V6")
        
        rownames(coord) = rownames(penal)
        coord[1:nbmodtot, 1:4] = cbind(x$Frequency[, name.prod], penal)
        coord[, 5] = coord[, 2] - qnorm(1 - 0.05/2) * coord[, 3]
        coord[, 6] = coord[, 2] + qnorm(1 - 0.05/2) * coord[, 3]
        
        coord <- as.data.frame(coord)
        
        coord$sig[coord$V4< 0.05] <- "Sig"
        coord$sig[coord$V4>= 0.05] <- "Non_sig"
        coord$sig <- as.factor(coord$sig)
        
        plot <- ggplot(coord, aes(x=V1, y=V2, colour=sig), label=rownames(coord)) +
          geom_errorbar(aes(ymin=(V2-qnorm(1 - 0.05/2)*V3), ymax=(V2+qnorm(1 - 0.05/2)*V3)), width=.05, linetype=2)+
          geom_point() +
          geom_text(label=ifelse(coord$V4<0.20, as.character(rownames(coord)),''), angle=90)+
          scale_colour_manual(values = c("#00AFBB","red")) +
          coord_cartesian(xlim =c(0, 100))+
          ggtitle(paste("Penalties for Product",name.prod)) +
          xlab("Frequency") + ylab("Penalty based on all Attributes")

        print(plot)
        TRUE
      }
    },
    
    .buildData = function() {
      
      dataprodeff=data.frame(self$data[,self$options$prodeff])
      colnames(dataprodeff)=self$options$prodeff
      
      datapaneff=data.frame(self$data[,self$options$paneff])
      colnames(datapaneff)=self$options$paneff
      
      datalikvar=data.frame(self$data[,self$options$likvar])
      colnames(datalikvar)=self$options$likvar
      
      datasensoatt=data.frame(self$data[,self$options$sensoatt])
      colnames(datasensoatt)=self$options$sensoatt
      
      data=data.frame(dataprodeff,datapaneff,datalikvar,datasensoatt)

      return(data)
    }
  )
)

