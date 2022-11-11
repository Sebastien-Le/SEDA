# This file is a generated template, your changes will not be overwritten

JARClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "JARClass",
  inherit = JARBase,
  private = list(       
    
    
    #### Init + run functions ----
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

