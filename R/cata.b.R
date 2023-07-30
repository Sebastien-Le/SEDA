
# This file is a generated template, your changes will not be overwritten


cataClass = if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "cataClass",
    inherit = cataBase,
    private = list(
    
    #---------------------------------------------  
    #### Init + run functions ----

      .init = function() {
            if (is.null(self$data) | is.null(self$options$group)) {
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
            <p><b>What you need to know before analysing CATA data in jamovi</b></p>
            <p>______________________________________________________________________________</p>

            <p> CATA stands for <I>Check-All-That-Apply</I>. CATA is a method used in sensory evaluation and consumer research to obtain 
            descriptive and discriminative information about stimuli, products or samples.</p>
            <p> In a CATA test, panelists or consumers are presented with a list of attributes or sensory characteristics related to the stimuli they are evaluating. 
            The panelists are asked to check all the attributes that they perceive or find present in the stimulus. 
            Unlike traditional rating scales where panelists assign a single score to each attribute, CATA allows the panelists 
            to select multiple attributes that they perceive simultaneously in the stimulus.</p>
            <p> CATA captures the multiplicity of attributes: CATA allows panelists to express the complex sensory profiles of 
            stimuli by selecting multiple attributes that co-occur in the same stimulus. CATA is user-friendly: The check-all-that-apply format is 
            straightforward and easy for panelists to understand and complete.</p>

            <p> In the SEDA module, the analysis of CATA data is based on a contingency table, and a two steps procedure. After building the 
            contingency table, the interface provides a description of the stimuli according to the CATA items. Then, a correspondence 
            analysis is performed to vizualize the relationship between the stimuli and the CATA items.</p>

            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },

      .run = function() {
        if (is.null(self$options$stimuli)|| is.null(self$options$group)){
          return()
        }
        else if (length(self$options$group) < 2) {
          jmvcore::reject("The number of factors is too low")
          return()
        }
        else { 
          data = private$.buildData()
          
          res.dataprod = private$.Dataprod(data)
          # self$results$dataproduit$setContent(res.dataprod)

          imagecol = self$results$plotcata
          imagecol$setState(res.dataprod)

          dfres = private$.descfreq(res.dataprod)
          res.descfreqcol = private$.descfreq(t((res.dataprod)))
          t = 0
          for (i in 1:length(dfres)) {
            if (is.null(dfres[[i]]) == FALSE)
              t = t+1
          }
          
          if (t != 0) {
            j <- 1
            xinit <- as.data.frame(dfres[[j]])
            xxinit <- cbind(rep(names(dfres)[j],dim(xinit)[1]),rownames(xinit),xinit)
            colnames(xxinit)[1] <- "Modality"
            colnames(xxinit)[2] <- "Word"
            rownames(xxinit) <- NULL
            
            for (j in 1:(length(dfres)-1)) {
              j <- j+1
              xj <- as.data.frame(dfres[[j]])
              xxj <- cbind(rep(names(dfres)[j],dim(xj)[1]),rownames(xj),xj)
              colnames(xxj)[1] <- "Modality"
              colnames(xxj)[2] <- "Word"
              rownames(xxj) <- NULL
              xxinit <- rbind(xxinit,xxj)
            }
            
            tab <- xxinit
          private$.populateTEXTUALTable(res.dataprod)
          # self$results$frequencebrutdes$setContent(dfres)
          private$.populateDFTable(tab)
          
          }
        }
        },
      
      .descfreq = function(res.dataprod) {
        threshold=self$options$thres/100
        res.descfreq = descfreq(res.dataprod, proba = threshold)
        return(res.descfreq)
      },
      
      # Image CA Représentant des produits par attribues
      .plotcatatis= function(image, ...){
        if (is.null(self$options$stimuli)|| is.null(self$options$group)){
          return()
        }
        else if (length(self$options$group) < 3) {
          jmvcore::reject("The number of factors is too low for the Representation of the Products and the CATA")
          return()
        }
        else {
          res.dataprod=image$state #Really important line
          res.ca = FactoMineR::CA(res.dataprod)
          plot=plot.CA(res.ca, title = "Representation of the Stimuli and the CATA")
          print(plot)
          TRUE}
        },
      
      #Réalisation du tableau par produit
      .Dataprod = function(data) {
        formula=reformulate(self$options$stimuli, response = ".")
        data=aggregate(formula, data = data, sum)
        rownames(data)=data[,1]
        res.dataprod=data[,-1]
        
        return(res.dataprod)
      },
      .populateTEXTUALTable = function(table) {
        
        stimuli=levels(self$data[[self$options$stimuli]]) #nom de la variable à décrire
        group=row.names(self$data[,self$options$group]) #nom de la variable textuelle
        textual=self$results$textualgroup$textual #fait réf. au tableau de contingence dans les sorties jamovi
        coltable=colnames(table) #tableau de contingence
        
        textual$addColumn(name="rownames",title="",type="text") #on renseigne le tableau de contingence de la sortie jamovi
        for (i in 1:length(coltable))
          textual$addColumn(name=coltable[i], title=coltable[i], type="integer")
        
        for (i in seq_along(stimuli)) {#
          row=list()
          row[["rownames"]]=rownames(table)[i]
          
          for (j in 1:length(coltable)) {
            row[[coltable[j]]]=table[i,j] #on recopie le tableau de contingence
          }
          textual$addRow(rowKey=i, values=row)
        }#
        
        total=list()
        total[["rownames"]]="Nb.group"
        
        for (j in 1:length(coltable))
          total[[coltable[j]]]=sum(table[,j])
        
        textual$addRow(rowKey=length(coltable)+1, values = total)
      },
      .populateDFTable= function(tab){
        
        # for (i in 1:dim(tab)[1]){
        #   self$results$dfresgroup$dfres$addRow(rowKey=i, values=list(component=as.character(tab[,1])[i])) #Méthode addRow
        # }
        
        for (i in 1:(dim(tab)[1])) {
          row=list()
          self$results$dfresgroup$dfres$addRow(rowKey=i, values=list(component=as.character(tab[,1])[i]))
          row[["word"]]=as.character(tab[,2])[i]
          row[["internper"]]=tab[,3][i]
          row[["globper"]]=tab[,4][i]
          row[["internfreq"]]=tab[,5][i]
          row[["globfreq"]]=tab[,6][i]
          row[["pvaluedfres"]]=tab[,7][i]
          row[["vtest"]]=round(tab[,8][i], digits = 2)
          self$results$dfresgroup$dfres$setRow(rowKey=i, values = row) #Méthode setRow
        }
        
      },
      .buildData = function() {
        datastimuli=data.frame(self$data[,self$options$stimuli])
        colnames(datastimuli)=self$options$stimuli
        
        datanote=data.frame(self$data[,self$options$group])
        colnames(datanote)=self$options$group

        data=data.frame(datastimuli, datanote)
        return(data)
      }
    )
)