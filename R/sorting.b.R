SortingClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "SortingClass",
    inherit = SortingBase,
    
    active = list(
      dataProcessed = function() {
      if (is.null(private$.dataProcessed))
      private$.dataProcessed <- private$.buildData()
      return(private$.dataProcessed)
      },
      #important car si on passe directement par self$nbclust doesn't work
      nbclust = function() {
            if (is.null(private$.nbclust))
                private$.nbclust <- private$.computeNbclust()

            return(private$.nbclust)
        },
      nQualsup = function() {
            if (is.null(private$.nQualsup))
                private$.nQualsup <- private$.computeNQualsup()

            return(private$.nQualsup)
        },
      SortingResult = function() {
        if (is.null(private$.SortingResult))
        private$.SortingResult <- private$.getSortingResult()
        return(private$.SortingResult)
      }
    ),

    private = list(
        
        .dataProcessed = NULL,
        .nbclust = NULL,
        .SortingResult = NULL,

#---------------------------------------------  
#### Init + run functions ----

        .init = function() {
            if (is.null(self$data) | is.null(self$options$actvars)) {
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
            <p><b>What you need to know before analysing sorting data in jamovi</b></p>
            <p>______________________________________________________________________________</p>

            <p> Sorting task refers to a type of cognitive test or experimental procedure where assessors are asked to categorize or group stimuli 
            based on the way they perceive the stimuli. The stimuli can be any sensory information, such as visual, auditory, or tactile, and the task aims to understand how people organize and process this information.</p>
            
            <p>  Sorting tasks are commonly used in psychological research to study various aspects of cognition, perception, and information processing. 
            These tasks can provide insights into how individuals perceive and make sense of the world around them, how they categorize different stimuli, and how they form mental representations of the information.</p>

            <p> Sometimes groups of stimuli are labelled. These labels are essential for interpreting how the stimuli as a whole are perceived. 
            The SEDA module treats labels as textual information and analyses them as such.</p>

            <p> The interface provides different types of results. First, a description of the stimuli based on the labels, second a multivariate
            representation of the stimuli according to the groups provided by the assessors. This multivariate analysis is completed by an unsupervised 
            clustering of the stimuli.</p>

            <p> Clustering is based on the number of components saved. By default, clustering is based on the first 5 components,
            <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>

            <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of clusters is automatically chosen by the computer.</p>

            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },        

        .run = function() {
            
            if (is.null(self$options$actvars)) return()
            
            else if (length(self$options$actvars) < self$options$nFactors) {
                jmvcore::reject("The number of factors is too low")
                return()
            }
            
            else {
                
                predata <- private$.buildData()
                
                data <- predata[[1]]
                datatext <- predata[[2]]

                self$results$longformatdata$setContent(datatext)
                
                #if (self$options$longformat==TRUE){
                #lancement de l'analyse textuelle
                res.textual=private$.textual(datatext)
                dfres=private$.descfreq(res.textual)

                # t = 0
                # for (i in 1:length(dfres)) {
                #     if (is.null(dfres[[i]]) == FALSE)
                #         t = t+1
                # }
                
                #t sert a savoir si dfres est completement NULL ou non. 
                #Si oui, t=0 et la suite n'est pas faite. 
                #Si non, t=1 et la suite est faite.
                
                if (FALSE%in%lapply(dfres,is.null)) {

                    j <- 1
                    xinit <- as.data.frame(dfres[[j]])
                    xxinit <- cbind(rep(names(dfres)[j],dim(xinit)[1]),rownames(xinit),xinit)
                    colnames(xxinit)[1] <- "Modality"
                    colnames(xxinit)[2] <- "Word"
                    rownames(xxinit) <- NULL

                    for (j in 1:(length(dfres)-1)) { ###
                        j <- j+1
                        xj <- as.data.frame(dfres[[j]])
                        xxj <- cbind(rep(names(dfres)[j],dim(xj)[1]),rownames(xj),xj)
                        colnames(xxj)[1] <- "Modality"
                        colnames(xxj)[2] <- "Word"
                        rownames(xxj) <- NULL
                        xxinit <- rbind(xxinit,xxj)
                    } ###
                    tab <- xxinit
                    private$.populateDFTable(tab)
                }
                #}
                                
                res.mca=private$.MCA(data)
                res.classif <- private$.classif(res.mca)

                dimdesc=private$.dimdesc(res.mca)
                self$results$dimdesc$setContent(dimdesc)

                private$.printeigenTable(res.mca)
                private$.printTables(res.mca, "coord")
                private$.printTables(res.mca, "contrib")
                private$.printTables(res.mca, "cos2")

                imageindiv=self$results$plotindiv
                imageindiv$setState(res.mca)

                imagevar=self$results$plotvar
                imagevar$setState(res.mca)

                imageitemvar=self$results$plotitemvar
                imageitemvar$setState(res.mca)

                if (self$options$graphclassif==TRUE){
                    imageclass = self$results$plotclassif
                    imageclass$setState(res.classif)
                }          

                private$.output(res.mca)
                private$.output2(res.classif)
            }
        },
        
        .dimdesc = function(table) {            
            proba_gui=self$options$proba/100
            nFactors_gui=self$options$nFactors
            
            res=dimdesc(table, axes=1:nFactors_gui, proba = proba_gui) 
            print(res[-length(res)])
        },
        
        .textual = function(data) {
            FactoMineR::textual(data, num.text = 2, contingence.by = 1, sep.word = ";")
        },
        
        .descfreq = function(res) {
            threshold=self$options$leveltext/100
            FactoMineR::descfreq(res$cont.table, proba = threshold)
        },
        
        .computeNbclust = function() {
            nbclust <- self$options$nbclust
            return(nbclust)
        },
        
        .MCA = function(data) {
            
            actvars_gui=self$options$actvars
            qualisup_gui=self$options$qualisup
            #nFactors_gui=self$options$nFactors
            ventil=self$options$ventil/100
            
            if (is.null(qualisup_gui) == FALSE) {
                FactoMineR::MCA(data, quali.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(qualisup_gui)), ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
            }
            else {
                FactoMineR::MCA(data,ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
            }
        },

        .classif = function(res) {
          FactoMineR::HCPC(res,nb.clust=self$nbclust,graph=F)
        },      
        
        .printeigenTable = function(table){

            for (i in 1:dim(table$eig)[1]){
                self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i)))
            } #on cree les lignes du tableau, avec autant de lignes qu'il y a de dimensions

            eigen=table$eig[,1]
            purcent=table$eig[,2]
            purcentcum=table$eig[,3]

            for (i in seq_along(eigen)) {
                row=list()
                row[["component"]]=paste("Dim.",i)
                row[["eigenvalue"]]=eigen[i] #   a chaque nom de colonne (eigenvalue, purcent et purcentcum)
                row[["purcent"]]=purcent[i] #    on associe
                row[["purcentcum"]]=purcentcum[i] #  une valeur des calculs precedents
                self$results$eigengroup$eigen$setRow(rowNo=i, values = row)
            }

        },

        .printTables = function(table, quoi){

            actvars_gui=self$options$actvars
            nFactors_gui=self$options$nFactors
            nFactors_out <- min(self$options$ncp,dim(self$MCAResult$eig)[1])

            if (is.null(self$options$individus)==FALSE)
                individus_gui=self$data[[self$options$individus]]
            else
                individus_gui=c(1:nrow(self$data))

            if (quoi=="coord") {
                quoivar=table$var$coord
                quoiind=table$ind$coord
                tablevar=self$results$variables$coordonnees
                tableind=self$results$individus$coordonnees
            }

            else if (quoi=="contrib") {
                quoivar=table$var$contrib
                quoiind=table$ind$contrib
                tablevar=self$results$variables$contribution
                tableind=self$results$individus$contribution
            }

            else if (quoi=="cos2") {
                quoivar=table$var$cos2
                quoiind=table$ind$cos2
                tablevar=self$results$variables$cosinus
                tableind=self$results$individus$cosinus
            }

            tableind$addColumn(name="individus", title="", type="text")
            for (i in seq(nrow(quoiind)))
                tableind$addRow(rowKey=i, value=NULL)

            tablevar$addColumn(name="variables", title="", type="text")
            for (i in seq(nrow(quoivar)))
                tablevar$addRow(rowKey=i, value=NULL)

            for (i in 1:nFactors_out){
                tablevar$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number') #, superTitle='Facteurs'
                tableind$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number')
            }

            for (var in seq(nrow(quoivar))) {
                row=list()
                row[["variables"]]=rownames(quoivar)[var]
                for (i in 1:nFactors_out) {
                    row[[paste0("dim",i)]]=quoivar[var,i]
                }
                tablevar$setRow(rowNo=var, values=row) #on remplie le tableau en reprenant les r?sultats de results$var$coord
            }

            for (ind in 1:length(individus_gui)) {
                row=list()
                if (is.null(self$options$individus))
                    row[["individus"]]= individus_gui[ind]
                else
                    row[["individus"]]= rownames(quoiind)[ind]
                for (i in 1:nFactors_out){
                    row[[paste0("dim",i)]]=quoiind[ind,i]
                }
                tableind$setRow(rowNo=ind, values=row) #on remplie le tableau en reprenant les r?sultats de results$var$coord
            }

        },

        .plotitemvar = function(image, ...) {

            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors) return()

            else {

                res.mca=image$state
                abs=self$options$abs
                ord=self$options$ord
                modqualisup_gui=self$options$varmodqualisup
                modvar_gui=self$options$varmodvar
                modality_gui=self$options$modality

                if (modqualisup_gui==TRUE && modvar_gui==TRUE)
                plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind"), selectMod = paste(modality_gui), title="Representation of the Active and Supplementary Categories")

                else if (modqualisup_gui==TRUE && modvar_gui==FALSE)
                plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind", "var"), selectMod = paste(modality_gui), title="Representation of the Supplementary Categories")

                else if (modqualisup_gui==FALSE && modvar_gui==TRUE)
                plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind", "quali.sup"), selectMod = paste(modality_gui),title = "Representation of the Active Categories")

                else
                plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind", "var", "quali.sup"), selectMod = paste(modality_gui), title="Representation of the Descriptions")

                print(plot)
                TRUE

            }
        },

        .plotindiv = function(image, ...) {

            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors) return()

            else {

                res.mca=image$state
                abs=self$options$abs
                ord=self$options$ord
                # qualisup = self$options$varmodqualisup

                plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("var","quali.sup", "quanti.sup"),title = "Representation of the Stimuli")

                print(plot)
                TRUE
            }
        },

        .plotvar = function(image, ...) {

            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors) return()

            else {

                res.mca=image$state
                abs=self$options$abs
                ord=self$options$ord
                qualisup = self$options$varmodqualisup

                plot=plot.MCA(res.mca, axes=c(abs, ord), choix="var", title = "Representation of the Subjects")

                print(plot)
                TRUE
            }
        },

        .populateDFTable= function(tab){

            for (i in 1:dim(tab)[1]){
                self$results$dfresgroup$dfres$addRow(rowKey=i, values=list(component=as.character(tab[,1])[i])) #M?thode addRow
            }

            for (i in 1:(dim(tab)[1])) {
                row=list()
                row[["word"]]=as.character(tab[,2])[i]
                row[["internper"]]=tab[,3][i]
                row[["globper"]]=tab[,4][i]
                row[["internfreq"]]=tab[,5][i]
                row[["globfreq"]]=tab[,6][i]
                row[["pvaluedfres"]]=tab[,7][i]
                row[["vtest"]]=tab[,8][i]
                self$results$dfresgroup$dfres$setRow(rowNo=i, values = row) #Methode setRow
            }

        },

        .plotquantisup = function(image, ...) {

            if (is.null(self$options$actvars)||is.null(self$options$quantisup)||length(self$options$actvars) < self$options$nFactors) image$setState(NULL)

            else {

                res.mca=image$state
                abs=self$options$abs
                ord=self$options$ord

                plot=plot.MCA(res.mca, axes=c(abs, ord), choix="quanti.sup", title = "Representation of the Quantitative Supplementary Variables")

                print(plot)
                TRUE
            }
        },

        .plotclassif= function(image, ...){

        if (is.null(self$options$actvars)) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          res.classif=image$state
          plot=FactoMineR::plot.HCPC(res.classif, axes=c(abs_gui, ord_gui), choice="map", draw.tree = F, title="Representation of the Stimuli According to Clusters")
          print(plot)
          TRUE
        }
        },

        .output = function(res.mca) {
        nFactors_out <- min(self$options$ncp,dim(self$MCAResult$eig)[1])
        if (self$results$newvar$isNotFilled()) {
          keys <- 1:nFactors_out
          measureTypes <- rep("continuous", nFactors_out)
          titles <- paste(("Dim."), keys)
          descriptions <- character(length(keys))
          self$results$newvar$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
          for (i in 1:nFactors_out) {
            scores <- as.numeric(res.mca$ind$coord[, i])
            self$results$newvar$setValues(index=i, scores)
          }
          self$results$newvar$setRowNums(rownames(self$data))
        }
      },

      .output2 = function(res.classif){
        if (self$results$newvar2$isFilled()) {
          keys <- 1
          measureTypes <- "nominal"
          titles <- "Cluster"
          descriptions <- "Cluster variable"
          self$results$newvar2$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
            scores <- as.factor(res.classif$data.clust[,dim(res.classif$data.clust)[2]])
            self$results$newvar2$setValues(index=1, scores)

          self$results$newvar2$setRowNums(rownames(self$data))
        }
      },        

        .buildData = function() {
            
            dataactvars=data.frame(self$data[,self$options$actvars])
            colnames(dataactvars)=self$options$actvars


            dataqualisup=data.frame(self$data[,self$options$qualisup])
            colnames(dataqualisup)=self$options$qualisup

            data=data.frame(dataactvars,dataqualisup)
            
            if (is.null(self$options$individus)==FALSE) {
                rownames(data)=self$data[[self$options$individus]]
            }
            else {
                rownames(data)=c(1:nrow(data))
            }
                
            #creation jeu de donnees textuel
            c1 <- rownames(data)
            c2 <- as.vector(data[,1])
            j <- 0
            for (j in 1:(dim(data)[2]-1)){
                j <- j+1
                c1 <- c(c1,rownames(data))
                c2 <- c(c2, as.vector(data[,j]))
            }        
            datatext <- as.data.frame(cbind(c1,c2))
            
            return(list(data,datatext))
        }
    )
)