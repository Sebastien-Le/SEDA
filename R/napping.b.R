NappingClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "NappingClass",
    inherit = NappingBase,

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

      classifResult = function() {
        if (is.null(private$.classifResult))
        private$.classifResult <- private$.getclassifResult()
        return(private$.classifResult)
      },

      NappingResult = function() {
        if (is.null(private$.NappingResult))
        private$.NappingResult <- private$.getNappingResult()
        return(private$.NappingResult)
      }
    ),

    private = list(
        .dataProcessed = NULL,
        .nbclust = NULL,
        .nQualsup = NULL,
        .classifResult = NULL,
        .NappingResult = NULL,

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
            <p><b>What you need to know before analysing Napping data in jamovi</b></p>
            <p>______________________________________________________________________________</p>
            <p> Napping data is a type of data used in sensory analysis to understand the sensory perception 
            of stimuli, from a global/holistic point of view, regardless of any <I>a priori</I> list of sensory attributes.
            It was introduced by Jérôme Pagès in 2003.</p>

            <p> The napping data collection method involves presenting a set of stimuli simultaneously to a group of panelists. 
            Each panelist is asked to position the stimuli in a two-dimensional space based on their perceived similarities and differences. 
            The panelists are typically provided with a sheet or a table where they can physically place the stimuli 
            (<I>e.g.</I>, product samples, images, or other sensory samples) on the two-dimensional space.</p>
            
            <p> The two dimensions used in napping are typically not labeled but correspond to perceptual qualities that the panelists perceive to differentiate the stimuli. 
            The distances and positions of the stimuli on the two-dimensional space reflect the perceived similarities and differences between them. 
            Stimuli that are closer together are perceived as more similar, while those farther apart are perceived as more different.</p>

            <p> Open the <b>perfumes_napping</b> dataset. Choose the Ident variable as <I>Stimuli Labels</I>. Put as many <B>pairs</B> of (X,Y) coordinates in the first 
            block. This is essential to obtain a consensual representation of the stimuli. You can also add <B>pairs</B> of supplementary (X,Y) coordinates. This last feature is optional.</p>
            
            <p> <B>Warning</B>: an error message will appear if the number of coordinates is odd.</p>
            
            <p> Clustering is based on the number of components saved. 
            By default, clustering is based on the first 5 components, 
            <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>

            <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of clusters is automatically chosen by the computer.</p>

            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },

        .run = function() {

            ready <- TRUE

            #private$.errorCheck()
            #if (length(self$options$actvars)<3 || length(self$options$qualisup)%%2!=0){
            #if (length(self$options$actvars)<3 || length(self$options$actvars)%%2!=0){
            if (length(self$options$actvars)<3 || (length(self$options$actvars)%%2!=0 || length(self$options$qualisup)%%2!=0)){
                #return()
                ready <- FALSE
                }

            private$.errorCheck()

            if (ready) {
                
            res.classif <- private$.getclassifResult()

            private$.printeigenTable()
            
            dimdesc=private$.dimdesc()
            self$results$descdesdim$setContent(dimdesc)

            imagevar=self$results$plotgroup
            imagevar$setState(self$NappingResult)

            imagevar=self$results$plotind
            imagevar$setState(self$NappingResult)

            if (self$options$graphclassif==TRUE){
                imageclass = self$results$plotclassif
                imageclass$setState(res.classif)
            }

            private$.output()
            private$.output2(res.classif)

            }
        },

        .computeNbclust = function() {
          nbclust <- self$options$nbclust
          return(nbclust)
        },

        .computeNQualsup = function() {
          nQualsup <- length(self$options$qualisup)
          return(nQualsup)
        },

        .getclassifResult = function() {
            reshcpc <- FactoMineR::HCPC(self$NappingResult,nb.clust=self$nbclust,graph=F)
            private$.classifResult <- reshcpc
            return(private$.classifResult)
        },        

        .getNappingResult= function() {
            data <- self$dataProcessed
            nbgroups <- length(self$options$actvars)/2
            nbgroupsup <- length(self$options$qualisup)/2
            nbg <- (nbgroups+nbgroupsup)

            #no supplementary coordinates
            if (is.null(self$options$qualisup) == TRUE){
            r <- FactoMineR::MFA(data,group=c(rep(2,nbgroups)),type=rep("c",nbgroups),ncp=5,name.group=c(paste("S",1:nbgroups,sep="")),graph=FALSE)
            }
            #with supplementary coordinates
            else{
            r <- FactoMineR::MFA(data,group=c(rep(2,nbg)),type=rep("c",nbg),num.group.sup=(nbgroups+1):nbg,ncp=5,name.group=c(paste("S",1:nbg,sep="")),graph=FALSE)    
            }

            private$.NappingResult <- r
            return(private$.NappingResult)
        },

        .dimdesc = function() {

            table <- self$NappingResult
            proba=self$options$proba/100
            nFactors=self$options$nFactors

            res.dimdesc<-dimdesc(table, axes=1:nFactors, proba = proba)
            res.dimdesc$call<-NULL
            return(res.dimdesc)
        },

        .printeigenTable = function(){

            table <- self$NappingResult

            for (i in 1:dim(table$eig)[1]){
                self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i)))
            }
            eigen=table$eig[,1]
            purcent=table$eig[,2]
            purcentcum=table$eig[,3]

            for (i in seq_along(eigen)) {
                row=list()
                row[["component"]]=paste("Dim.",i)
                row[["eigenvalue"]]=eigen[i]
                row[["purcent"]]=purcent[i]
                row[["purcentcum"]]=purcentcum[i]
                self$results$eigengroup$eigen$setRow(rowNo=i, values = row)
            }
        },

        .printTables = function(table, quoi){

            actvars_gui=self$options$actvars
            qtsup_gui=self$options$quantisup
            nFactors_out <- min(self$options$ncp,dim(self$NappingResult$eig)[1])

            nFactors_out <- min(self$options$ncp,dim(self$NappingResult$eig)[1])
            
            if (is.null(self$options$individus)==FALSE)
                individus_gui=self$data[[self$options$individus]]
            else
                individus_gui=c(1:nrow(self$data))

            if (quoi=="coord") {
                quoivar=as.data.frame(rbind(table$quanti.var$cor,table$quanti.var.sup$cor))
                quoiind=table$ind$coord
                tablevar=self$results$variables$correlations
                tableind=self$results$individus$coordonnees
            }

            else if (quoi=="contrib") {
                quoivar=table$quanti.var$contrib
                quoiind=table$ind$contrib
                tablevar=self$results$variables$contribution
                tableind=self$results$individus$contribution
            }

            else if (quoi=="cos2") {
                quoivar=as.data.frame(rbind(table$quanti.var$cos2,table$quanti.var.sup$cos2))
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

            if (quoi=="coord"||quoi=="cos2"){
                for (var in 1:(length(actvars_gui)+length(qtsup_gui))) {
                    row=list()
                    row[["variables"]]=rownames(quoivar)[var]
                    for (i in 1:nFactors_out) {
                        row[[paste0("dim",i)]]=quoivar[var,i]
                    }
                    tablevar$setRow(rowNo=var, values=row)
                }
            }
            else{
                for (var in seq_along(actvars_gui)) {
                    row=list()
                    row[["variables"]]=rownames(quoivar)[var]
                    for (i in 1:nFactors_out) {
                        row[[paste0("dim",i)]]=quoivar[var,i]
                    }
                    tablevar$setRow(rowNo=var, values=row)
                }

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
                tableind$setRow(rowNo=ind, values=row)
            }

        },

        .plotindividus = function(image, ...){
            
            nbgroups<-length(self$options$actvars)/2
            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors||(nbgroups*2)%%2!=0) return()

            else {
                res.napping=image$state
                abs_gui=self$options$abs
                ord_gui=self$options$ord

                plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui), choix="ind", habillage = "none", title = "Representation of the Stimuli")

                print(plot)
                TRUE

            }
        },

        .plotvariables = function(image, ...){
            
            nbgroups<-length(self$options$actvars)/2
            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors||(nbgroups*2)%%2!=0) image$setState(NULL)
            else {
                res.napping=image$state
                abs_gui=self$options$abs
                ord_gui=self$options$ord
                limcosvar_gui=self$options$limcosvar/100
                # test if there are a variables which sum of their cosinus across the dimension abs_gui and ord_gui is superior to limcosvar_gui
                limcostest=sum(apply(res.napping$quanti.var$cos2[,c(abs_gui,ord_gui)],1,sum)>limcosvar_gui,ifelse(is.null(res.napping$quanti.var.sup),0,apply(res.napping$quanti.var.sup$cos2[,c(abs_gui,ord_gui)],1,sum)>limcosvar_gui))
                
                if (limcostest>0) {
                    ploti=FactoMineR::plot.MFA(res.napping, choix="var", axes=c(abs_gui, ord_gui),habillage="group", lim.cos2.var = limcosvar_gui,graph.type = "ggplot" )
                    print(ploti)
                    TRUE
                }
                else image$setState(NULL)
            }
        },

        .plotgroups = function(image, ...){
            
            nbgroups<-length(self$options$actvars)/2
            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors||(nbgroups*2)%%2!=0) return()
            else {
                res.napping=image$state
                abs_gui=self$options$abs
                ord_gui=self$options$ord
                
                #plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui),choix="group",habillage="none", col.hab=c(rep("black",nbgroups)), title = "Representation of the Subjects")
                plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui),choix="group",habillage="none", title = "Representation of the Subjects")

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

        .output = function(){
        nFactors_out <- min(self$options$ncp,dim(self$NappingResult$eig)[1])
        res.mfa <- self$NappingResult
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
            scores <- as.numeric(res.mfa$ind$coord[, i])
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
          scores <- as.factor(res.classif$data.clust[rownames(private$.buildData()),dim(res.classif$data.clust)[2]])
          self$results$newvar2$setValues(index=1, scores)

          self$results$newvar2$setRowNums(rownames(self$data))
        }
      },

        .errorCheck = function() {

            if (length(self$options$actvars)>3 && (length(self$options$actvars)%%2!=0 || length(self$options$qualisup)%%2!=0))
            jmvcore::reject(jmvcore::format('The number of coordinates should be even'))
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
            return(data)
        }
    )
)