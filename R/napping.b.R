
# This file is a generated template, your changes will not be overwritten

NappingClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "NappingClass",
    inherit = NappingBase,
    private = list(
        .run = function() {
            
            nbgroups<-length(self$options$actvars)/2
            
            if (is.null(self$options$actvars)) return()
            
            else if (length(self$options$actvars) < self$options$nFactors) {
                jmvcore::reject("The number of factors is too low")
                return()
            }
                
            
            else if ((nbgroups*2)%%2!=0) {
                jmvcore::reject("Number of active variables must be an even number")
                return()
            }
            
            else {
                
                # Preparing data
                data <- private$.buildData()
                datamfa <- data[[1]]
                dataquantisup <- data[[2]] 
                dataqualisup <- data[[3]]
                
                res.napping=private$.mfa(data = datamfa,nbgr = nbgroups, qtsup=dataquantisup,qlsup = dataqualisup)

                # Results
                private$.printeigenTable(res.napping)

                private$.printTables(res.napping, "coord")
                private$.printTables(res.napping, "contrib")
                private$.printTables(res.napping, "cos2")


                imageind=self$results$plotind
                imageind$setState(res.napping)

                imagevar=self$results$plotvar
                imagevar$setState(res.napping)

                imageind2=self$results$plotind2
                imageind2$setState(res.napping)

                imagevar2=self$results$plotvar2
                imagevar2$setState(res.napping)

                imageind3=self$results$plotind3
                imageind3$setState(res.napping)

                imagevar3=self$results$plotvar3
                imagevar3$setState(res.napping)

                imagegrp=self$results$plotgrp
                imagegrp$setState(res.napping)

                dimdesc=private$.dimdesc(res.napping)
                self$results$descdesdim$setContent(dimdesc)

            }
        },
        .mfa = function(data, nbgr, qtsup=NULL,qlsup=NULL){

            actvars_gui=self$options$actvars
            quantisup_gui=self$options$quantisup
            qualisup_gui=self$options$qualisup
            nFactors_gui=self$options$nFactors

            if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui)== TRUE){
                FactoMineR::MFA(data, group = c(rep(2,nbgr),ncol(qtsup)),type=rep("c",nbgr+1), num.group.sup=(nbgr+1), ncp=nFactors_gui,name.group=c(paste("S",1:nbgr,sep=""),"QSV"),graph=FALSE)
            }
            else if (is.null(quantisup_gui)==TRUE && is.null(qualisup_gui) == FALSE){
                FactoMineR::MFA(data, group = c(rep(2,nbgr),ncol(qlsup)),type=c(rep("c",nbgr),"n"), num.group.sup=(nbgr+1), ncp=nFactors_gui,name.group=c(paste("S",1:nbgr,sep=""),"CSV"),graph=FALSE)
            }
            else if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui) == FALSE){
                FactoMineR::MFA(data, group = c(rep(2,nbgr), ncol(qtsup), ncol(qlsup)),type=c(rep("c",nbgr+1),"n"), num.group.sup=c(nbgr+1, nbgr+2),ncp=nFactors_gui, name.group=c(paste("S",1:nbgr,sep=""),"QSV", "CSV"),graph=FALSE)
            }
            else{
                FactoMineR::MFA(data,group=c(rep(2,nbgr)),type=rep("c",nbgr),ncp=nFactors_gui,name.group=c(paste("S",1:nbgr,sep="")),graph=FALSE)
            }
        },

        .dimdesc = function(table) {

            proba=self$options$proba/100
            nFactors=self$options$nFactors

            res.dimdesc<-dimdesc(table, axes=1:nFactors, proba = proba)
            res.dimdesc$call<-NULL
            return(res.dimdesc)
        },
        .printeigenTable = function(table){

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
            
            nFactors_gui=self$options$nFactors
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

            for (i in 1:nFactors_gui){
                tablevar$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number') #, superTitle='Facteurs'
                tableind$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number')
            }

            if (quoi=="coord"||quoi=="cos2"){
                for (var in 1:(length(actvars_gui)+length(qtsup_gui))) {
                    row=list()
                    row[["variables"]]=rownames(quoivar)[var]
                    for (i in 1:nFactors_gui) {
                        row[[paste0("dim",i)]]=quoivar[var,i]
                    }
                    tablevar$setRow(rowNo=var, values=row)
                }
            }
            else{
                for (var in seq_along(actvars_gui)) {
                    row=list()
                    row[["variables"]]=rownames(quoivar)[var]
                    for (i in 1:nFactors_gui) {
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
                for (i in 1:nFactors_gui){
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
        .plotindividus2 = function(image, ...){
            
            nbgroups<-length(self$options$actvars)/2
            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors||(nbgroups*2)%%2!=0) return()
            else {
                res.napping=image$state
                abs_gui=self$options$abs
                ord_gui=self$options$ord

                plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui), choix="ind", invisible="quali.sup",habillage = "none", title = "Representation of the Stimuli")

                print(plot)
                TRUE

            }
        },
        .plotindividus3 = function(image, ...){
            
            nbgroups<-length(self$options$actvars)/2
            if (is.null(self$options$actvars)||is.null(self$options$qualisup)||length(self$options$actvars) < self$options$nFactors||(nbgroups*2)%%2!=0) image$setState(NULL)
            else {
                res.napping=image$state
                abs_gui=self$options$abs
                ord_gui=self$options$ord
                plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui), choix="ind", invisible="ind",habillage = "none", title = "Representation of the Categorical Supplementary Variables")
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
        .plotvariables2 = function(image, ...){
            
            nbgroups<-length(self$options$actvars)/2
            if (is.null(self$options$actvars)||length(self$options$actvars) < self$options$nFactors||(nbgroups*2)%%2!=0) image$setState(NULL)
            else {
                res.napping=image$state
                abs_gui=self$options$abs
                ord_gui=self$options$ord
                limcosvar_gui=self$options$limcosvar/100
                limcostest=sum(apply(res.napping$quanti.var$cos2[,c(abs_gui,ord_gui)],1,sum)>limcosvar_gui)
               
                if (limcostest>0) {
                    plota=FactoMineR::plot.MFA(res.napping, choix="var", invisible="quanti.sup", axes=c(abs_gui, ord_gui),habillage="group",lim.cos2.var = limcosvar_gui )
                    print(plota)
                    TRUE
                }
                else image$setState(NULL)
            }
        },
        .plotvariables3 = function(image, ...){
            
            nbgroups<-length(self$options$actvars)/2
            if (is.null(self$options$actvars)||is.null(self$options$quantisup)||length(self$options$actvars) < self$options$nFactors||(nbgroups*2)%%2!=0) image$setState(NULL)
            else {
                res.napping=image$state
                abs_gui=self$options$abs
                ord_gui=self$options$ord
                limcosvar_gui=self$options$limcosvar/100
                limcostest=sum(apply(res.napping$quanti.var.sup$cos2[,c(abs_gui,ord_gui)],1,sum)>limcosvar_gui)
                
                if (limcostest>0) {
                    plot=FactoMineR::plot.MFA(res.napping, choix="var",invisible="quanti", axes=c(abs_gui, ord_gui),habillage="group", lim.cos2.var = limcosvar_gui,graph.type = "ggplot" )
                    print(plot)
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
                nbgr<-length(self$options$actvars)/2
                quantisup_gui=self$options$quantisup
                qualisup_gui=self$options$qualisup

                if (is.null(self$options$quantisup)== FALSE && is.null(qualisup_gui)== TRUE){
                    plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui),choix="group",habillage="none", col.hab=c(rep("black",nbgr),"red"), title = "Representation of the Subjects")
                }
                else if (is.null(quantisup_gui) == TRUE && is.null(qualisup_gui)== FALSE){
                    plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui),choix="group",habillage="none", col.hab=c(rep("black",nbgr),"green"), title = "Representation of the Subjects")
                }
                else if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui)== FALSE){
                    plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui),choix="group",habillage="none", col.hab=c(rep("black",nbgr),"red","green"), title = "Representation of the Subjects")
                }
                else {
                    plot=FactoMineR::plot.MFA(res.napping,axes=c(abs_gui, ord_gui),choix="group",habillage="none", col.hab=c(rep("black",nbgr)), title = "Representation of the Subjects")

                }
                print(plot)
                TRUE
            }
        },

        .buildData = function() {

            dataactvars=data.frame(self$data[,self$options$actvars])
            colnames(dataactvars)=self$options$actvars

            dataquantisup=data.frame(self$data[,self$options$quantisup])
            colnames(dataquantisup)=self$options$quantisup

            dataqualisup=data.frame(self$data[,self$options$qualisup])
            colnames(dataqualisup)=self$options$qualisup

            datamfa=data.frame(dataactvars,dataquantisup,dataqualisup)

            if (is.null(self$options$individus)==FALSE) {
                rownames(datamfa)=self$data[[self$options$individus]]
            }
            else {
                rownames(datamfa)=c(1:nrow(datamfa))
            }
            return(list(datamfa,dataquantisup, dataqualisup))
        }
    )
)