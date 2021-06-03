
# This file is a generated template, your changes will not be overwritten

QDAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "QDAClass",
    inherit = QDABase,
    private = list(
        .run = function() {
            
            if (is.null(self$options$sensoatt)) return()
            
            else if (is.null(self$options$prodeff)) return()
            
            else if (is.null(self$options$paneff)) return()
            
            else{
                
                #Division blocs
                
                data <- self$data
                data.qt <- data[,sapply(data, function(x) is.numeric(x))]
                data.ql <- data[,sapply(data, function(x) is.factor(x))]
                data <- data.frame(data.ql, data.qt) #Ranger le jeu de données avec d'abord les var QL puis les var QT
                
                res.decat <- private$.decat(data)
                
                #Mise en place du tableau sous R
                
                if (is.null(dim(res.decat$resT[[1]])[1]) == FALSE) {
                    
                    j=1
                    while (dim(res.decat$resT[[j]])[1] == 0)
                        j=j+1 #j est le rang du premier data.frame de res.decat$resT dont le nombre de lignes n'est pas nul
                    
                    tab=cbind(names(res.decat$resT)[j],res.decat$resT[[j]])
                    A <- as.vector(rownames(tab)) #A contient les noms des descipteurs de resT$produit_1
                    tab[,6] <- as.factor(A)
                    colnames(tab)[1]="Product"
                    
                    for (i in (j+1):length(res.decat$resT)) {
                        if (dim(res.decat$resT[[i]])[1] != 0) { #On ne rajoute au tableau final que les data.frame dont le nombre de lignes n'est pas nul
                            pretab=cbind(names(res.decat$resT)[i],res.decat$resT[[i]])
                            colnames(pretab)[1]="Product"
                            A <- as.vector(rownames(pretab)) #A contient les noms des descipteurs de resT$produit_i
                            pretab[,6] <-as.factor(A)
                            tab=rbind(tab,pretab)
                        }
                    }
                    
                    rownames(tab) <- c()
                    tab <- tab[, c(1,6,2,3,4,5)] #Ordonner les colonnes
                    colnames(tab)[2]="Descriptor"
                    
                    if (length(self$options$sensoatt) == 1) #Afficher le nom du descripteur ? la place de "1" quand un seul descripteur est choisi
                        tab[,2]=self$options$sensoatt
                }
                
                
                #Resultats
                
                if (is.null(res.decat$resT) == FALSE) {
                    private$.printresTable(tab)
                    private$.printresFTable(res.decat$resF)
                }
            }
        },
        
        .decat= function(data){
            
            threshold=self$options$threshold/100
            prodeff=self$options$prodeff
            paneff=self$options$paneff
            formul=paste0("~", prodeff, "+", paneff)
            firstvar=3
            lastvar=ncol(data)
            proba=threshold
            nbrow = NULL 
            nbcol = NULL 
            random = TRUE
            donnee <- data
            
            # old.par <- par(no.readonly = TRUE)
            # old.contr = options()$contrasts
            options(contrasts = c("contr.sum", "contr.sum"))
            
            for (j in 1 :(firstvar-1)) donnee[,j] <- as.factor(donnee[,j])
            level.lower = -qnorm(proba/2)
            # formul = as.formula(formul)
            formul = as.formula(paste(formul, collapse = " "))
            lab.sauv <- lab <- colnames(donnee)
            for (i in 1:length(lab)) lab[i]=gsub(" ",".",lab[i])
            colnames(donnee) = lab
            equation <- as.character(formul)
            # Terms=attr(terms(as.formula(equation)),"term.labels")
            Terms=attr(terms(as.formula(paste(equation, collapse = " "))),"term.labels")
            equation = paste("~",Terms[1])
            if (length(Terms) > 1) for (i in 2:length(Terms)) equation <- paste(equation,"+",Terms[i])
            #  equation <- as.character(as.formula(equation))
            equation <- as.character(as.formula(paste(equation, collapse = " ")))
            
            dim.donnee <- dim(donnee)[2]
            
            if (length(strsplit(equation,split="+",fixed=TRUE)[[2]]) == 1) random = FALSE # if there is 1 effect, there is not random effect
            for (i in 1:dim.donnee) {
                if (gsub(" ","",strsplit(equation,split="+",fixed=TRUE)[[2]][1])==lab[i]) col.p <- i
                if (random){
                    if (gsub(" ","",strsplit(equation,split="+",fixed=TRUE)[[2]][2])==lab[i]) col.j <- i
                }
            }
            nb.modalite <- nlevels(donnee[,col.p])
            don.aux <- cbind.data.frame(donnee,fac=ordered(donnee[,col.p],rev(levels(donnee[,col.p]))))
            dim.don.aux <- dim(don.aux)[2]
            don.aux[,col.p] <- as.factor(don.aux[,dim.don.aux])
            tabF <- matrix(0,lastvar+1-firstvar,2)
            adjmean <- coeff <- tabT <- matrix(0,lastvar+1-firstvar,nb.modalite)
            lab2 <- labels(don.aux)[[2]]
            for (varendo in firstvar:lastvar) {
                formule <- paste(lab[varendo],"~",equation[2])
                # formule <- as.formula(formule)
                formule <- as.formula(paste(formule, collapse = " "))
                res <- summary(aov( formule , data = donnee, na.action =na.exclude))[[1]]
                
                nrow.facteur=nrow(res)
                if (random) {
                    panelist=colnames(donnee)[col.j]
                    product = colnames(donnee)[col.p]
                    for (i in 3:length(Terms)){                ## 1 is product, 2 is panelist
                        if ((any(grep(product,Terms[i])))&(any(grep(":",Terms[i])))&(any(grep(panelist,Terms[i])))) nrow.facteur = i
                    }
                }
                
                ##    tabF[varendo-firstvar+1,1] <- -qnorm(pf(res[1,4],res[1,1],res[dim(res)[1],1],lower.tail=FALSE))
                ##    tabF[varendo-firstvar+1,2] <-        pf(res[1,4],res[1,1],res[dim(res)[1],1],lower.tail=FALSE)
                tabF[varendo-firstvar+1,1] <- -qnorm(pf(res[1,3]/res[nrow.facteur,3],res[1,1],res[nrow.facteur,1],lower.tail=FALSE))
                tabF[varendo-firstvar+1,2] <-        pf(res[1,3]/res[nrow.facteur,3],res[1,1],res[nrow.facteur,1],lower.tail=FALSE)
                res2 <- summary.lm(aov( formule , data = donnee, na.action =na.exclude))$coef[1:nb.modalite,]
                moy <- res2[1,1]
                res2 <- res2[-1,]
                if (nb.modalite >2){
                    ##      tabT[varendo-firstvar+1,1:(nb.modalite-1)] <-  -qnorm(( pf(res2[,3]^2,1,res[(dim(res)[[1]]),1],lower.tail=FALSE) )/2)*(res2[,1]/abs(res2[,1]))
                    tabT[varendo-firstvar+1,1:(nb.modalite-1)] <- -qnorm((pf(res2[,3]^2*(res[nrow(res),3]/res[nrow.facteur,3]) ,1,res[nrow.facteur,1],lower.tail=FALSE) )/2)*sign(res2[,1])
                    coeff[varendo-firstvar+1,1:(nb.modalite-1)] <-  res2[,1]
                }
                if (nb.modalite ==2){
                    ##      tabT[varendo-firstvar+1,1:(nb.modalite-1)] <-  -qnorm(( pf(res2[3]^2,1,res[(dim(res)[[1]]),1],lower.tail=FALSE) )/2)*(res2[1]/abs(res2[1]))
                    tabT[varendo-firstvar+1,1:(nb.modalite-1)] <- -qnorm((pf(res2[3]^2*(res[nrow(res),3]/res[nrow.facteur,3]) ,1,res[nrow.facteur,1],lower.tail=FALSE) )/2)*sign(res2[1])
                    coeff[varendo-firstvar+1,1:(nb.modalite-1)] <-  res2[1]
                }
                res2 <- summary.lm(aov( formule , data = don.aux, na.action =na.exclude))$coef[2,]
                ##    tabT[varendo-firstvar+1,nb.modalite] <-  -qnorm(( pf(res2[3]^2,1,res[(dim(res)[[1]]),1],lower.tail=FALSE) )/2)*(res2[1]/abs(res2[1]))
                tabT[varendo-firstvar+1,nb.modalite] <- -qnorm((pf(res2[3]^2*(res[nrow(res),3]/res[nrow.facteur,3]) ,1,res[nrow.facteur,1],lower.tail=FALSE) )/2)*sign(res2[1])
                coeff[varendo-firstvar+1,nb.modalite] <-  res2[1]
                adjmean[varendo-firstvar+1,] <- moy+coeff[varendo-firstvar+1,]
            }
            nomdescripteur <- lab.sauv[firstvar:lastvar]
            dimnames(tabF) <- list(nomdescripteur,c("Vtest","P-value"))
            dimnames(adjmean) <-   dimnames(coeff) <- dimnames(tabT) <- list(nomdescripteur,levels(donnee[,col.p]))
            resF <- vector("list",length=1)
            select1 <- (1:nrow(tabF))[tabF[order(tabF[,2]),2]< proba]
            if (length(select1) >0 ){
                resF <- cbind.data.frame(qnorm(tabF[order(tabF[,2]),2],lower.tail=FALSE)[select1],tabF[order(tabF[,2]),2][select1])
                dimnames(resF)[[2]]=c("Vtest","P-value")
                resT <- vector("list",length=nb.modalite)
                for (i in 1:nb.modalite) {
                    select <- (1:nrow(tabT))[abs(tabT[rev(order(tabT[,i])),i])>=level.lower]
                    resT[[i]] <- cbind.data.frame(coeff[rev(order(tabT[,i])),i][select],adjmean[rev(order(tabT[,i])),i][select],2*(pnorm(-abs(tabT[rev(order(tabT[,i])),i][select]))),tabT[rev(order(tabT[,i])),i][select])
                    dimnames(resT[[i]])[[2]]=c("Coeff","Adjust mean","P-value","Vtest")
                }
                names(resT) = c(levels(donnee[,col.p]))
            }
            
            result = list() 
            result$tabF = tabF
            result$tabT = t(tabT)
            result$coeff = t(coeff)
            result$adjmean = t(adjmean)
            if (length(select1) > 0){
                result$resF = resF
                result$resT = resT
            }
            
            if (length(select1) == 0) print("Warning: No variables are discriminant")
            # options(contrasts = old.contr)
            return(result)
            
        },

        .printresTable= function(tab){
            
            for (i in 1:dim(tab)[1]){
                self$results$resT$addRow(rowKey=i, values=list(component=as.character(tab[,1])[i])) #M?thode addRow
            }
            
            for (i in 1:(dim(tab)[1])) {
                row=list()
                row[["var"]]=as.character(tab[,2])[i]
                row[["coeff"]]=tab[,3][i]
                row[["adjmean"]]=tab[,4][i]
                row[["pvalue"]]=tab[,5][i]
                row[["vtest"]]=tab[,6][i]
                self$results$resT$setRow(rowNo=i, values = row) #M?thode setRow
            }
        },
        
        .printresFTable = function(tab) {
            
            resF <- self$results$resF
            
            for (i in 1:dim(tab)[1]) {
                resF$addRow(rowKey=i, value=NULL)  
                row=list()
                row[["att"]] = row.names(tab)[i]
                row[["vtest"]] = tab[,1][i]
                row[["pvalue"]] = tab[,2][i]
                resF$setRow(rowKey=i, values = row)
            }
        }
    )
)