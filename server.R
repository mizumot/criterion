library(shiny)
library(shinyAce)
library(psych)
library(CTT)
library(lattice)
library(latticeExtra)
library(beeswarm)



shinyServer(function(input, output, clientData, session) {

  observe({



########################################
# Difference Index (pre-post test)
########################################


    # Basic stats ==========================================================
    prepost.ds <- reactive({
        
        
        if (input$rowname == 1) {

            x <- read.csv(text=input$pretest, sep="\t")
            x <- x[,-1]

            y <- read.csv(text=input$posttest, sep="\t")
            y <- y[,-1]
            
        } else {
            
            x <- read.csv(text=input$pretest, sep="\t")
            y <- read.csv(text=input$posttest, sep="\t")

        }
        
        brownRpbi <- function(data,missing) {
            m <- mean(rowSums(data))
            sd <- sd(rowSums(data))
            totalDat <- cbind(data,rowSums(data))
            sortDat <- totalDat[order(-totalDat[,length(totalDat)]),]
            pbi <- c()
            r <- c()
            itemD <- c()
            rownames(sortDat) <- c(1:nrow(sortDat))
            highDat <- head(sortDat,nrow(sortDat) %/% 3)
            lowDat <- tail(sortDat,nrow(sortDat) %/% 3)
            for (i in 1:length(data)) {
                if (is.element(colnames(data)[i], missing) == F ) {
                    mhigh <- mean(subset(totalDat[,length(totalDat)],(data[,i] == 1)))
                    mlow <- mean(subset(totalDat[,length(totalDat)],(data[,i] == 0)))
                    imean <- mean(data[,i])
                    itemD <- c(itemD,round((mean(highDat[,i]) - mean(lowDat[,i])),3))
                    if (imean == 1 || imean == 0) {
                        pbi <- c(pbi,0)
                    } else {
                        pbi <- c(pbi,round(((mhigh - mlow) / sd) * sqrt(imean * (1 - imean)),3))
                    }
                }
            }
            pbiDF <- data.frame(itemD,pbi)
            colnames(pbiDF) <- c("ID","rpbi")
            return(pbiDF)
        }
        
        
        myAlpha <- function(data) {
            alphaRes <- reliability(data, itemal = T)
            if (length(alphaRes$N_person) == 0) {
                n <- sprintf("%d",alphaRes$nPerson)
                items <- sprintf("%d",alphaRes$nItem)
                mean <- sprintf("%.2f",round(alphaRes$scaleMean,2))
                sd <- sprintf("%.2f",round(alphaRes$scaleSD,2))
                alpha <- substring(sprintf("%.3f",round(alphaRes$alpha,3)),2,5)
                sem <- round(alphaRes$scaleSD * sqrt(1-alphaRes$alpha),3)
            } else {
                n <- sprintf("%d",alphaRes$N_person)
                items <- sprintf("%d",alphaRes$N_item)
                mean <- sprintf("%.2f",round(alphaRes$scale.mean,2))
                sd <- sprintf("%.2f",round(alphaRes$scale.sd,2))
                alpha <- substring(sprintf("%.3f",round(alphaRes$alpha,3)),2,5)
                sem <- round(alphaRes$scale.sd * sqrt(1-alphaRes$alpha),3)
            }
            
            sumStats <- data.frame(Total=c(n,items,mean,sd,alpha,sem))
            rownames(sumStats) <- c("N","Number of items","Mean","SD","Cronbach's alpha","SEM")
            if (length(alphaRes$N_person) == 0) {
                dropif <- round(ifelse(is.na(alphaRes$alphaIfDeleted),0,alphaRes$alphaIfDeleted),3)
                r.drop <- round(ifelse(is.na(alphaRes$pBis), 0, alphaRes$pBis),3)
                item.mean <- round(alphaRes$itemMean,3)
                itemStats <- data.frame(dropif,r.drop,item.mean)
                rownames(itemStats) <- colnames(data)
            } else {
                dropif <- round(ifelse(is.na(alphaRes$alpha.if.deleted),0,alphaRes$alpha.if.deleted),3)
                r.drop <- round(ifelse(is.na(alphaRes$pbis), 0, alphaRes$pbis),3)
                item.mean <- round(alphaRes$item.mean,3)
                itemStats <- data.frame(dropif,r.drop,item.mean)
                rownames(itemStats) <- attr(alphaRes$item.mean,"names")
            }
            colnames(itemStats) <- c("Drop if","r dropped","IF")
            itemStats <- cbind(itemStats,brownRpbi(data,c()))
            
            return(list(sumStats,itemStats))
        }

            if (ncol(x) == ncol(y) ){

                alphaResult.x <- myAlpha(x)
                alphaResult.y <- myAlpha(y)
                
                list(Pretest = alphaResult.x, Posttest = alphaResult.y)


            } else {
        
                cat("Your datasets need to have the same number of items (columns).")
            }
    })
    
    output$prepost.ds.out <- renderPrint({
        prepost.ds()
    })
    
    
    
    
    
    # Difference Index ==========================================================
    DI <- reactive({
        
        if (input$rowname == 1) {
            
            x <- read.csv(text=input$pretest, sep="\t")
            x <- x[,-1]
            
            y <- read.csv(text=input$posttest, sep="\t")
            y <- y[,-1]
            
        } else {
            
            x <- read.csv(text=input$pretest, sep="\t")
            y <- read.csv(text=input$posttest, sep="\t")
            
        }
        
        if (ncol(x) == ncol(y) ){

            preIF <- colMeans(x, na.rm=T)
            postIF <- colMeans(y, na.rm=T)
            
            prepost <- data.frame(preIF, postIF)
            prepost$DI <- prepost$postIF - prepost$preIF
            colnames(prepost) <- c("  Pretest IF", " Posttest IF", "     DI")
            
            round(prepost, 3)
            
    
        } else {
            
            cat("Your datasets need to have the same number of items (columns).")
            
        }
    
    })
    
    output$DI.out <- renderPrint({
        DI()
    })
    
    
    
    
    
    # Change (gain) score ==========================================================
    gain <- reactive({
        
        pre <- read.csv(text=input$pretest, sep="\t")
        post <- read.csv(text=input$posttest, sep="\t")
        
    
    if (ncol(pre) == ncol(post)){
        
        if (input$rowname == 1) {
            
            pre$total <- rowSums(pre[,-1], na.rm=T)
            x <- data.frame(pre[,1], pre$total)
            post$total <- rowSums(post[,-1], na.rm=T)
            y <-  data.frame(post[,1], post$total)
        
            z <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1], all=T) # all=T includes "NA"
        
            z$change <- z[,3] - z[,2]
            
            # Merged without NA
            zz <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
            
            # Pre-calc for the following two indices
            PreSD <- sd(zz[,2])
            PostSD <- sd(zz[,3])
            ColPrePost <- cor(zz[,2], zz[,3]) # TestRetest reliability
            MeanPre <- mean(zz[,2])
            MeanPost <- mean(zz[,3])
            
            # Expected posttest score (In'nami, 2012)
            ExpPost <- c()
            j <- 1
            for (i in 1:nrow(z)) {
                if (!is.na(z[i, 4])) {
                    ExpPost[j] <- MeanPost + ColPrePost * (PostSD/PreSD) * (z[,2][j] - MeanPre)
                    j <- j + 1
                } else {
                    ExpPost[j] <- NA
                    j <- j + 1
                }
            }
            
            z$ExpPost <- round(ExpPost, 2)
            z$diff <- z[,3] - z$ExpPost
            
            # Corrected Posttest Scores (Bonate, 2000)
            # http://bit.ly/1m3n37a
            AdjPost <- c()
            j <- 1
            for (i in 1:nrow(z)) {
                if (!is.na(z[i, 4])) {
                    AdjPost[j] <- z[,3][j] - (PostSD/PreSD) * (ColPrePost - 1) * (z[,2][j] - MeanPre)
                    j <- j + 1
                } else {
                    AdjPost[j] <- NA
                    j <- j + 1
                }
            }
            
            z$AdjPost <- round(AdjPost, 2)
        
            colnames(z) <- c("ID", "Pre", "Post", "Gain", "Exp.Post", "±RTM", "Adj.Post")
        
            z
        
        } else { # without 1st rowname
            
            if (nrow(pre) == nrow(post)){
            
                x <- rowSums(pre, na.rm=T)
                y <- rowSums(post, na.rm=T)
            
                z <- data.frame(x, y)

                z$change <- z[,2] - z[,1]
                
                # Pre-calc for the following two indices
                PreSD <- sd(z[,1])
                PostSD <- sd(z[,2])
                ColPrePost <- cor(z[,1], z[,2]) # TestRetest reliability
                MeanPre <- mean(z[,1])
                MeanPost <- mean(z[,2])
                
                # Expected posttest score (In'nami, 2012)
                ExpPost <- c()
                j <- 1
                for (i in 1:nrow(z)) {
                    if (!is.na(z[i, 3])) {
                        ExpPost[j] <- MeanPost + ColPrePost * (PostSD/PreSD) * (z[,1][j] - MeanPre)
                        j <- j + 1
                    } else {
                        ExpPost[j] <- NA
                        j <- j + 1
                    }
                }
                
                z$ExpPost <- round(ExpPost, 2)
                z$diff <- z[,2] - z$ExpPost
                
                # Corrected Posttest Scores (Bonate, 2000)
                # http://bit.ly/1m3n37a
                AdjPost <- c()
                j <- 1
                for (i in 1:nrow(z)) {
                    if (!is.na(z[i, 3])) {
                        AdjPost[j] <- z[,3][j] - (PostSD/PreSD) * (ColPrePost - 1) * (z[,1][j] - MeanPre)
                        j <- j + 1
                    } else {
                        AdjPost[j] <- NA
                        j <- j + 1
                    }
                }
                
                z$AdjPost <- round(AdjPost, 2)
                
                colnames(z) <- c("Pre", "Post", "Gain", "Exp.Post", "±RTM", "Adj.Post")
                
                z
            
            } else {
                
                cat("Your datasets need to have the same number of cases (rows).")
 
            }
        }
    
    } else {
        
        cat("Your datasets need to have the same number of items (columns).")

    }
    
    })
    
    output$gain.out <- renderPrint({
        gain()
    })





    # Gain score reliability ==========================================================
    gain.rel <- reactive({
    
        pre <- read.csv(text=input$pretest, sep="\t")
        post <- read.csv(text=input$posttest, sep="\t")
    
    
        if (ncol(pre) == ncol(post)){
        
            if (input$rowname == 1) {
            
                pre.alpha <- alpha(pre[,-1], check.keys=F, na.rm=T)
                post.alpha <- alpha(post[,-1], check.keys=F, na.rm=T)
            
                pre$total <- rowSums(pre[,-1], na.rm=T)
                x <- data.frame(pre[,1], pre$total)
                post$total <- rowSums(post[,-1], na.rm=T)
                y <-  data.frame(post[,1], post$total)
            
                # Merged without NA
                zz <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
            
            
                # Pre-calc for the following two indices
                RelA <- pre.alpha[[1]][,1] # Preの信頼性係数
                RelB <- post.alpha[[1]][,1] # Postの信頼性係数
                ColAB <- cor(zz[,2], zz[,3]) # PrePostの相関係数
                SdA <- sd(zz[,2]) # PretestのSD
                SdB <- sd(zz[,3]) # PosttestのSD
                VarA <- SdA^2 # Preの分散（標準偏差の2乗）
                VarB <- SdB^2 # Postの分散（標準偏差の2乗）
            
                # Lord1963
                # http://lilt.ilstu.edu/staylor/csdcb/articles/Volume9/Chiou%20et%20al%201996.pdf
                numerator1 <- VarA*RelA + VarB*RelB - 2*ColAB*SdA*SdB
                denominator1 <- VarA + VarB - 2*ColAB*SdA*SdB
                GainRelLord <- round((numerator1/denominator1), 3)
            
                # Thorndike (Educational Measurement, 1971)
                numerator2 <- ((RelA + RelB)/2) - ColAB
                denominator2 <- 1 - ColAB
                GainRelThorndike <- round((numerator2/denominator2), 3)
                
                cat(" Lord's formula (1963):", GainRelLord, "\n",
                    "Thorndike's formula (1971):", GainRelThorndike, "\n")
                
           
            } else { # without 1st rowname
            
            if (nrow(pre) == nrow(post)){
                
                pre.alpha <- alpha(pre, check.keys=F, na.rm=T)
                post.alpha <- alpha(post, check.keys=F, na.rm=T)
                
                x <- rowSums(pre, na.rm=T)
                y <- rowSums(post, na.rm=T)
                
                z <- data.frame(x, y)


                # Pre-calc for the following two indices
                RelA <- pre.alpha[[1]][,1] # Preの信頼性係数
                RelB <- post.alpha[[1]][,1] # Postの信頼性係数
                ColAB <- cor(z[,1], z[,2]) # PrePostの相関係数
                SdA <- sd(z[,1]) # PretestのSD
                SdB <- sd(z[,2]) # PosttestのSD
                VarA <- SdA^2 # Preの分散（標準偏差の2乗）
                VarB <- SdB^2 # Postの分散（標準偏差の2乗）
                
                # Lord1963
                # http://lilt.ilstu.edu/staylor/csdcb/articles/Volume9/Chiou%20et%20al%201996.pdf
                numerator1 <- VarA*RelA + VarB*RelB - 2*ColAB*SdA*SdB
                denominator1 <- VarA + VarB - 2*ColAB*SdA*SdB
                GainRelLord <- round((numerator1/denominator1), 3)
                
                # Thorndike (Educational Measurement, 1971)
                numerator2 <- ((RelA + RelB)/2) - ColAB
                denominator2 <- 1 - ColAB
                GainRelThorndike <- round((numerator2/denominator2), 3)
                
                cat(" Lord's formula (1963):", GainRelLord, "\n",
                "Thorndike's formula (1971):", GainRelThorndike, "\n")
                
                } else {
                
                    cat("Your datasets need to have the same number of cases (rows).")
                
                }
            }
        
        } else {
        
            cat("Your datasets need to have the same number of items (columns).")
        
        }
    
    })

    output$gain.rel.out <- renderPrint({
        gain.rel()
    })





    # Overlayed histograms ==========================================================

    makedistPlot <- function(){
        
        x <- read.csv(text=input$pretest, sep="\t")
        y <- read.csv(text=input$posttest, sep="\t")
        
        
    if (ncol(x) == ncol(y)){

        if (input$rowname == 1) {
            
            x$total <- rowSums(x[,-1], na.rm=T)
            x <- data.frame(x[,1], x$total)
            y$total <- rowSums(y[,-1], na.rm=T)
            y <-  data.frame(y[,1], y$total)
            
            z <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
            
            x <- z[,2]
            y <- z[,3]
            
        } else {
            
            x <- rowSums(x, na.rm=T)
            y <- rowSums(y, na.rm=T)
            
        }
     
        
        simple.bincount <- function(x, breaks) {
            nx <- length(x)
            nbreaks <- length(breaks)
            counts <- integer(nbreaks - 1)
            for (i in 1:nx) {
                lo <- 1
                hi <- nbreaks
                if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
                    while (hi - lo >= 2) {
                        new <- (hi + lo) %/% 2
                        if(x[i] > breaks[new])
                        lo <- new
                        else
                        hi <- new
                    }
                    counts[lo] <- counts[lo] + 1
                }
            }
            return(counts)
        }
        
        nclass.x <- nclass.FD(x)
        breaks.x <- pretty(x, nclass.x)
        counts.x <- simple.bincount(x, breaks.x)
        counts.max.x <- max(counts.x)
        
        nclass.y <- nclass.FD(y)
        breaks.y <- pretty(y, nclass.y)
        counts.y <- simple.bincount(y, breaks.y)
        counts.max.y <- max(counts.y)
        
        counts.max <- max(c(counts.max.x, counts.max.y))
        
        
        xy.min <- min(c(x,y))
        xy.min <- xy.min - xy.min*0.1
        xy.max <- max(c(x,y))
        xy.max <- xy.max + xy.max*0.1
        
        p1 <- hist(x, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
        p2 <- hist(y, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
        
        plot(p1, las=1, xlab = "Pretest is expressed in blue; Posttest in red. Vertical lines show the mean.",
        main = "", col = rgb(0,0,1,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3))
        plot(p2, las=1, xlab = "", main = "", col = rgb(1,0,0,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3), add = TRUE)
        
        abline(v = mean(x), col = "blue", lwd = 2)
        abline(v = mean(y), col = "red", lwd = 2)
    
    } else {
        
        NULL
        
    }
    }

    
    output$distPlot <- renderPlot({
        print(makedistPlot())
    })
    
    
    


    # Boxplots ==========================================================
    
    makeboxPlot <- function(){
        
        x <- read.csv(text=input$pretest, sep="\t")
        y <- read.csv(text=input$posttest, sep="\t")
        
    
    if (ncol(x) == ncol(y)){

        if (input$rowname == 1) {
            
            x$total <- rowSums(x[,-1], na.rm=T)
            x <- data.frame(x[,1], x$total)
            y$total <- rowSums(y[,-1], na.rm=T)
            y <-  data.frame(y[,1], y$total)
            
            z <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
            
            x <- z[,2]
            y <- z[,3]
            
        } else {
            
            x <- rowSums(x, na.rm=T)
            y <- rowSums(y, na.rm=T)
            
        }

        score <- c(x, y)
        
        group <- factor(c(rep(1, length(x)), rep(2, length(y))))
        group <- factor(group, levels=c(1,2), labels=c("Pretest", "Posttest"))
        
        boxplot(score ~ group, las=1, xlab= "Means and +/-1 SDs are displayed in red.")
        
        beeswarm(score ~ group, col = 4, pch = 16, vert = TRUE,  add = TRUE)
        
        points(1.2, mean(x), pch = 18, col = "red", cex = 2)
        arrows(1.2, mean(x), 1.2, mean(x) + sd(x), length = 0.1, angle = 45, col = "red")
        arrows(1.2, mean(x), 1.2, mean(x) - sd(x), length = 0.1, angle = 45, col = "red")
        
        points(2.2, mean(y), pch = 18, col = "red", cex = 2)
        arrows(2.2, mean(y), 2.2, mean(y) + sd(y), length = 0.1, angle = 45, col = "red")
        arrows(2.2, mean(y), 2.2, mean(y) - sd(y), length = 0.1, angle = 45, col = "red")
    
    } else {
        
        NULL
        
    }
    }
    
    
    output$boxPlot <- renderPlot({
        print(makeboxPlot())
    })
    
    
    
    
    
    # Spaghetti Plot ==========================================================
    
    makeindvPlot <- function(){
        
        x <- read.csv(text=input$pretest, sep="\t")
        y <- read.csv(text=input$posttest, sep="\t")
    
    
    if (ncol(x) == ncol(y)){
        
        if (input$rowname == 1) {
            
            x$total <- rowSums(x[,-1], na.rm=T)
            x <- data.frame(x[,1], x$total)
            y$total <- rowSums(y[,-1], na.rm=T)
            y <-  data.frame(y[,1], y$total)
            
            z <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
            
            x <- z[,2]
            y <- z[,3]
            
        } else {
            
            if (nrow(x) == nrow(y)){
                
                x <- rowSums(x, na.rm=T)
                y <- rowSums(y, na.rm=T)
                
                z <- data.frame(x, y)
                
                x <- z[,1]
                y <- z[,2]
                
            } else {
                
                NULL
                
            }
        }
        
        xy <- c(x, y)
        data.point <- factor(c(rep(1, length(x)), rep(2, length(y))))
        data.point <- factor(data.point, levels=c(1,2), labels=c("Pretest", "Posttest"))
        dat <- data.frame(xy, data.point)
        dat$indiv <- factor(c(rep(1:length(x)), rep(1:length(y))))
        each <- xyplot(xy ~ data.point, group = indiv,
        type = c("l"), data = dat, xlab ="", ylab="")
        a <- mean(x)
        b <- mean(y)
        value <- c(a, b)
        data.point2 <- factor(c(rep(1, 1), rep(2, 1)))
        data.point2 <- factor(data.point2, levels=c(1,2), labels=c("Pretest", "Posttest"))
        dat2 <- data.frame(value, data.point2)
        all <- xyplot(value ~ data.point2, col = "black",
        lwd = 5, type = c("l"), data = dat, xlab = "", ylab = "")
        indv <- each + as.layer(all, axes = NULL)
        # print(indv)
        
    } else {
        
        NULL
        
    }
    }

    
    output$indvPlot <- renderPlot({
        print(makeindvPlot())
    })
    
    
    
    
    
    # Scatterplot ==========================================================

    makecorrelPlot <- function(){
        
        x <- read.csv(text=input$pretest, sep="\t")
        y <- read.csv(text=input$posttest, sep="\t")
    
    if (ncol(x) == ncol(y)){

        if (input$rowname == 1) {
            
            x$total <- rowSums(x[,-1], na.rm=T)
            x <- data.frame(x[,1], x$total)
            y$total <- rowSums(y[,-1], na.rm=T)
            y <-  data.frame(y[,1], y$total)
            
            z <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
            
            x <- z[,2]
            y <- z[,3]
            
        } else {
            
            x <- rowSums(x, na.rm=T)
            y <- rowSums(y, na.rm=T)
            
        }
        
        xy.min <- min(c(x,y))
        xy.min <- xy.min - xy.min*0.1
        xy.max <- max(c(x,y))
        xy.max <- xy.max + xy.max*0.1
        
        plot(x, y, las=1, pch = 16, xlab = "Pretest", ylab = "Posttest", main = "",
        xlim = c(xy.min,xy.max), ylim = c(xy.min,xy.max))
        lines(par()$usr[1:2], par()$usr[3:4], lty = 3)
        
    } else {
        
        NULL
        
    }
    }


    output$correlPlot <- renderPlot({
        print(makecorrelPlot())
    })





    # t-test ==========================================================
    
    t <- reactive({
        
        x <- read.csv(text=input$pretest, sep="\t")
        y <- read.csv(text=input$posttest, sep="\t")
        
        if (ncol(x) == ncol(y)){
            
            if (input$rowname == 1) {
                
                x$total <- rowSums(x[,-1], na.rm=T)
                x <- data.frame(x[,1], x$total)
                y$total <- rowSums(y[,-1], na.rm=T)
                y <-  data.frame(y[,1], y$total)
                
                z <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
                
                x <- z[,2]
                y <- z[,3]
                
                t.test(x, y, paired=TRUE)

            } else {
                
                if (nrow(x) == nrow(y)){
                    
                    x <- rowSums(x, na.rm=T)
                    y <- rowSums(y, na.rm=T)
                    
                    z <- data.frame(x, y)
                    
                    x <- z[,1]
                    y <- z[,2]
                    
                    t.test(x, y, paired=TRUE)
                    
                } else {
                    
                    cat("Your datasets need to have the same number of cases (rows).")

                }
            }
        
        } else {
            
            cat("Your datasets need to have the same number of items (columns).")
            
        }
    })
    
    output$t.out <- renderPrint({
        t()
    })
    
    
    
    
    
    # Effect size ==========================================================

    es <- reactive({
        
        x <- read.csv(text=input$pretest, sep="\t")
        y <- read.csv(text=input$posttest, sep="\t")
        
    
    if (ncol(x) == ncol(y)){
        
        if (input$rowname == 1) {
            
            x$total <- rowSums(x[,-1], na.rm=T)
            x <- data.frame(x[,1], x$total)
            y$total <- rowSums(y[,-1], na.rm=T)
            y <-  data.frame(y[,1], y$total)
            
            z <- merge(x=x, y=y, by.x=names(x)[1], by.y=names(y)[1]) # all=T includes "NA"
            
            x <- z[,2]
            y <- z[,3]
            
            m1 <- mean(x)
            sd1 <- sd(x)
            n1 <- length(x)
            
            m2 <- mean(y)
            sd2 <- sd(y)
            n2 <- length(y)
            
            diff <- x - y
            
            d <- abs(mean(diff)/(sd(diff)/sqrt(2*(1-cor(x, y)))))
            var.d <- ((1/n1)+((d^2)/(2*n1)))*(2*(1-cor(x, y)))
            
            df <- n1 - 1
            j <- 1 - (3/(4 * df - 1))
            g <- j * d
            var.g <- j^2 * var.d
            
            alpha <- 0.05
            crit <- qt(alpha/2, df, lower.tail = FALSE)
            
            lower.d <- d - crit * sqrt(var.d)
            upper.d <- d + crit * sqrt(var.d)
            
            lower.g <- g - crit * sqrt(var.g)
            upper.g <- g + crit * sqrt(var.g)
            
            
            cat("=======================================================", "\n")
            cat(" Mean difference / within-groups SD:", "\n",
            "(Based on Borenstein et al., 2009) ", "\n",
            "\n",
            "  d [95% CI] =", d, "[", lower.d, ",", upper.d, "]", "\n",
            "   var(d) =", var.d, "\n",
            "\n",
            "  g [95% CI] =", g, "[", lower.g, ",", upper.g, "]", "\n",
            "   var(g) =", var.g, "\n",
            "\n"
            )
            
            
            
            delta <- abs(mean(diff)/sd(x))
            
            cat("=======================================================", "\n")
            cat(" Mean difference / SD of Data 1 (e.g., pretest):", "\n",
            "\n",
            "  Delta =", delta, "\n",
            "\n"
            )
            
            
            
            result.dependent<-t.test(x,y,paired=TRUE)
            paired.t<-result.dependent$statistic
            r2 <- sqrt(paired.t^2/(paired.t^2+df))
            r2 <- r2[[1]]
            
            cat("=======================================================", "\n")
            cat(" r --- sqrt(paired.t^2/(paired.t^2+df)):", "\n",
            "\n",
            "  r =", r2, "\n",
            "\n"
            )



        } else {
            
            if (nrow(x) == nrow(y)){
                
                x <- rowSums(x, na.rm=T)
                y <- rowSums(y, na.rm=T)
                
                z <- data.frame(x, y)
                
                x <- z[,1]
                y <- z[,2]
                
                m1 <- mean(x)
                sd1 <- sd(x)
                n1 <- length(x)
                
                m2 <- mean(y)
                sd2 <- sd(y)
                n2 <- length(y)
                
                diff <- x - y
                
                d <- abs(mean(diff)/(sd(diff)/sqrt(2*(1-cor(x, y)))))
                var.d <- ((1/n1)+((d^2)/(2*n1)))*(2*(1-cor(x, y)))
                
                df <- n1 - 1
                j <- 1 - (3/(4 * df - 1))
                g <- j * d
                var.g <- j^2 * var.d
                
                alpha <- 0.05
                crit <- qt(alpha/2, df, lower.tail = FALSE)
                
                lower.d <- d - crit * sqrt(var.d)
                upper.d <- d + crit * sqrt(var.d)
                
                lower.g <- g - crit * sqrt(var.g)
                upper.g <- g + crit * sqrt(var.g)
                
                
                cat("=======================================================", "\n")
                cat(" Mean difference / within-groups SD:", "\n",
                "(Based on Borenstein et al., 2009) ", "\n",
                "\n",
                "  d [95% CI] =", d, "[", lower.d, ",", upper.d, "]", "\n",
                "   var(d) =", var.d, "\n",
                "\n",
                "  g [95% CI] =", g, "[", lower.g, ",", upper.g, "]", "\n",
                "   var(g) =", var.g, "\n",
                "\n"
                )
                
                
                
                delta <- abs(mean(diff)/sd(x))
                
                cat("=======================================================", "\n")
                cat(" Mean difference / SD of Data 1 (e.g., pretest):", "\n",
                "\n",
                "  Delta =", delta, "\n",
                "\n"
                )
                
                
                
                result.dependent<-t.test(x,y,paired=TRUE)
                paired.t<-result.dependent$statistic
                r2 <- sqrt(paired.t^2/(paired.t^2+df))
                r2 <- r2[[1]]
                
                cat("=======================================================", "\n")
                cat(" r --- sqrt(paired.t^2/(paired.t^2+df)):", "\n",
                "\n",
                "  r =", r2, "\n",
                "\n"
                )

                
            } else {
                
                cat("Your datasets need to have the same number of cases (rows).")
                
            }
        }
    
    } else {
        
        cat("Your datasets need to have the same number of items (columns).")
        
    }
    })
    
    output$es.out <- renderPrint({
        es()
    })
    
    
    
    
    
    
    
    
    
    
########################################
# Cut-score Indices
########################################

    # Basic stats ==========================================================
    cutscore.ds <- reactive({
    
    
        if (input$rownameCS == 1) {
        
            dat <- read.csv(text=input$cutscore, sep="\t")
            dat <- dat[,-1]
        
        } else {
        
            dat <- read.csv(text=input$cutscore, sep="\t")
        
        }
        
        brownRpbi <- function(data,missing) {
            m <- mean(rowSums(data))
            sd <- sd(rowSums(data))
            totalDat <- cbind(data,rowSums(data))
            sortDat <- totalDat[order(-totalDat[,length(totalDat)]),]
            pbi <- c()
            r <- c()
            itemD <- c()
            rownames(sortDat) <- c(1:nrow(sortDat))
            highDat <- head(sortDat,nrow(sortDat) %/% 3)
            lowDat <- tail(sortDat,nrow(sortDat) %/% 3)
            for (i in 1:length(data)) {
                if (is.element(colnames(data)[i], missing) == F ) {
                    mhigh <- mean(subset(totalDat[,length(totalDat)],(data[,i] == 1)))
                    mlow <- mean(subset(totalDat[,length(totalDat)],(data[,i] == 0)))
                    imean <- mean(data[,i])
                    itemD <- c(itemD,round((mean(highDat[,i]) - mean(lowDat[,i])),3))
                    if (imean == 1 || imean == 0) {
                        pbi <- c(pbi,0)
                    } else {
                        pbi <- c(pbi,round(((mhigh - mlow) / sd) * sqrt(imean * (1 - imean)),3))
                    }
                }
            }
            pbiDF <- data.frame(itemD,pbi)
            colnames(pbiDF) <- c("ID","rpbi")
            return(pbiDF)
        }
        
        
        myAlpha <- function(data) {
            alphaRes <- reliability(data, itemal = T)
            if (length(alphaRes$N_person) == 0) {
                n <- sprintf("%d",alphaRes$nPerson)
                items <- sprintf("%d",alphaRes$nItem)
                mean <- sprintf("%.2f",round(alphaRes$scaleMean,2))
                sd <- sprintf("%.2f",round(alphaRes$scaleSD,2))
                alpha <- substring(sprintf("%.3f",round(alphaRes$alpha,3)),2,5)
                sem <- round(alphaRes$scaleSD * sqrt(1-alphaRes$alpha),3)
            } else {
                n <- sprintf("%d",alphaRes$N_person)
                items <- sprintf("%d",alphaRes$N_item)
                mean <- sprintf("%.2f",round(alphaRes$scale.mean,2))
                sd <- sprintf("%.2f",round(alphaRes$scale.sd,2))
                alpha <- substring(sprintf("%.3f",round(alphaRes$alpha,3)),2,5)
                sem <- round(alphaRes$scale.sd * sqrt(1-alphaRes$alpha),3)
            }
        
            sumStats <- data.frame(Total=c(n,items,mean,sd,alpha,sem))
            rownames(sumStats) <- c("N","Number of items","Mean","SD","Cronbach's alpha","SEM")
            if (length(alphaRes$N_person) == 0) {
                dropif <- round(ifelse(is.na(alphaRes$alphaIfDeleted),0,alphaRes$alphaIfDeleted),3)
                r.drop <- round(ifelse(is.na(alphaRes$pBis), 0, alphaRes$pBis),3)
                item.mean <- round(alphaRes$itemMean,3)
                itemStats <- data.frame(dropif,r.drop,item.mean)
                rownames(itemStats) <- colnames(data)
            } else {
                dropif <- round(ifelse(is.na(alphaRes$alpha.if.deleted),0,alphaRes$alpha.if.deleted),3)
                r.drop <- round(ifelse(is.na(alphaRes$pbis), 0, alphaRes$pbis),3)
                item.mean <- round(alphaRes$item.mean,3)
                itemStats <- data.frame(dropif,r.drop,item.mean)
                rownames(itemStats) <- attr(alphaRes$item.mean,"names")
            }
            colnames(itemStats) <- c("Drop if","r dropped","IF")
            itemStats <- cbind(itemStats,brownRpbi(data,c()))
        
            return(list(sumStats,itemStats))
        }
        
            myAlpha(dat)
    })

    output$cutscore.ds.out <- renderPrint({
        cutscore.ds()
    })





    # Total score and % ==========================================================
    totalper <- reactive({
    
        dat <- read.csv(text=input$cutscore, sep="\t")
    
        if (input$rownameCS == 1) {
        
            x <- dat
            x$total <- rowSums(dat[,-1], na.rm=T)
            x$percent <- round(x$total/length(dat[,-1]),3)
            sortDat <- x[order(-x[,length(x)]),]
        
            res <- data.frame(sortDat[,1], sortDat$total, sortDat$percent*100)
            colnames(res) <- c("ID", "Total score", "%")
            res
        
        } else {
            
            x <- dat
            x$total <- rowSums(dat, na.rm=T)
            x$percent <- round(x$total/length(dat),3)
            sortDat <- x[order(-x[,length(x)]),]
            
            res <- data.frame(sortDat$total, sortDat$percent*100)
            colnames(res) <- c("Total", "%")
            res
        
        }
    })

    output$totalper.out <- renderPrint({
        totalper()
    })
    
    
    
    
    
    # Cut-score indiecs ==========================================================
    cutscoreInd <- reactive({
        
        dat <- read.csv(text=input$cutscore, sep="\t")
        
        if (input$rownameCS == 1) {
            
            x <- dat
            x$total <- rowSums(dat[,-1], na.rm=T)
            x$percent <- round(x$total/length(dat[,-1]),3)
            sortDat <- x[order(-x[,length(x)]),]
            
            TotalIF <- round(colMeans(dat[,-1], na.rm=T), 3)
            
            
            # From the input of cut-score
            CutScore <- input$CutPoint
            cs <- CutScore * 0.01
            
            
            # Dependability Indices
            x.alpha <- alpha(dat[,-1], check.keys=F, na.rm=T)
            KR20 <- x.alpha[[1]][,1] # KR20=Cronbach
            
            n <- nrow(dat)
            varp = function(x) { var(x) * (length(x)-1) / length(x) } #varではなくvarpを求める関数
            s2 <- varp(x$percent)
            M <- mean(x$percent)
            k <- ncol(dat)-1
            
            DependOrg <- ((n*s2)/(n-1))*KR20 / ((((n*s2)/(n-1))*KR20) + ((M*(1-M)-s2)/(k-1)))
            Depend <- round(DependOrg, 3)
            
            CIcrtOrg <- sqrt((M*(1-M)-s2)/(k-1))
            CIcrt <- round((CIcrtOrg*100),3)
            
            phicutOrg <- 1 - ((1/(k-1)) * ((M*(1-M) - s2)/((M-cs)^2+s2)))
            phicut <- round(phicutOrg, 3)
            
            
            # データフレームで項目ごとの指標算出
            pass <- subset(x, percent >= cs) # cut-score以上の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            pass <- pass[setdiff(colnames(pass), delete)]
            PassIF <- round(colMeans(pass[,-1], na.rm=T), 3) # 合格者のIF
            
            fail <- subset(x, percent < cs)  # cut-score未満の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            fail <- fail[setdiff(colnames(fail), delete)]
            FailIF <- round(colMeans(fail[,-1], na.rm=T), 3) # 不合格のIF
            
            # B-index
            B <- PassIF - FailIF
            
            # 一致指数（Agreement statistic）
            # Pit = 正答者の中での正解者数/全受験者数
            Pit <- colSums(pass[,-1])/nrow(dat)
            # 誤答率Qi
            Qi <- 1 - TotalIF
            # Pt 合格者の割合
            Pt <- nrow(pass)/nrow(dat)
            # 一致指数（Agreement statistic）
            A <- round((2 * Pit + Qi - Pt), 3)
            
            # Φ (Phi)
            # 正解者数率Pi（全体のIFと同じ）
            Pi <- round(colMeans(dat[,-1], na.rm=T), 3) # 全体のIF
            # 不正解者数率Qi
            Qi <- 1 - Pi
            # Pt 合格者の割合
            Pt <- nrow(pass)/nrow(dat)
            # 不合格者数率Qt
            Qt <- 1 - Pt
            # Pit = 正答者の中での正解者数/全受験者数
            Pit <- colSums(pass[,-1])/nrow(dat)
            # Φ
            Φ <- round((Pit - Pi*Pt) / (sqrt(Pi*Qi*Pt*Qt)),3)
            
            res <- data.frame(TotalIF, PassIF, FailIF, B, A, Φ)
            
            cat(" Dependability (Φ) =", Depend, "\n",
            "CRT CI (in %) =", CIcrt, "\n",
            "Φ(λ) =", phicut, "\n",
            "\n"
            )
            cat("=======================================================", "\n", "\n")
            
            res


        } else {
            
            x <- dat
            x$total <- rowSums(dat, na.rm=T)
            x$percent <- round(x$total/length(dat),3)
            sortDat <- x[order(-x[,length(x)]),]
            
            TotalIF <- round(colMeans(dat, na.rm=T), 3)
            
            # From the input of cut-score
            CutScore <- input$CutPoint
            cs <- CutScore * 0.01
            
            
            # Dependability Indices
            x.alpha <- alpha(dat[,-1], check.keys=F, na.rm=T)
            KR20 <- x.alpha[[1]][,1] # KR20=Cronbach
            
            n <- nrow(dat)
            varp = function(x) { var(x) * (length(x)-1) / length(x) } #varではなくvarpを求める関数
            s2 <- varp(x$percent)
            M <- mean(x$percent)
            k <- ncol(dat)-1
            
            DependOrg <- ((n*s2)/(n-1))*KR20 / ((((n*s2)/(n-1))*KR20) + ((M*(1-M)-s2)/(k-1)))
            Depend <- round(DependOrg, 3)
            
            CIcrtOrg <- sqrt((M*(1-M)-s2)/(k-1))
            CIcrt <- round((CIcrtOrg*100),3)
            
            phicutOrg <- 1 - ((1/(k-1)) * ((M*(1-M) - s2)/((M-cs)^2+s2)))
            phicut <- round(phicutOrg, 3)
            
            
            # データフレームで項目ごとの指標算出
            pass <- subset(x, percent >= cs) # cut-score以上の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            pass <- pass[setdiff(colnames(pass), delete)]
            PassIF <- round(colMeans(pass, na.rm=T), 3) # 合格者のIF
            
            fail <- subset(x, percent < cs)  # cut-score未満の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            fail <- fail[setdiff(colnames(fail), delete)]
            FailIF <- round(colMeans(fail, na.rm=T), 3) # 不合格のIF
            
            
            # B-index
            B <- PassIF - FailIF
            
            # 一致指数（Agreement statistic）
            # Pit = 正答者の中での正解者数/全受験者数
            Pit <- colSums(pass)/nrow(dat)
            # 誤答率Qi
            Qi <- 1 - TotalIF
            # Pt 合格者の割合
            Pt <- nrow(pass)/nrow(dat)
            # 一致指数（Agreement statistic）
            A <- round((2 * Pit + Qi - Pt), 3)
            
            # Φ (Phi)
            # 正解者数率Pi（全体のIFと同じ）
            Pi <- round(colMeans(dat, na.rm=T), 3) # 全体のIF
            # 不正解者数率Qi
            Qi <- 1 - Pi
            # Pt 合格者の割合
            Pt <- nrow(pass)/nrow(dat)
            # 不合格者数率Qt
            Qt <- 1 - Pt
            # Pit = 正答者の中での正解者数/全受験者数
            Pit <- colSums(pass)/nrow(dat)
            # Φ
            Φ <- round((Pit - Pi*Pt) / (sqrt(Pi*Qi*Pt*Qt)),3)
            
            res <- data.frame(TotalIF, PassIF, FailIF, B, A, Φ)
            
            cat(" Dependability (Φ) =", Depend, "\n",
            "CRT CI (in %) =", CIcrt, "\n",
            "Φ(λ) =", phicut, "\n",
            "\n"
            )
            cat("=======================================================", "\n", "\n")
            
            res


        }
    })
    
    output$cutscoreInd.out <- renderPrint({
        cutscoreInd()
    })





    # Histogram ==========================================================
    makedistPlot2 <- function(){
        
        dat <- read.csv(text=input$cutscore, sep="\t")

        if (input$rowname == 1) {
            
            x <- rowSums(dat[,-1], na.rm=T)
            
        } else {
            
            x <- rowSums(dat, na.rm=T)
        
        }

        simple.bincount <- function(x, breaks) {
            nx <- length(x)
            nbreaks <- length(breaks)
            counts <- integer(nbreaks - 1)
            for (i in 1:nx) {
                lo <- 1
                hi <- nbreaks
                if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
                    while (hi - lo >= 2) {
                        new <- (hi + lo) %/% 2
                        if(x[i] > breaks[new])
                        lo <- new
                        else
                        hi <- new
                    }
                    counts[lo] <- counts[lo] + 1
                }
            }
            return(counts)
        }
        
        nclass <- nclass.FD(x)
        breaks <- pretty(x, nclass)
        counts <- simple.bincount(x, breaks)
        counts.max <- max(counts)
        
        h <- hist(x, las=1, breaks="FD", xlab= "Red vertical line shows the mean.",
        ylim=c(0, counts.max*1.2), main="", col = "cyan")
        rug(x)
        abline(v = mean(x, na.rm=T), col = "red", lwd = 2)
        xfit <- seq(min(x, na.rm=T), max(x, na.rm=T))
        yfit <- dnorm(xfit, mean = mean(x, na.rm=T), sd = sd(x, na.rm=T))
        yfit <- yfit * diff(h$mids[1:2]) * length(x)
        lines(xfit, yfit, col = "blue", lwd = 2)
    }
    
    
    output$distPlot2 <- renderPlot({
        print(makedistPlot2())
    })
    
    
    
    
    
    # Boxplot ==========================================================

    makeboxPlot2 <- function(){
        
        dat <- read.csv(text=input$cutscore, sep="\t")
        
        if (input$rowname == 1) {
            
            x <- rowSums(dat[,-1], na.rm=T)
            
        } else {
            
            x <- rowSums(dat, na.rm=T)
            
        }
        
        boxplot(x, horizontal=TRUE, xlab= "Mean and +/-1 SD are displayed in red.")
        beeswarm(x, horizontal=TRUE, col = 4, pch = 16, add = TRUE)
        points(mean(x, na.rm=T), 0.9, pch = 18, col = "red", cex = 2)
        arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) + sd(x, na.rm=T), length = 0.1, angle = 45, col = "red")
        arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) - sd(x, na.rm=T), length = 0.1, angle = 45, col = "red")
    }
    
    
    output$boxPlot2 <- renderPlot({
        print(makeboxPlot2()) # 上の function を参照する指定
    })
    
    
    
    
    
    # Comparison of masters and non-masters ==========================================================
    
    masternonmaster <- function(){
        
        dat <- read.csv(text=input$cutscore, sep="\t")


        if (input$rownameCS == 1) {
            
            x <- dat
            x$total <- rowSums(dat[,-1], na.rm=T)
            x$percent <- round(x$total/length(dat[,-1]),3)
            
            # From the input of cut-score
            CutScore <- input$CutPoint
            cs <- CutScore * 0.01
            
            pass <- subset(x, percent >= cs) # cut-score以上の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            pass <- pass[setdiff(colnames(pass), delete)]
            passTotal <- rowSums(pass[,-1], na.rm=T) # 合格者の合計点
            
            fail <- subset(x, percent < cs)  # cut-score未満の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            fail <- fail[setdiff(colnames(fail), delete)]
            failTotal <- rowSums(fail[,-1], na.rm=T) # 不合格の合格点
            
        } else {
            
            x <- dat
            x$total <- rowSums(dat, na.rm=T)
            x$percent <- round(x$total/length(dat),3)
            
            # From the input of cut-score
            CutScore <- input$CutPoint
            cs <- CutScore * 0.01
            
            pass <- subset(x, percent >= cs) # cut-score以上の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            pass <- pass[setdiff(colnames(pass), delete)]
            passTotal <- rowSums(pass, na.rm=T) # 合格者の合計点
            
            fail <- subset(x, percent < cs)  # cut-score未満の学習者のデータフレーム
            delete <- c("total", "percent") # 削除する列の指定
            fail <- fail[setdiff(colnames(fail), delete)]
            failTotal <- rowSums(fail, na.rm=T) # 不合格の合格点
            
        }
        
        x <- passTotal
        y <- failTotal
        
        score <- c(x, y)
        group <- factor(c(rep("Masters", length(x)), rep("Non-masters", length(y))))
        
        boxplot(score ~ group, las=1, xlab= "Means and +/-1 SDs are displayed in red.")
        
        beeswarm(score ~ group, col = 4, pch = 16, vert = TRUE,  add = TRUE)
        
        points(1.2, mean(x), pch = 18, col = "red", cex = 2)
        arrows(1.2, mean(x), 1.2, mean(x) + sd(x), length = 0.1, angle = 45, col = "red")
        arrows(1.2, mean(x), 1.2, mean(x) - sd(x), length = 0.1, angle = 45, col = "red")
        
        points(2.2, mean(y), pch = 18, col = "red", cex = 2)
        arrows(2.2, mean(y), 2.2, mean(y) + sd(y), length = 0.1, angle = 45, col = "red")
        arrows(2.2, mean(y), 2.2, mean(y) - sd(y), length = 0.1, angle = 45, col = "red")
    }
    
    output$masternonmaster <- renderPlot({
        print(masternonmaster())
    })










    # R Session Info (tabPanel "Difference Index") ==========================================================

    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })
    
    
    # R Session Info (tabPanel "Cut-score Indices") ==========================================================
    
    info22 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info22.out <- renderPrint({
        info22()
    })




  
  })
   
})
