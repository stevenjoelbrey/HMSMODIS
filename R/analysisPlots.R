# analysisPlots.R

library(stringr)

# TODO: Handle binding the different years dataframes
merged <- get(load("Data/hms_with_modis_2006_2007.RData"))

for (y1 in 2007:2014){
  
  y2 <- y1 + 1
  
  f <- paste0("Data/hms_with_modis_",y1,"_",y2,".RData")
  merged <- rbind(merged, get(load(f)))
  
}
hms_df <- merged


# subset HMS df to CONUS regions
desiredRegions <- c("NorthEast","MidAtlantic","SouthEast","MidWest"                  
                    ,"SouthernPlains","GreatPlains","RockyMountains","SouthWest"                
                    ,"NorthWest")
# region colors 
rCol <- c("#CACDFC", "#779CA6", "#CCFEA1", "#F2AE75", "#CECB6F", "#92CDFC",
          "#97CC9C", "#92FFCF", "#FFE7A0")

niceNames <- c("Northeast","Mid Atlantic","Southeast","Midwest","Southern Plains"      
               ,"Great Plains","Rocky Mountains","Southwest","Northwest")

regionMask <- hms_df$region %in% desiredRegions
hms_df <- hms_df[regionMask,]
hms_df$regionCol <- "black"

# Assign a pch based on season, 
# NOTE: I am very disgusted by the line of code below
mon <- as.numeric(str_sub(as.character(hms_df$Date), 6,7))
hms_df$month <- mon

pch <- rep(NA, length(mon))
pch[mon == 11 | mon == 12 | mon==1 | mon == 2] <- 17
pch[mon == 3 | mon == 4 | mon == 5] <- 15
pch[mon == 6 | mon == 7 | mon == 8 | mon == 9 | mon == 10] <- 19
hms_df$pch <- pch


# Assign the colors
for (r in desiredRegions){
  
  col <- rCol[r == desiredRegions]
  
  m <- r == hms_df$region
  
  hms_df$regionCol[m] <- col
  
}

Dur <- as.numeric(str_sub(hms_df$Dur,1,2))
hms_df$Dur <- Dur

oldPar <- par()
  
################################################################################
# Duration_vs_FRP for HP where there are detections within 4km
################################################################################

# subset hms_df by those that have modis within 4km
m <- hms_df$nWithin4 > 0 
df <- hms_df[m,]

pdf(file = "Figures/duration_vs_FRP_all.pdf")
par(las=1)
plot(df$Dur, df$mean_frp, col=df$regionCol, pch=df$pch,
     ylab="FRP", xlab="HMS Fire Duration (hrs)",
     bty="n", xlim=c(0,25))
dev.off()

# now make one for each region on a 3x3 grid
png(filename = "Figures/duration_vs_FRP_regional.png", height=16.7, width=16.7, 
    units="in", res=100)

par(mfrow=c(3,3), las=1, mar=c(4,6,3,4))
for (r in desiredRegions){
  
  labMask <- r==desiredRegions
  df_r <- df[df$region== r,]
  
  plot(df_r$Dur, df_r$mean_frp, col=df_r$regionCol, pch=df_r$pch,
       ylab="", xlab="",
       bty="n", xlim=c(0,25), font.lab=2,
       cex.axis=2.5,
       cex=2
  )
  niceName <- niceNames[labMask]
  title(niceName, cex.main=3)
  
}
dev.off()


################################################################################
# modis_per_hp_regional
################################################################################
# Show % hysplit point with no modis, this is value added plot, probably lead 
# with this one
png(filename="Figures/modis_per_hp_regional.png", height=16.7, width=16.7, 
    units="in", res=1000)

# Want all hms points for this plot
df<-hms_df

par(mfrow=c(3,3), las=1)
for (r in desiredRegions){
  
  labMask <- r==desiredRegions
  df_r <- df[df$region== r,]
  
  propZero <- round(sum(df_r$nWithin4 == 0)/dim(df_r)[1] * 100, 2)
  
  h <- hist(df_r$nWithin4, col=rCol[labMask], font.lab=2, main="", 
       xlab="MODIS per HMS fire", ylab="")
  
  l <- length(h$mids)
  b <- round(.4*l)
  
  #text(h$mids[4], h$counts[1], labels=paste("<-",propZero,"%"))
  
  #legend("topright", legend = paste(propZero,"%"), bty="n")
  
  niceName <- niceNames[labMask]
  title(paste0(niceName,": ",propZero," %"))
  
}
dev.off()


################################################################################
# modis_per_hp_vs_dur_regional.pdf
################################################################################
pdf(file="Figures/modis_per_hp_vs_dur_regional.pdf")

# Want all hms points for this plot
df<-hms_df

par(mfrow=c(3,3), las=1)
for (r in desiredRegions){
  
  labMask <- r==desiredRegions
  df_r <- df[df$region== r,]
  
  plot(df_r$nWithin4, df_r$Dur, col=df_r$regionCol, pch=df_r$pch,
       xlab="MODIS per per HMS fire", ylab="HMS Fire Duration (hrs)",
       bty="n", ylim=c(0,25), font.lab=2
  )
  
  niceName <- niceNames[labMask]
  title(niceName)
  
}
dev.off()

################################################################################
# confidense_regional for detections within 4 km of HP
################################################################################

pdf(file="Figures/confidense_regional.pdf")

# Want all hms points for this plot
m <- hms_df$nWithin4 > 0
df<-hms_df[m,]

par(mfrow=c(3,3), las=1)
for (r in desiredRegions){
  
  labMask <- r==desiredRegions
  df_r <- df[df$region== r,]
  
  h <- hist(df_r$mean_conf , col=rCol[labMask], font.lab=2, main="", 
            xlab="confidense", ylab="")
  
  l <- length(h$mids)
  b <- round(.4*l)
  
  #text(h$mids[4], h$counts[1], labels=paste("<-",propZero,"%"))
  
  #legend("topright", legend = paste(propZero,"%"), bty="n")
  
  niceName <- niceNames[labMask]
  title(paste0(niceName))
  
}
dev.off()

################################################################################
# confidense_monthly_regional for detections within 4 km of HP
################################################################################

pdf(file="Figures/confidense_monthly_regional.pdf")

# Want only those with MODIS nearby
m <- hms_df$nWithin4 > 0
df<-hms_df[m,]

par(mfrow=c(3,3), las=1)
for (r in desiredRegions){
  
  labMask <- r==desiredRegions
  df_r <- df[df$region== r,]
  
  l <- list()
  # Create list of monthly distributions
  for (m in 1:12){
    l[[m]] <- df_r[df_r$month == m,]$mean_conf
  }
  
  boxplot(l,  col=rCol[labMask], frame=FALSE, outline=TRUE,
          names=1:12, pars=list(bty="n") )
  
  niceName <- niceNames[labMask]
  title(paste0(niceName))
  
}
dev.off()

################################################################################
# modis_per_hp_monthly_regional 
################################################################################

pdf(file="Figures/modis_per_hp_monthly_regional.pdf")

# Want all hms points for this plot
df<-hms_df

par(mfrow=c(3,3), las=1)
for (r in desiredRegions){
  
  labMask <- r==desiredRegions
  df_r <- df[df$region== r,]
  
  propZero <- round(sum(df_r$nWithin4 == 0)/dim(df_r)[1] * 100, 2)
  
  l <- list()
  # Create list of monthly distributions
  for (m in 1:12){
    l[[m]] <- df_r[df_r$month == m,]$nWithin4
  }
  
  boxplot(l,  col=rCol[labMask], frame=FALSE, outline=FALSE,
          names=1:12, pars=list(bty="n") )
  
  niceName <- niceNames[labMask]
  title(paste0(niceName,": ", propZero, " %"))
  
}
dev.off()

