continuous_SubTraj <- function(data0, time_interval){
  # Input:
      # data0
      # time_interval: time_interval=c("2021-12-03 07:00:00", "2021-12-03 08:00:00" ) 
  # Output:

  require(raster) # for distance calculation
  
  
  # Extract sub_data
  x1 <- intersect(which(data0$date>= time_interval[1]), which(data0$date<= time_interval[2]))
  data1 <- data0[x1,]

  cat("Number of observaions:", dim(data1)[1], "\n")
  na_data <- which(is.na(data1$x)==TRUE) 
  cat("Number of NA:", length(na_data), "\n")

  
  # Elimination of redundancies
  indx_redund <- which(data1$timediff==0)[-1]
  cat("the % of redundancies (timediff==200ms):", (length(indx_redund)-1)/dim(data1)[1]*100,"%", "\n")
  # head(data1[indx_redund, ], 10)
  data1 <- data1[-indx_redund , ]
  indx_partial_redund <- which(data1$timediff<0.1)[-1]
  cat("The % of partial redundancies (timediff<<200ms):", (length(indx_partial_redund)/dim(data1)[1])*100,"%", "\n")
  # head(data1[indx_partial_redund, ], 10)
  data1 <- data1[-indx_partial_redund, ]
  cat("Number of observations with no redundacies:", dim(data1)[1], "\n")
  
  
  # distance and vilocity computation
  data1$dist_parcourue <- c()
  data1$dist_parcourue[1] <- 0
  for(i in 2:dim(data1)[1]) data1$dist_parcourue[i] <-  pointDistance(c(data1$x[i], data1$y[i]), c(data1$x[i-1], data1$y[i-1]), lonlat = FALSE, allpairs = FALSE)
  data1$Vitesse <- data1$dist_parcourue/data1$timediff
  data1$Vitesse[1] <- 0
  
  

  cat("The % of cases where velocity is zero:", (length(which(data1$Vitesse==0))/dim(data1)[1])*100, "%", "\n" )
  data_non_sensible<-data1[which(data1$Vitesse==0), ]
  
  
  # plot sub_data trajectory
  Wall_lignes_firminy <- read_excel("~/Devellyn/traitement_data position/Firminy installation/Walls_ligne3_Origine(-15,-30).xlsx")
  Wall_lignes_firminy <- as.data.frame(Wall_lignes_firminy)
  Wall_lignes_firminy$`Start X'` <- as.numeric(Wall_lignes_firminy$`Start X'`)/100
  Wall_lignes_firminy$`Start Y'` <- as.numeric(Wall_lignes_firminy$`Start Y'`)/100
  Wall_lignes_firminy$`End X'` <- as.numeric(Wall_lignes_firminy$`End X'`)/100
  Wall_lignes_firminy$`End Y'` <- as.numeric(Wall_lignes_firminy$`End Y'`)/100
  walls <- Wall_lignes_firminy[,7:12]
  colnames(walls) <- c("endx", "endy", "endz", "x", "y", "z")
  
  plan <- ggplot()+ 
    geom_segment(data = walls,  mapping = aes(x = x, y = y, xend=endx, yend=endy))+
    xlim(-80, 80)+
    ylim(-80,80)
  
  data.point <- plan +
    geom_point(data = data1, aes(x, y), size=0.1, color="red")
  print(data.point)
  
  # Extrat continuous sub_trajectories
  indx_discont <- which(data1$timediff>0.25) # indices of points marqued after a gap
  n_gap <-  length(indx_discont) # number of gaps in the sub_data
  cat("Number of gap:",n_gap, "\n")
  
  disc_instant <- data1[indx_discont,] # table of all points after a gap, the plot below illustrates the variation of gap length
  par(mfrow=c(2,1))
    plot(disc_instant$dist_parcourue, type="l", ylab=c("distance (m)"), main=("distance variation of discontiuous data"), col=2)
    plot(disc_instant$timediff, type="l", ylab=c("timediff (s)"), main=("timediff variation of discontiuous data"), col=3)
  par(mfrow=c(1,1))
    plot(disc_instant$timediff, disc_instant$dist_parcourue, xlab=c("timediff (s)"), ylab=c("distance (m)"), main=("gap convergence"))
  
  
  cat("Number of continuous subtrajectories:", n_gap+1, "\n")
  
  cont_sub_traj <- list(length(indx_discont)+1) # list of dataframes corresponding to continuous trajectories
  cont_sub_traj[[1]] <- data1[1:(indx_discont[1]-1), ]
  for (i in 2:n_gap) 
    cont_sub_traj[[i]] <- data1[indx_discont[i-1]:(indx_discont[i]-1), ]
  cont_sub_traj[[n_gap+1]] <- data1[indx_discont[n_gap]:dim(data1)[1], ]
  
  leng_sub_traj <- lapply(cont_sub_traj, nrow) # number of points for each continuous sub trajectory

  sub_traj <- 1:length(cont_sub_traj) # table containing statistics on all continuous sub-traj
  sub_traj <- as.data.frame(sub_traj)
  sub_traj$n_point <- unlist(leng_sub_traj)
  sub_traj$duration <-c()
  sub_traj$distance <- c()
  sub_traj$time_gap <- c()
  for(i in 1: length(cont_sub_traj)){
    sub_traj$duration[i] <- sum(cont_sub_traj[[i]]$timediff)
    sub_traj$distance[i] <- sum(cont_sub_traj[[i]]$dist_parcourue)
    sub_traj$time_gap[i] <- cont_sub_traj[[i]]$timediff[1]
  }
  
  return(list(sub_traj=sub_traj, cont_sub_traj=cont_sub_traj, indx_discont=indx_discont, disc_instant=disc_instant, data_non_sensible=data_non_sensible, data1=data1))
  
}