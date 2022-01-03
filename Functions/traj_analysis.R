traj_analysis <- function(data1)
{
  itinerary <- cbind(data1$room, data1$timediff)
  traj <- c()
  bloc <- 1 
  bloc.size <- 1
  dd <- c(1,itinerary[1, 1], 1, itinerary[1,2] )
  time.spent <- 0
  indx.points.in.bloc <- list()
  indx.points.in.bloc[[1]] <- 1
  # for(i in 2:19){
  for(i in 2:dim(data1)[1]){
    # i=20
    if(itinerary[i,1]==itinerary[i-1, 1]){
      bloc <- bloc
      bloc.size <- bloc.size +1
      indx.points.in.bloc[[bloc]] <- c(indx.points.in.bloc[[bloc]], i)
      
      time.spent <- time.spent+ as.numeric(itinerary[i-1, 2])
      dd <- c(bloc, itinerary[i-1, 1], bloc.size, time.spent)
    }else{
      traj <- rbind(traj, dd)
      bloc <- bloc+1
      bloc.size <-1 
      time.spent <- 0
      dd <- c(bloc, itinerary[i,1], bloc.size, time.spent)
      indx.points.in.bloc[[bloc]] <- c(i)
    }
  }
  if(itinerary[dim(data1)[1],1]!=itinerary[(dim(data1)[1]-1), 1]){
    traj <- rbind(traj, dd)
    bloc <- bloc+1
    bloc.size <-1
    time.spent <- 0
    dd <- c(bloc, itinerary[dim(data1)[1],1], bloc.size, time.spent)
    # indx.points.in.bloc[[bloc]] <- c(i)
  }
  colnames(traj) <- c("bloc", "room","bloc size", "time spent ")
  traj=as.data.frame(traj)
  # names(indx.points.in.bloc) <- c(1:length(indx.points.in.bloc))
  
  return(list(traj=traj, indx.points.in.bloc=indx.points.in.bloc ))
}
# --------------------------------------------------------  

# rooms <- traj.room(itinerary)
# length(rooms$indx.points.in.bloc)
# traj <- as.data.frame(rooms$traj)
# View(traj)
 