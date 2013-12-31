if(!require("ggplot2")){
  install.packages("ggplot2")
  if(!require("ggplot2")){
    stop("Error! Could not install ggplot2!")
  }
}

get.char.offset = function(char, offset, font.dict){
  char.as.list = font.dict[[char]]
  char.matrix = do.call(rbind, char.as.list)
  char.matrix = cbind(char.matrix[,1]+offset, char.matrix[,2])
  new.offset = offset+length(char.as.list)/6+1
  return(list(char.matrix, new.offset))
}

plot_hist = function(input.string, out.file){
  input.string = tolower(input.string)
  char.vect = strsplit(input.string, '')[[1]]
  
  offset = 0
  big.matrix = matrix(nrow=0, ncol=2)
  load("charlist.RData")
  font.dict = char.list # loaded in the previous line
  
  for(i in char.vect){
    if(i==" "){
      offset = offset+5
    } else{
      ret.val = get.char.offset(i, offset, font.dict)
      big.matrix = rbind(big.matrix, ret.val[[1]])
      offset = ret.val[[2]]
    }
  }
  
  dfHist = as.data.frame(big.matrix)
  
  hist_cut = ggplot(dfHist, aes(x=V1, fill=as.factor(V2)))
  hist_cut = hist_cut + geom_histogram(binwidth=1)
  hist_cut = hist_cut + scale_fill_manual(values=c("#000000", "#FFFFFF", "#000000", "#FFFFFF", "#000000","#FFFFFF", "#000000" ))
  hist_cut = hist_cut + theme_bw()
  hist_cut = hist_cut + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  png(out.file, width=700, height=100)
  print(hist_cut)
  dev.off()
}

main = function(){
  args<-commandArgs(TRUE)
  input.string = args[1]
  #print("Enter the string you want printed in the histogram, then hit Enter:")
  #input.string = readline()
  out.file = "hist.png"
  plot_hist(input.string, out.file)
}

main()