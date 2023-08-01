#' Function
#'
#' @export
#'
fun_kernel_density = function(x,cut,xlab_name,para_notation,percent,from,to){
  
  cum_prob_L = round(mean(x<cut[1]),3)
  cum_prob_M = round(mean(x>=cut[1] & x<=cut[2]),3)
  cum_prob_U = round(mean(x>cut[2]),3)
  if (percent=="N"){
    post_mean = round(mean(x),3)
    graph_labels = c(bquote("With Mean of"~.(para_notation)==.(post_mean)),
                     bquote(P(.(para_notation)<.(cut[1]))==.(cum_prob_L)),
                     bquote(P(.(cut[1])<={.(para_notation)<=.(cut[2])})==.(cum_prob_M)),
                     bquote(P(.(para_notation)>.(cut[2]))==.(cum_prob_U)) )
  }
  if (percent=="Y"){
    post_mean = round(mean(x),1)
    graph_labels = c(bquote("With Mean of"~.(para_notation)*"%"==.(post_mean)*"%"),
                     bquote(P(.(para_notation)*"%"<.(cut[1])*"%")==.(cum_prob_L)),
                     bquote(P(.(cut[1])*"%"<={.(para_notation)*"%"<=.(cut[2])}*"%")==.(cum_prob_M)),
                     bquote(P(.(para_notation)*"%">.(cut[2])*"%")==.(cum_prob_U)) )
  } 
  
  color = c("black","white","white","white")
  fill = c("white","yellow3","green4","red3")
  
  if (is.na(from) & is.na(to)) {
    den = density(x)
  } else if (is.na(from) & !is.na(to)){
    den = density(x,to=to)
  } else if (!is.na(from) & is.na(to)){
    den = density(x,from=from)
  } else {
    den = density(x,from=from,to=to)
  }
  data = data.frame(y=den$x,f=den$y,group="1",group2="1")
  
  data_L = data[data$y<cut[1],]
  data_M = data[data$y>=cut[1] & data$y<=cut[2],]
  data_U = data[data$y>cut[2],]
  
  ii = 0
  if (dim(data_L)[1]==0) {fill=fill[-2-ii]; color=color[-2-ii]; graph_labels=graph_labels[-2-ii]; ii=ii+1} else {data_L$group="2"; data_L$group2="2"}
  if (dim(data_M)[1]==0) {fill=fill[-(3-ii)]; color=color[-3-ii]; graph_labels=graph_labels[-3-ii]; ii=ii+1} else {data_M$group="3"; data_M$group2="3"}
  if (dim(data_U)[1]==0) {fill=fill[-(4-ii)]; color=color[-4-ii]; graph_labels=graph_labels[-4-ii]; ii=ii+1} else {data_U$group="4"; data_U$group2="4"}
  
  data_combine = rbind(data,data_L,data_M,data_U)
  data_combine$group = factor(data_combine$group)
  data_combine$group2 = factor(data_combine$group2)
  
  pdf = ggplot(data,aes(x=y,y=f,fill=group,color=group2)) +
    geom_ribbon(data_L,mapping=aes(ymax=f,fill=group,color=group2),ymin=0) +
    geom_ribbon(data_M,mapping=aes(ymax=f,fill=group,color=group2),ymin=0,show.legend=FALSE) +
    geom_ribbon(data_U,mapping=aes(ymax=f,fill=group,color=group2),ymin=0,show.legend=FALSE) +
    geom_line() +
    scale_color_manual(values=color,name="",labels=graph_labels) +
    scale_fill_manual(values=alpha(fill,0.5),name="",labels=graph_labels) +
    labs(x=xlab_name,y="Density") +
    theme_bw() + theme(legend.position="top",text=element_text(size=20)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE),color=guide_legend(nrow=2,byrow=TRUE)) 
  return(pdf)
}

#' Function
#'
#' @export
#'
fun_hist = function(x,cut,xlab_name,para_notation,percent){
  
  cum_prob_L = round(mean(x<cut[1]),3)
  cum_prob_M = round(mean(x>=cut[1] & x<=cut[2]),3)
  cum_prob_U = round(mean(x>cut[2]),3)
  if (percent=="N"){
    post_mean = round(mean(x),3)
    graph_labels = c(bquote("With Posterior Mean of"~.(para_notation)==.(post_mean)),
                     bquote(P(.(para_notation)<.(cut[1])~"|"~bold(y))==.(cum_prob_L)),
                     bquote(P(.(cut[1])<={.(para_notation)<=.(cut[2])}~"|"~bold(y))==.(cum_prob_M)),
                     bquote(P(.(para_notation)>.(cut[2])~"|"~bold(y))==.(cum_prob_U)) )
  }
  if (percent=="Y"){
    post_mean = round(mean(x),1)
    graph_labels = c(bquote("With Posterior Mean of"~.(para_notation)*"%"==.(post_mean)*"%"),
                     bquote(P(.(para_notation)*"%"<.(cut[1])*"% |"~bold(y))==.(cum_prob_L)),
                     bquote(P(.(cut[1])*"%"<={.(para_notation)*"%"<=.(cut[2])}*"% |"~bold(y))==.(cum_prob_M)),
                     bquote(P(.(para_notation)*"%">.(cut[2])*"% |"~bold(y))==.(cum_prob_U)) )
  }  
  
  color = rep("black",4)
  fill = c("grey","yellow3","green4","red3")
  
  data = data.frame(x=x,group="1")
  data_L = data[x<cut[1],]
  data_M = data[x>=cut[1] & x<=cut[2],]
  data_U = data[x>cut[2],]
  
  ii = 0
  if (dim(data_L)[1]==0) {fill=fill[-2-ii]; color=color[-2-ii]; graph_labels=graph_labels[-2-ii]; ii=ii+1} else data_L$group = "2"
  if (dim(data_M)[1]==0) {fill=fill[-(3-ii)]; color=color[-3-ii]; graph_labels=graph_labels[-3-ii]; ii=ii+1} else data_M$group = "3"
  if (dim(data_U)[1]==0) {fill=fill[-(4-ii)]; color=color[-4-ii]; graph_labels=graph_labels[-4-ii]; ii=ii+1} else data_U$group = "4"
  
  data_combine = rbind(data,data_L,data_M,data_U)
  data_combine$group = factor(data_combine$group)
  
  Hist = ggplot(data_combine,aes(x=x,fill=group,color=group)) + 
    geom_histogram(alpha=0.5,position="identity") +
    scale_color_manual(values=color,name="",labels=graph_labels) +
    scale_fill_manual(values=fill,name="",labels=graph_labels) +
    labs(x=xlab_name,y="Posterior Distribution (Counts)") + 
    theme_bw() + theme(legend.position="top",text=element_text(size=20)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE),color=guide_legend(nrow=2,byrow=TRUE)) 
  return(Hist)
}

