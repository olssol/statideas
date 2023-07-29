fun_beta_binomial_desity = function(x,n,alpha,beta,cut,prior_post){

  if (prior_post=="prior"){
    p_prior_mean = round(alpha/(alpha+beta),3)
    cum_f = round(1-pbeta(cut,alpha,beta),3)
    graph_labels = c(bquote("With Prior Mean of"~p==.(p_prior_mean)),bquote(Pr(p>=.(cut))==.(cum_f)))
    ylab = "Prior Density"
  }
  if (prior_post=="post"){
    p_post_mean = round((alpha+x)/(alpha+beta+n),3)
    cum_f = round(1-pbeta(cut,alpha+x,beta+n-x),3)
    graph_labels = c(bquote("With Posterior Mean of"~p==.(p_post_mean)),bquote(Pr(p>=.(cut)~"| data")==.(cum_f)))
    ylab = "Posterior Density"
  }

  p = seq(0,1,0.001)
  if (prior_post=="prior") f = dbeta(p,alpha,beta)
  if (prior_post=="post") f = dbeta(p,alpha+x,beta+n-x)
  data = data.frame(p=p,f=f,group="1",group2="1")
  data_2 = data.frame(p=p[p>=cut],f=f[p>=cut],group="2",group2="2")
  data = data[data$f!=Inf,]
  data_2 = data_2[data_2$f!=Inf,]

  pdf = ggplot(data,aes(x=p,y=f,fill=group,color=group2)) +
    geom_ribbon(data_2,mapping=aes(ymax=f,fill=group,color=group2),ymin=0,alpha=0.5) +
    geom_line() +
    scale_color_manual(values=c("black","white"),name="",labels=graph_labels) +
    scale_fill_manual(values=c("white","red"),name="",labels=graph_labels) +
    labs(x="DLT Rate (p)",y=ylab) +
    theme_bw() + theme(legend.position="top",text=element_text(size=20))

  return(pdf)
}



fun_kernel_density = function(x,cut,xlab_name,para_notation,percent){

  cum_prob_L = round(mean(x<cut[1]),3)
  cum_prob_M = round(mean(x>=cut[1] & x<=cut[2]),3)
  cum_prob_U = round(mean(x>cut[2]),3)
  if (percent=="N"){
    post_mean = round(mean(x),3)
    graph_labels = c(bquote("Density with"~E(.(para_notation)~"|"~bold(y))==.(post_mean)),
                     bquote(P(.(para_notation)<.(cut[1])~"|"~bold(y))==.(cum_prob_L)),
                     bquote(P(.(cut[1])<={.(para_notation)<=.(cut[2])}~"|"~bold(y))==.(cum_prob_M)),
                     bquote(P(.(para_notation)>.(cut[2])~"|"~bold(y))==.(cum_prob_U)) )
  }
  if (percent=="Y"){
    post_mean = round(mean(x),1)
    graph_labels = c(bquote("Density with"~E(.(para_notation)~"|"~bold(y))==.(post_mean)*"%"),
                     bquote(P(.(para_notation)<.(cut[1])*"% |"~bold(y))==.(cum_prob_L)),
                     bquote(P(.(cut[1])*"%"<={.(para_notation)<=.(cut[2])}*"% |"~bold(y))==.(cum_prob_M)),
                     bquote(P(.(para_notation)>.(cut[2])*"% |"~bold(y))==.(cum_prob_U)) )
  }

  color = c("black","white","white","white")
  fill = c("white","yellow3","green4","red3")

  den = density(x)
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


fun_hist = function(x,cut,xlab_name,para_notation,percent){

  cum_prob_L = round(mean(x<cut[1]),3)
  cum_prob_M = round(mean(x>=cut[1] & x<=cut[2]),3)
  cum_prob_U = round(mean(x>cut[2]),3)
  if (percent=="N"){
    post_mean = round(mean(x),3)
    graph_labels = c(bquote("Distribution with"~E(.(para_notation)~"|"~bold(y))==.(post_mean)),
                     bquote(P(.(para_notation)<.(cut[1])~"|"~bold(y))==.(cum_prob_L)),
                     bquote(P(.(cut[1])<={.(para_notation)<=.(cut[2])}~"|"~bold(y))==.(cum_prob_M)),
                     bquote(P(.(para_notation)>.(cut[2])~"|"~bold(y))==.(cum_prob_U)) )
  }
  if (percent=="Y"){
    post_mean = round(mean(x),1)
    graph_labels = c(bquote("Density Distribution"~E(.(para_notation)~"|"~bold(y))==.(post_mean)*"%"),
                     bquote(P(.(para_notation)<.(cut[1])*"% |"~bold(y))==.(cum_prob_L)),
                     bquote(P(.(cut[1])*"%"<={.(para_notation)<=.(cut[2])}*"% |"~bold(y))==.(cum_prob_M)),
                     bquote(P(.(para_notation)>.(cut[2])*"% |"~bold(y))==.(cum_prob_U)) )
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
    labs(x=xlab_name) +
    theme_bw() + theme(legend.position="top",text=element_text(size=20)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE),color=guide_legend(nrow=2,byrow=TRUE))
  return(Hist)
}
