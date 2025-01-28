library(tidyverse)
library(cowplot)

feature_df=read.csv("feature_data.csv")
feature_df

plotlist1=list()
for(f in unique(feature_df$Feature)){
  #f="Pos_code"

  tmp=feature_df |> 
    filter(Feature==f)

  plot=if(tmp$Type[1]=="Numerical"){
    tmp |> 
      ggplot(aes(x=Value,y=Score,ymin=lower,ymax=upper))+
      geom_step(linewidth=2,color="blue")+
      geom_hline(yintercept = 0,linetype="dashed",linewidth=1)+
      theme_minimal()+
      xlab(f)+
      labs(title = f)
    }else{

      tmp |> 
        ggplot(aes(x=Category,y=Score))+
        geom_col(fill="blue",stat = "value")+
        geom_hline(yintercept = 0,linetype="dashed",linewidth=1)+
        theme_minimal()+
        xlab(f)+
        labs(title = f)

  }
  plot
  plotlist1[[f]]=plot
  }
plotlist1[4]

plot_grid(plotlist = plotlist1)
cowplot::save_plot(plot_grid(plotlist = plotlist1),file="ebm.png",base_height=10,base_width=10,dpi=300)


# rocs
ebm_roc=data.frame()
for(i in 1:4){
tmp=read.csv(file=paste0("roc_curve_ebm_",i,".csv"))
  ebm_roc=rbind(ebm_roc,tmp)
}
ebm_roc



frisk_roc=data.frame()
for(i in 1:4){
tmp=read.csv(file=paste0("roc_curve_frisk_",i,".csv"))
frisk_roc=rbind(frisk_roc,tmp)
}
frisk_roc





gbm_roc=data.frame()
for(i in 1:4){
tmp=read.csv(file=paste0("test_roc_",i,".csv"), sep = ";", dec = ",")
gbm_roc=rbind(gbm_roc,tmp)
}
gbm_roc
colnames(gbm_roc)=c("X","thresholds","tnr","true_positive_rate","false_positive_rate")
gbm_roc=gbm_roc |> select(-tnr)




# We follow threshold averaging, see https://openreview.net/pdf?id=FByH3qL87G
# for that, we first create 10 thresholds per model
roc_comparison=frisk_roc |> mutate(thresholds=ntile(thresholds,10)) |> 
  group_by(thresholds) |> 
  mutate(true_positive_rate=mean_se(true_positive_rate)) |> unnest() |> 
  distinct(thresholds,y,ymin,ymax) |> 
  left_join(
    frisk_roc |> mutate(thresholds=ntile(thresholds,10)) |> 
  group_by(thresholds) |> 
  mutate(false_positive_rate=mean_se(false_positive_rate)) |> unnest() |> 
  rename(x=y,xmin=ymin,xmax=ymax) |> 
      select(thresholds,x,xmax,xmin)
    ,by="thresholds"
  ) |> ungroup() |> 
  add_row(x=1,y=1)|> distinct() |> mutate(model="fasterrisk") |> 
  bind_rows(
    ebm_roc |> mutate(thresholds=ntile(thresholds,10)) |> 
  group_by(thresholds) |> 
  mutate(true_positive_rate=mean_se(true_positive_rate)) |> unnest() |> 
  distinct(thresholds,y,ymin,ymax) |> 
  left_join(
    ebm_roc |> mutate(thresholds=ntile(thresholds,10)) |> 
  group_by(thresholds) |> 
  mutate(false_positive_rate=mean_se(false_positive_rate)) |> unnest() |> 
  rename(x=y,xmin=ymin,xmax=ymax) |> 
      select(thresholds,x,xmax,xmin)
    ,by="thresholds"
  ) |> ungroup() |> 
  add_row(x=1,y=1)|> 
  add_row(x=0,y=0)|> distinct() |> mutate(model="EBM")
  ) |> 
  bind_rows(gbm_roc |> mutate(thresholds=ntile(thresholds,10)) |> 
    group_by(thresholds) |> 
    mutate(true_positive_rate=mean_se(true_positive_rate)) |> unnest() |> 
    distinct(thresholds,y,ymin,ymax) |> 
    left_join(
      gbm_roc |> mutate(thresholds=ntile(thresholds,10)) |> 
    group_by(thresholds) |> 
    mutate(false_positive_rate=mean_se(false_positive_rate)) |> unnest() |> 
    rename(x=y,xmin=ymin,xmax=ymax) |> 
        select(thresholds,x,xmax,xmin)
      ,by="thresholds"
    ) |> ungroup() |> 
    add_row(x=1,y=1)|> 
    add_row(x=0,y=0)|> distinct() |> mutate(model="GBM")) |> 
  ggplot(aes(x=x,y=y,ymin=ymin,ymax=ymax,color=model))+
  #geom_pointrange()+
  geom_line(linewidth=1.5)+
  #geom_ribbon()
  #geom_step(aes(x=false_positive_rate,y=ymax),color="blue")+
  #geom_step(aes(x=false_positive_rate,y=ymin),color="red")+
  geom_abline(slope = 1, linetype="dashed")+
  scale_color_manual(values = c("#804AB7","#EB5F2A","black"), name="Model") +
  theme_minimal()+
  theme(legend.position = "bottom")+
  xlab("False Positive Rate")+
  ylab("True Positive Rate")
roc_comparison

ggsave("roc_comparison.png",roc_comparison,height = 7,width = 6.5,dpi=300)
