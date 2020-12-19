#' plot the chord diagram for the uncomtrade among different continent from a network
#' @param GRAPH a tidygraph object
#' @param dat Logical value, whether return the trade flow data or not
#' @param weight_factor, the denominator to scale the trade volume
#' @return a plot
#' @export
chordDiagramFromNet=function(GRAPH,dat=FALSE,weight_factor=1){
  library("conflicted")
  conflict_prefer("filter", "dplyr")
  stopifnot(is.tbl_graph(GRAPH))
  ### construct the trade flow dataframe
  continent_trade_from_net<-  GRAPH %>% activate(edges) %>% filter(from_name!=to_name) %>% as_tibble() %>% group_by(from_continent,to_continent) %>% summarise(trade_volume=sum(weight)/weight_factor) %>% ungroup() %>% rename(source=from_continent,target=to_continent)
  
  col1=c("#33BEB7","#B2C224","#FECC2F","#FBA127","#DB3937","#A463D7","#0C5BCE")
  reg1=c("Asia","Europe","Africa","Oceania","North","South","Middle")
  reg2=c(NA,NA,NA,NA,"America","America","East")
  region=c("Asia","Europe","Africa","Oceania","North America","South America","Middle East")
  d1=bind_cols(region=region,order1=2:8,col1=col1,reg1=reg1,reg2=reg2)
  
  circos.clear()
  par(mar = rep(3, 4), cex=0.8)
  circos.par(start.degree = 90, track.margin=c(-0.1, 0.1), 
             gap.degree = 2, points.overflow.warning = FALSE)
  
  # plot the chord diagram
  chordDiagram(x = continent_trade_from_net, directional = 1, order = d1$region,
               grid.col = d1$col1, annotationTrack = "grid",
               transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1),
               direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
               diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE)
  
  # add labels and axis
  circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    adjust=sum(continent_trade_from_net$trade_volume)/100## the position of second label
    sector.index = get.cell.meta.data("sector.index")
    reg1 = d1 %>% dplyr::filter(region == sector.index) %>% pull(reg1)
    reg2 = d1 %>% dplyr::filter(region == sector.index) %>% pull(reg2)
    circos.text(x = mean(xlim)+adjust, y =3.5,labels = reg1, cex = 1,niceFacing = TRUE,facing = "clockwise",font=2)
    circos.text(x = mean(xlim), y =3.5,labels = reg2, cex = 1,niceFacing = TRUE,facing = "clockwise",font=2)
    circos.axis(h = "top", labels.cex = 1,labels.niceFacing = TRUE, labels.pos.adjust = TRUE)
  })
  
  if(isTRUE(dat)){
    return(continent_trade_from_net)
  }
}
