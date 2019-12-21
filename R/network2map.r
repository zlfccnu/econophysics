#' plot network on world map
#' @param GRAPH an igraph object
#' @param node_size the predefined node size for the plot, maybe determined inside the func
#' @param node_color the predefined node color for the plot, maybe determined inside the func
#' @param inter_com_size inter community link size
#' @param inter_com_color inter community link color
#' @param label_color the color of the node label
#' @param label_size the size of the node label
#' @param hue the central color of the random generate color code
#' @param luminosity the color type, "random", "light", "bright", "dark"
#' @param inter_com_color the inter community link colors
#' @param inter_com_type the inter community link type
#' @param random_color the way to generate color, random or not
#' @return an ggplot object
#' @export
network2map=function(GRAPH,node_size=NULL,node_color=NULL,inter_com_size=0.5,inner_com_size=0.8,label_color=NULL,label_size=NULL,hue="blue",luminosity="bright",inter_com_color="red",inter_com_type=2,random_color=FALSE){
  library(ggrepel)
  ## prepare the data for plot
  mem=as.vector(membership(walktrap.community(GRAPH)))
  ## set the node color according to the community partition
  if(isTRUE(random_color)){
    node_color=distinctColorPalette(k=max(mem))[mem]##
  }else{
    node_color=randomColor(count = max(mem),hue =hue,luminosity = luminosity)[mem]
  }
  nodes1=data.frame(id=1:vcount(GRAPH),lon=V(GRAPH)$lon,lat=V(GRAPH)$lat,name=V(GRAPH)$name,mem=mem,node_size=round(log10(strength(GRAPH))),node_color=node_color,label_size=round(log10(strength(GRAPH))),label_color=node_color,stringsAsFactors = FALSE)
  rownames(nodes1)=NULL
  nodes1=mutate(nodes1,mem=factor(mem))
  
  
  ## if the node size, node color, label size, label color are predefined in the args
  ## list, they will be modified according to the args list
  if(!is.null(node_size)){
    nodes1$node_size=node_size
  }
  
  if(!is.null(node_color)){
    nodes1$node_color=node_color
  }
  if(!is.null(label_size)){
    nodes1$label_size=label_size## the default label size
  }
  
  if(!is.null(label_color)){
    nodes1$label_color=rep(label_color,vcount(GRAPH))## the default label color
  }
  
  edges1=data.frame(from = get.edgelist(GRAPH,names = FALSE)[,1], to = get.edgelist(GRAPH,names = FALSE)[,2], weight = E(GRAPH)$weight,stringsAsFactors = FALSE,line_color=rep(inter_com_color,ecount(GRAPH)),line_type=inter_com_type,line_size=inter_com_size)
  
  ## set the inner community line color
  for(i in 1:dim(edges1)[1]){
    if(nodes1[edges1[i,"from"],"mem"]==nodes1[edges1[i,"to"],"mem"]){
      edges1[i,"line_color"]=nodes1[edges1[i,"to"],"node_color"]
    }
  }
  ## set the inner community line type
  for(i in 1:dim(edges1)[1]){
    if(nodes1[edges1[i,"from"],"mem"]==nodes1[edges1[i,"to"],"mem"]){
      edges1[i,"line_type"]=1
    }
  }
  
  ## set the inner community line size
  for(i in 1:dim(edges1)[1]){
    if(nodes1[edges1[i,"from"],"mem"]==nodes1[edges1[i,"to"],"mem"]){
      edges1[i,"line_size"]=inner_com_size
    }
  }
  
  edges_for_plot1 <- edges1 %>%
    inner_join(nodes1 %>% dplyr::select(id,lon,lat), by = c('from' = 'id')) %>%
    rename(x = lon, y = lat) %>%
    inner_join(nodes1 %>% dplyr::select(id, lon, lat), by = c('to' = 'id')) %>%
    rename(xend = lon, yend = lat)
  
  ## prepare the map
  maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "bottom") +
    theme(panel.grid = element_blank()) +
    theme(panel.background = element_rect(fill = "#dbdee0")) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'cm'))
  
  country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                                 data = map_data('world'),
                                 fill = "#CECECE", color = "#515151",
                                 size = 0.15)
  mapcoords <- coord_fixed(xlim = c(-180, 180), ylim = c(-50, 100))
  ## ggplot 
  CountryPlot=ggplot(nodes1)
  CountryPlot=CountryPlot+ country_shapes
  CountryPlot=CountryPlot+geom_curve(data = edges_for_plot1,aes(x = x, y = y, xend = xend, yend = yend),color=edges_for_plot1$line_color, curvature = 0.33,alpha = 0.9,angle =60, show.legend=FALSE,ncp=10,linetype=edges_for_plot1$line_type,size=edges_for_plot1$line_size,arrow = arrow(angle = 10,length = unit(0.2,"inches"),type="closed"))
  
  # CountryPlot=CountryPlot+scale_size_continuous(guide = FALSE, range = c(0.25, 2))
  
  CountryPlot=CountryPlot+geom_point(data = nodes1,aes(x = lon, y = lat,size=node_size),fill=nodes1$node_color, stroke = 0.5,shape=21,color="white")
  
  CountryPlot=CountryPlot+scale_size_continuous(guide = FALSE, range = c(1, max(round(log10(strength(GRAPH)))))) 
  CountryPlot=CountryPlot+ geom_text_repel(data = nodes1,aes(x = lon, y = lat, label = name),color=nodes1$label_color,size=nodes1$label_size,nudge_x = 1,nudge_y =1, fontface = "bold")
  CountryPlot=CountryPlot+mapcoords 
  CountryPlot=CountryPlot+ maptheme
  return(CountryPlot)
}