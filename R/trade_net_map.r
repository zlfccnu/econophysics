#' plot the trade network on map and color the country according to the core and periphery 
#' @param net the trade network
#' @param core_node_col the core node color determined by the coreness ranking
#' @param periphery_node_col the color of the periphery node
#' @param mode the type of the strength
#' @return a ggplot object
#' @export
trade_net_map=function(net,core_node_col="red",periphery_node_col="orange",mode=c("all","out","in")){
  ## detect community for network
  mem=as.integer(membership(infomap.community(as.undirected(net))))
  mem_size=max(unique(mem))
  colorsCodes=bind_cols(id=1:mem_size,color=colorRampPalette(c("blue", "red"))(mem_size))
  mygroup1<-bind_cols(mem=mem)
  mygroup1<- left_join(mygroup1,colorsCodes,by=c("mem"="id"))
  net<- activate(net,nodes) %>% mutate(degree=degree(net),mygroup=mygroup1$color,strength=strength(net,mode = mode))
  ## set up the map color according to the coreness
  world_data<- map_data("world")
  world_data<- mutate(world_data,color="#f9f9f9")
  world_data<- mutate(world_data,iso=countrycode(region,origin = "country.name",destination = "iso3c"))
  ## should modify to use another core algorithm
  core_rank<- bind_cols(vertex=V(net)$name,core_rank=coreness(net,mode = mode))
  core_rank<- mutate(core_rank,core_rank=ifelse(core_rank==max(core_rank),core_node_col,periphery_node_col))
  core_rank<- mutate(core_rank,iso=countrycode(vertex,origin = "country.name",destination = "iso3c"))
  world_data<- left_join(world_data,core_rank,by=c("iso"="iso"))
  world_data<- mutate(world_data,color=ifelse(!is.na(core_rank),core_rank,color))
  
  world<- ggplot(world_data, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group),fill=world_data$color, color = "grey65", size = 0.2)
  
  ## the plot function
  ggnet2map<- function(gg, net, size = 3, alpha = 0.75, weight, node.group, node.color = NULL, node.alpha = NULL, ring.group, segment.alpha = NULL, segment.color = "grey", great.circles = FALSE, segment.size = 0.25, arrow.size = 0, label.nodes = FALSE, label.size = size/2, ...) {
    require_namespaces<- function (pkgs) 
    {
      for (pkg in pkgs) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          stop(str_c("please install the package '", pkg, 
                     "'.  install.packages('", pkg, "') "))
        }
      }
    }
    require_namespaces(c("network", "sna"))
    if (class(net) == "igraph" && "intergraph" %in% rownames(installed.packages())) {
      net = intergraph::asNetwork(net)
    }
    else if (class(net) == "igraph") {
      stop("install the 'intergraph' package to use igraph objects with ggnet")
    }
    if (!network::is.network(net)) {
      net = try(network::network(net), silent = TRUE)
    }
    if (!network::is.network(net)) {
      stop("could not coerce net to a network object")
    }
    get_v = utils::getFromNamespace("%v%", ns = "network")
    vattr = network::list.vertex.attributes(net)
    is_dir = ifelse(network::is.directed(net), "digraph", "graph")
    if (!is.numeric(arrow.size) || arrow.size < 0) {
      stop("incorrect arrow.size value")
    }
    else if (arrow.size > 0 & is_dir == "graph") {
      warning("network is undirected; arrow.size ignored")
      arrow.size = 0
    }
    if (network::is.hyper(net)) {
      stop("ggnetworkmap cannot plot hyper graphs")
    }
    if (network::is.multiplex(net)) {
      stop("ggnetworkmap cannot plot multiplex graphs")
    }
    if (network::has.loops(net)) {
      warning("ggnetworkmap does not know how to handle self-loops")
    }
    labels = label.nodes
    inherit <- function(x) ifelse(is.null(x), alpha, x)
    m <- network::as.matrix.network.adjacency(net)
    if (missing(gg)) {
      if (!("mapproj" %in% installed.packages())) {
        require_namespaces("mapproj")
      }
      gg <- ggplot() + coord_map()
      plotcord <- sna::gplot.layout.fruchtermanreingold(net, 
                                                        list(m, layout.par = NULL))
      plotcord <- data.frame(plotcord)
      colnames(plotcord) = c("lon", "lat")
    }
    else {
      plotcord = data.frame(lon = as.numeric(get_v(net, "lon")), 
                            lat = as.numeric(get_v(net, "lat")))
    }
    if (!is.logical(labels)) {
      stopifnot(length(labels) == nrow(plotcord))
      plotcord$.label <- labels
    }
    else if ("id" %in% vattr) {
      plotcord$.label <- as.character(get_v(net, "id"))
    }
    else if ("vertex.names" %in% vattr) {
      plotcord$.label <- network::network.vertex.names(net)
    }
    point_aes <- list(x = substitute(lon), y = substitute(lat))
    point_args <- list(alpha = substitute(inherit(node.alpha)))
    if (!missing(node.group)) {
      plotcord$.ngroup <- get_v(net, as.character(substitute(node.group)))
      if (missing(ring.group)) {
        point_aes$color = substitute(.ngroup)
      }
      else {
        point_aes$fill = substitute(.ngroup)
      }
    }
    else if (!missing(node.color)) {
      point_args$color <- substitute(node.color)
    }
    else {
      point_args$color <- substitute("black")
    }
    if (!missing(ring.group)) {
      plotcord$.rgroup <- get_v(net, as.character(substitute(ring.group)))
      point_aes$color <- substitute(.rgroup)
      point_args$pch <- substitute(21)
    }
    edges <- network::as.matrix.network.edgelist(net)
    edges <- data.frame(lat1 = plotcord[edges[, 1], "lat"], 
                        lon1 = plotcord[edges[, 1], "lon"], lat2 = plotcord[edges[, 
                                                                                  2], "lat"], lon2 = plotcord[edges[, 2], "lon"])
    edges <- subset(na.omit(edges), (!(lat1 == lat2 & lon2 == 
                                         lon2)))
    edge_args <- list(size = substitute(segment.size), alpha = substitute(inherit(segment.alpha)), 
                      color = substitute(segment.color))
    edge_aes <- list()
    if (!missing(arrow.size) & arrow.size > 0) {
      edge_args$arrow <- substitute(arrow(type = "closed", 
                                          length = unit(arrow.size, "cm")))
    }
    if (great.circles) {
      require_namespaces("geosphere")
      pts <- 25
      i <- 0
      edges <- ddply(.data = edges, .variables = c("lat1", 
                                                   "lat2", "lon1", "lon2"), .parallel = FALSE, .fun = function(x) {
                                                     p1Mat <- x[, c("lon1", "lat1")]
                                                     colnames(p1Mat) <- NULL
                                                     p2Mat <- x[, c("lon2", "lat2")]
                                                     colnames(p2Mat) <- NULL
                                                     inter <- geosphere::gcIntermediate(p1 = p1Mat, p2 = p2Mat, 
                                                                                        n = pts, addStartEnd = TRUE, breakAtDateLine = TRUE)
                                                     if (!is.list(inter)) {
                                                       i <<- i + 1
                                                       inter <- data.frame(inter)
                                                       inter$group <- i
                                                       return(inter)
                                                     }
                                                     else {
                                                       if (is.matrix(inter[[1]])) {
                                                         i <<- i + 1
                                                         ret <- data.frame(inter[[1]])
                                                         ret$group <- i
                                                         i <<- i + 1
                                                         ret2 <- data.frame(inter[[2]])
                                                         ret2$group <- i
                                                         return(rbind(ret, ret2))
                                                       }
                                                       else {
                                                         ret <- data.frame(lon = numeric(0), lat = numeric(0), 
                                                                           group = numeric(0))
                                                         for (j in 1:length(inter)) {
                                                           i <<- i + 1
                                                           ret1 <- data.frame(inter[[j]][[1]])
                                                           ret1$group <- i
                                                           i <<- i + 1
                                                           ret2 <- data.frame(inter[[j]][[2]])
                                                           ret2$group <- i
                                                           ret <- rbind(ret, ret1, ret2)
                                                         }
                                                         return(ret)
                                                       }
                                                     }
                                                   })
      edge_aes$x = substitute(lon)
      edge_aes$y = substitute(lat)
      edge_aes$group = substitute(group)
      edge_args$data = substitute(edges)
      edge_args$mapping <- do.call(aes, edge_aes)
      gg <- gg + do.call(geom_path, edge_args)
    }
    else {
      edge_aes$x = substitute(lon1)
      edge_aes$y = substitute(lat1)
      edge_aes$xend = substitute(lon2)
      edge_aes$yend = substitute(lat2)
      edge_args$data <- substitute(edges)
      edge_args$mapping = do.call(aes, edge_aes)
      gg <- gg + do.call(geom_segment, edge_args)
    }
    sizer <- NULL
    if (missing(weight)) {
      point_args$size <- substitute(size)
    }
    else {
      plotcord$.weight = get_v(net, as.character(substitute(weight)))
      if (is.factor(plotcord$.weight)) {
        sizer <- scale_size_discrete(name = substitute(weight), 
                                     range = c(size/nlevels(plotcord$weight), size))
      }
      else {
        sizer <- scale_size_area(name = substitute(weight), 
                                 max_size = size)
      }
      point_aes$size <- substitute(.weight)
    }
    point_args$data <- substitute(plotcord)
    point_args$mapping <- do.call(aes, point_aes)
    gg = gg + do.call(geom_point, point_args)
    if (!is.null(sizer)) {
      gg = gg + sizer
    }
    if (isTRUE(labels)) {
      gg <- gg + ggrepel::geom_text_repel(data = plotcord, aes(x = lon, y = lat, 
                                                               label = .label), size = label.size, ...)
    }
    gg = gg + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) + 
      labs(color = "", fill = "", size = "", y = NULL, x = NULL) + 
      theme(panel.background = element_blank(), legend.key = element_blank())
    return(gg)
  }
  
  p <- ggnet2map(world, as.igraph(net), size = 10, great.circles = FALSE,ring.group = degree, weight = strength,node.group = mygroup,node.color = NA,label.nodes = TRUE,label.size = 3)
  return(p)
}