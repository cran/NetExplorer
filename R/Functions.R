###################################################################################
# Copyright (C) 2021  Sebastian Sosa
#
# This file is part of NetExplorer.
#
# NetExplorer is a free software: it can be redistributed and/or modified
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# NetExplorer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
###################################################################################
#' @title Matrix to edge list
#' @description Converts a square adjacency matrix into a data frame with three columns representing an edge list. Columns are: actor, receiver and weight.
#' @param M a square adjacency matrix.
#' @param sym if \emph{TRUE}, it extracts the lower triangle of the matrix only.
#' @param erase.diag if \emph{TRUE}, it omits diagonals.
#' @details This function convert a matrix into an edgelist.
#' @return a data frame representing an edge list.
#' @author Sebastian Sosa
#' @keywords internal
mat.to.edgl <- function(M, sym = FALSE, erase.diag = TRUE) {
  # If argument sym is equal to TRUE----------------------
  if (sym) {
    # If argument erase.diag is equal to TRUE
    if (erase.diag == TRUE) {
      test <- lower.tri(M) # Extract matrix lower triangle
    }
    else {
      test <- lower.tri(M, diag = TRUE) # Extract matrix lower triangle and diagonal
    }
    # Extract matrix cells
    weight <- M[test]
    # Extract cells ids
    tmp <- which(test, arr.ind = TRUE)
    # Create an edgelist if actor, receiver and interactions weights
    DF <- cbind("from" = colnames(M)[tmp[, 1]], "to" = colnames(M)[tmp[, 2]])
    DF <- data.frame(DF, weight)
  }
  # If argument sym is equal to FALSE----------------------
  else {
    if(is.null(colnames(M))){stop("Argument M doesn't have column names.")}
    # Create a vector of actors
    from <- rep(c(colnames(M)), dim(M)[2])
    # Create a vector of receivers
    to <- rep(c(colnames(M)), each = dim(M)[1])
    # Extract matrix cells
    weight <- as.vector(M)
    # Create a data frame of those vectors
    DF <- data.frame(from, to, weight)
    # if argument erase diag is equal to TRUE
    if (erase.diag == TRUE) {
      diagonals <- which(DF$from == DF$to) # Remove case where actor is equal to receiver
      DF <- DF[-c(diagonals), ]
    }
  }
  return(DF)
}

# Function to find a data frame index ---------------------------
#' @title Find a data frame index
#' @description Finds the data frame index of a column from the name of the column or its index.
#' @param df a data frame in which to find the index of (a) specific column(s).
#' @param label_name a character or numeric vector indicating the column name or index respectively.
#' @return an numeric vector corresponding to the column index that matches argument label_name.
#' @details This function allows the user to select one or several columns according to their name(s) or their index(es).
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal
df.col.findId <- function(df, label_name) {
  # Check if argument label_name is a character or a numeric----------------------
  if (!is.character(label_name) & !is.numeric(label_name)) {
    stop("Argument label_name is not a character or a numeric vector.")
  }

  # If argument label_name is a character, which column number correspond to this character----------------------
  if (is.character(label_name) == TRUE) {
    if (all(!is.na(label_name))) {
      col.id <- match(label_name, colnames(df))
      if(any(is.na(col.id))){
        stop(paste("Error in argument label_name. label_name #", which(is.na(col.id)), " doesn't match with any column name of argument df."))
      }
    }
    else {
      stop("Argument label_name doesn't match any column name.")
    }
  }

  # Else argument label_name is numeric and correspond to column number----------------------
  else {
    if (length(label_name) <= ncol(df)) {
      col.id <- label_name
    }
    else {
      stop("Argument label_name is out of bound.")
    }
  }
  return(col.id)
}


# Function to colorize data frames based on a column data---------------------------
#' @title Colorize according to a column in a data frame
#' @description Add a new column to a data frame of gradient color based on a specific column of the data frame
#' @param df a data frame
#' @param col.att an integer indicating the column with which to create the gradient
#' @param color a character vector of length two indicating the starting point of the gradient and the ending point of the gradiant
#' @param  new.col.name a string indicating the gradient column name added to argument df
#' @return a data frame.
#' @details argument df with an additional column corresponding to the gradient column based on argument att.
#' @keywords internal
colorize <- function(df, col.att, color, new.col.name){
  colfunc <- grDevices::colorRampPalette(color)
  d = df[order(df[,col.att]),]
  d[,col.att] = as.factor(d[,col.att])
  color <- colfunc(length(levels(d[,col.att])))
  d[,ncol(d)+1] = color[d[,col.att]]

  if(is.numeric(df[,col.att])){as.numeric(d[,col.att])}
  if(is.character(df[,col.att])){as.character(d[,col.att])}

  colnames(d)[ncol(d)] = new.col.name
  return(d)
}

# Function to prepare data of node attributes to html---------------------------
#' @title Specify shapes according to a column in a data frame
#' @param vec a  vector
#' @param char a character vector
#' @return a numeric vector.
#' @details Converts a character vector into a numeric vector.
#' @keywords internal
shape <- function(vec, char){
  if(length(unique(vec)) > 7){stop("We don't have more than 7 node shapes to propose. Sorry...")}
  if(length(unique(vec)) != length(char)){stop("Argument char must be of same length of levels/unique of vec")}
  vec2 = as.factor(vec)
  vec2 = char[vec2]
  vec2 = ifelse(vec2 == 'circle',0, vec2)
  vec2 = ifelse(vec2 == 'cross',1, vec2)
  vec2 = ifelse(vec2 == 'losange',2, vec2)
  vec2 = ifelse(vec2 == 'rectangle',3, vec2)
  vec2 = ifelse(vec2 == 'star',4, vec2)
  vec2 = ifelse(vec2 == 'triangle',5, vec2)
  vec2 = ifelse(vec2 == 'y',6, vec2)
  vec2 = as.numeric(vec2)

  if(any(is.na(vec2))){stop("One of the symboles declared in argument char is not supported")}
  return(vec2)
}

# Function to prepare data of node attributes to html---------------------------
#' @title specifies shapes according to a column in a data frame
#' @param df a data frame with node information.
#' @param col.id  a character or numeric vector indicating the column name or index of node Ids.
#' @param col.size a character or numeric vector indicating the column name or index of node size.
#' @param color a vector character of length two indicating the range of color for node color.
#' @param col.color a character or numeric vector indicating the column name or index of node color.
#' @param col.shape a character or numeric vector indicating the column name or index of node shape.
#' @param shapes a character vector indicating the type of shape per category. 7 shapes are available: "circle", "cross", "diamond", "rectangle, "star", "triangle", "y"
#' @param strokeCol a vector character of length two indicating the range of color for node stroke color.
#' @param col.strokeCol a character or numeric vector indicating the column name or index of node stroke color.
#' @param col.stroke a character or numeric vector indicating the column name or index of node stroke.
#' @param node.opacity a character or numeric vector indicating the column name or index of node opacity.
#' @details format a data frame with informations relative to vis.net function.
#' @return a data frame.
#' @keywords internal
vis.net.format.att <- function(df,
                               col.id = NULL, col.size = NULL,
                               color = NULL, col.color = NULL,
                               shapes = NULL, col.shape = NULL,
                               strokeCol = NULL, col.strokeCol = NULL,
                               col.stroke = NULL,
                               node.opacity = NULL){

  if(any(!is.null(c(color,strokeCol,col.shape,node.opacity))) &
     is.null(col.id)){stop("Argument col.id cannot be NUll if any other argument are not.")}
  # Opacity -------------------------------
  if(!is.null(node.opacity)){
    node.opacity = df.col.findId(df, node.opacity)
    if(is.numeric(df[,node.opacity])){
      ori.node.opacity = colnames(df)[node.opacity]
      df$opacity = (df[,node.opacity] - min(df[,node.opacity]))/(max(df[,node.opacity])-min(df[,node.opacity]))
      df$opacity[which.min(df$opacity)] = 0.001
    }else{stop("Column for argument col.size need to be numeric")}
  }else{
    ori.node.opacity = NA
    df$opacity = 1
  }

  # Size -------------------------------
  if(!is.null(col.size)){
    col.size = df.col.findId(df, col.size)
    if(is.numeric(df[,col.size])){
      ori.size = colnames(df)[col.size]
      df$size = df[,col.size]
    }else{stop("Column for argument col.size need to be numeric")}
  }else{
    ori.size = NA
    df$size = 1
  }

  # ID -------------------------------
  if(!is.null(col.id)){
    col.id = df.col.findId(df, col.id)
    ori.id = colnames(df)[col.id]
    df$id = df[, col.id]
  }else{
    ori.id = NA
    df$id = 1:nrow(df)
  }

  # If shape is declared-----------------------------
  if(!is.null(col.shape)){
    col.shape = df.col.findId(df, col.shape)
    ori.shape = colnames(df)[col.shape]
    if(!is.null(shapes)){
      df$shape = shape(df[,col.shape], shapes)
    }else{
      df$shape = as.factor(df[,col.shape])
      df$shape = as.numeric(df$shape)
    }
  }else{
    ori.shape = NA
    df$shape = 0
  }# else everyone is a circle

  # If a value of stroke is declared-----------------------------
  if(!is.null(col.stroke) ){
    col.stroke = df.col.findId(df, col.stroke)
    if(is.numeric( df[,col.stroke])){
      ori.stroke = colnames(df)[col.stroke]
      df$strokeW = df[,col.stroke]
    }else{stop("Column for argument col.stroke need to be numeric")}

    # if a stroke color is declared
    if(!is.null(col.strokeCol)){

      # If a gradiant is declared
      if(length(strokeCol) == 2){

        # If a column is defined for the stroke color
        if(!is.null(col.strokeCol)){
          # Create gradiant
          col.strokeCol = df.col.findId(df, col.strokeCol)
          ori.strokeCol = colnames(df)[col.strokeCol]
          df = colorize(df, col.strokeCol, strokeCol, new.col.name ='strokeCol')

        }else{stop('Argument col.strokeCol cannot be null.')}

      }else{stop('Argument strokeCol need to be of length 2 in order to build a gradiant.')}

    }else{
      ori.strokeCol = NA
      df$strokeCol = 'white'
    }# else everyone is black

  }else{
    ori.strokeCol = NA
    df$strokeCol = NA
    ori.stroke = NA
    df$strokeW = 0
  }# else no stroke

  # if a node color is declared-----------------------------
  if(!is.null(col.color)){
    # If a gradiant is declared
    if(length(color) == 2){

      # If a column is defined for the node color
      if(!is.null(col.color)){

        # Create gradiant
        col.color = df.col.findId(df, col.color)
        ori.color = colnames(df)[col.color]
        df = colorize(df, col.att=col.color,color=color, new.col.name ='color')

      }else{stop('Argument col.color cannot be null.')}

    }else{stop('Argument color need to be of length 2 in order to build a gradiant.')}

  }else{
    ori.color = NA
    df$color = 'black'
  }# else everyone is black
  df$sizeValue = df[,col.size]
  df$colorValue = df[,col.color]
  df$strokeColValue = df[,col.stroke]
  df$strokeWValue = df$strokeW
  df$shapeValue = df[,col.shape]
  " return--------------"
  ori = c(ori.id, ori.size,  ori.color, ori.strokeCol, ori.stroke, ori.shape, ori.node.opacity)
  return(list(df,ori))
}
# Function to visualize the network---------------------------
#' @title Visualize a network
#' @description Plots a network
#' @param df a data frame with node information.
#' @param m a matrix of node connections.
#' @param col.id  a character or numeric vector indicating the column name or index of node Ids.
#' @param col.size a character or numeric vector indicating the column name or index of node size.
#' @param color a vector character of length two indicating the range of color for node color.
#' @param col.color a character or numeric vector indicating the column name or index of node color.
#' @param col.shape a character or numeric vector indicating the column name or index of node shape.
#' @param shapes a character vector indicating the type of shape per category. 7 shapes are available: "circle", "cross", "diamond", "rectangle, "star", "triangle", "y"
#' @param strokeCol a vector character of length two indicating the range of color for node stroke color.
#' @param col.strokeCol a character or numeric vector indicating the column name or index of node stroke color.
#' @param col.stroke a character or numeric vector indicating the column name or index of node stroke.
#' @param layers a column in argument df indicating the factor to use for multi-layer layout.
#' @param node.opacity a character or numeric vector indicating the column name or index of node opacity.
#' @param link.opacity a character or numeric vector indicating the column name or index of link opacity.
#' @param background a character indicating the background color.
#' @return Opens default web browser with the ploted network.
#' @details Opens default web browser and shows 'NetExplorer' interface with the desired network.
#' Further information and tutorial can be found in my video https://youtu.be/IcFTZWCTO_s/
#' @author Sebastian Sosa
#' @examples
#' vis.net(df,m,col.id = "id", col.size = "strength",
#' color = c('green', 'yellow'), col.color = "age" ,
#' strokeCol = c('red', 'blue'), col.strokeCol = "kinship",
#' col.stroke = "degree",
#' col.shape =  "sex",shapes = c("circle", "triangle"),
#' layers =  "kinship")

vis.net <- function(df, m,
                    col.id = NULL, col.size = NULL,
                    color = c("black", "white"), col.color = NULL,
                    col.shape = NULL,shapes = NULL,
                    strokeCol = c("white", "black"), col.strokeCol = NULL,
                    col.stroke = NULL,
                    layers = NULL,
                    node.opacity = NULL,
                    link.opacity = FALSE,
                    background = "grey"){
  # Formating attributes-----------------
  if(!is.null(df)){}
  d = vis.net.format.att(df,
                         col.id = col.id, col.size = col.size,
                         color = color, col.color = col.color,
                         col.shape = col.shape,shapes = shapes,
                         strokeCol = strokeCol, col.strokeCol = col.strokeCol,
                         col.stroke = col.stroke,
                         node.opacity = node.opacity)

  # If no id then use ids declared from matrix-----------------
  if(is.na(d[[2]][[1]])){
    d[[1]]$id = colnames(m)
  }
  if(!is.null(layers)){
    d[[1]]$layers = as.numeric(as.factor(d[[1]][,match(layers, colnames(d[[1]]))]))
  }else{
    d[[1]]$layers = 1
  }

  # edglist convertion----------------------------
  edgl = mat.to.edgl(m)
  edgl = edgl[edgl$weight != 0,]

  if(link.opacity){
    edgl$lOpacity = (edgl$weight - min(edgl$weight))/(max(edgl$weight) - min(edgl$weight))
    edgl$lOpacity[which.min(edgl$lOpacity)] = 0.0001
  }else{edgl$lOpacity = 1}

  # Info storing---------------------------
  df = d[[1]]
  df$grp = match('id', colnames(df))
  col.id = match('id', colnames(df))
  col.size = match('size', colnames(df))
  col.color = match('color', colnames(df))
  col.strokeCol = match('strokeCol', colnames(df))
  col.strokeW = match('strokeW', colnames(df))
  col.shape = match('shape', colnames(df))
  col.shape.cat = match(d[[2]][6], colnames(df))
  col.opacity = match('opacity', colnames(df))

  info.id = match(d[[2]][1], colnames(df))
  info.size = match(d[[2]][2], colnames(df))
  info.color = match(d[[2]][3], colnames(df))
  info.strokeCol = match(d[[2]][4], colnames(df))
  info.strokeW = match(d[[2]][5], colnames(df))
  info.shape = match(d[[2]][6], colnames(df))
  info.opacity  = match(d[[2]][7], colnames(df))

  # Edgelist node source information-------------
  tmp = df[,colnames(df) %in% c("id", "color")]
  colnames(tmp)[1] = "from"
  edgl = merge(edgl, tmp, by = "from", all.x = T)
  colnames(edgl)[5] = "colorL"

  # Edgelist node layer information-------------
  if(!is.null(layers)){
    test1 = unlist(lapply( edgl$from , function(x,d){
      d[d$id %in% x,]$layers
    }, d = d[[1]]))
    test2 = unlist(lapply( edgl$to , function(x,d){
      d[d$id %in% x,]$layers
    }, d = d[[1]]))
    df[df$id %in% edgl$from,]$kinship
    edgl$intralayer = test1 == test2
    edgl$intralayer = as.integer(edgl$intralayer)
    edgl$interlayer = ifelse( edgl$intralayer == 1, NaN, 1)
    edgl$intralayer = ifelse( edgl$intralayer == 1, 1, NaN)

  }else{
    edgl$intralayer = edgl$interlayer = NaN
  }

  # Exporting data to html file -----------------
  # Create a temporary directory
  tempdir <- paste(system.file(package = "NetExplorer"),"/","www", sep = "")
  tmpFile <- file.path(tempdir,  'patron1.txt')
  file.copy(tmpFile, paste0(tempdir,"/NetExplorer.html"), overwrite = TRUE)
  tmpFile <- file.path(tempdir,  'NetExplorer.html')
  cat(paste0('\n','\'',noquote('id : '), '\'',noquote('+ d.id ')),file = tmpFile, append = TRUE)
  if(!is.na(d[[2]][2])){
    cat(paste(noquote('+'),'\'', noquote('<p/>'),d[[2]][2], noquote('(size) : '), '\'', noquote('+ d.sizeValue')),file = tmpFile, append = TRUE)
  }
  if(!is.na(d[[2]][3])){
    cat(paste(noquote('+'), '\'', noquote('<p/>'),d[[2]][3], noquote('(color) : '), '\'', noquote('+ d.colorValue')),file = tmpFile, append = TRUE)
  }

  if(!is.na(d[[2]][4])){
    cat(paste(noquote('+'),'\'', noquote('<p/>'),d[[2]][4], noquote('(stroke color) : '), '\'', noquote('+ d.strokeColValue')),file = tmpFile, append = TRUE)
  }
  if(!is.na(d[[2]][5])){
    cat(paste(noquote('+'),'\'', noquote('<p/>'),d[[2]][5], noquote('(stroke width) : '), '\'', noquote('+ d.strokeWValue')),file = tmpFile, append = TRUE)
  }
  if(!is.na(d[[2]][6])){
    cat(paste(noquote('+'),'\'', noquote('<p/>'),d[[2]][6], noquote('(shape) : '), '\'', noquote('+ d.shapeValue'), "\n"),file = tmpFile, append = TRUE)
  }
  cat(paste0(noquote(").style("), '\'',"left",  '\'', noquote(","),noquote("(d3.event.pageX) +"), '\'',"px",'\'', noquote(")"),"\n",
             noquote(".style("), '\'',"top", '\'', noquote(","),noquote("(d3.event.pageY) +"), '\'',"px",'\'', noquote(")"), noquote(";"),"\n",
             noquote("})"), "\n"), file = tmpFile, append = TRUE)

  lines <- readLines(paste0(tempdir,"/patron2.txt"), warn = FALSE)
  cat(paste0(noquote(lines), "\n"),file = tmpFile, append = TRUE)
  cat(paste0('\n',
             "           function getData() {
   let json = { 'nodes':[", "\n"),file = tmpFile, append = TRUE)



  # Writing header, body and script that doesn't require adatptation from data------------------------------------------


  # Writing attributes data ------------------------------------------
  for (a in 1:nrow(df)) {
    cat(
      paste0(
        noquote('{'),
        '\'','id','\'',
        noquote(':'),
        '\'',df[a,col.id],'\'',
        noquote(','),
        '\'','size','\'',
        noquote(':'),
        df[a,col.size],
        noquote(','),
        '\'','color','\'',
        noquote(':'),
        '\'',df[a,col.color],'\'',
        noquote(','),
        '\'','strokeCol','\'',
        noquote(':'),
        '\'',df[a,col.strokeCol],'\'',
        noquote(','),
        '\'','strokeW','\'',
        noquote(':'),
        df[a,col.strokeW],
        noquote(','),
        '\'','shape','\'',
        noquote(':'),
        df[a,col.shape],
        noquote(','),
        '\'','opacity','\'',
        noquote(':'),
        df[a,col.opacity],
        noquote(','),
        '\'','layers','\'',
        noquote(':'),
        df$layers[a],
        noquote(',')), file = tmpFile, append = TRUE)

    if(!is.na(d[[2]][2])){
      cat(paste0(
        '\'','sizeValue','\'',
        noquote(':'),
        '\'', df[a,info.size], '\'',
        noquote(',')), file = tmpFile, append = TRUE)
    }
    if(!is.na(d[[2]][3])){
      cat(paste0(
        '\'','colorValue','\'',
        noquote(':'),
        '\'', df[a,info.color], '\'',
        noquote(',')), file = tmpFile, append = TRUE)
    }
    if(!is.na(d[[2]][4])){
      cat(paste0(
        '\'','strokeColValue','\'',
        noquote(':'),
        '\'', df[a,info.strokeCol], '\'',
        noquote(',')), file = tmpFile, append = TRUE)
    }
    if(!is.na(d[[2]][5])){
      cat(paste0(
        '\'','strokeWValue','\'',
        noquote(':'),
        '\'', df[a,info.strokeW], '\'',
        noquote(',')), file = tmpFile, append = TRUE)
    }
    if(!is.na(d[[2]][6])){
      cat(paste0(
        '\'','shapeValue','\'',
        noquote(':'),
        '\'', df[a,info.shape], '\'',
        noquote(',')), file = tmpFile, append = TRUE)
    }
    if(!is.na(d[[2]][7])){
      cat(paste0(
        '\'','opacityWValue','\'',
        noquote(':'),
        '\'', df[a,info.opacity], '\''), file = tmpFile, append = TRUE)
    }
    cat(paste0(noquote("},"),"\n"), file = tmpFile, append = TRUE)
  }
  cat(paste0(noquote('],'), '\n',
             "'links'", noquote(':['), '\n' ), file = tmpFile, append = TRUE)

  # Writing links data ------------------------------------------
  for (a in 1:nrow(edgl)) {
    cat(
      paste0(
        noquote('{'), '\n',
        '\'','source','\'', noquote(':'),  '\'', edgl[a, 1],'\',' ,
        '\'','target','\'', noquote(':'),  '\'', edgl[a, 2],'\',',
        '\'','colorL','\'', noquote(':'),  '\'', edgl[a, 5],'\',',
        '\'','lOpacity','\'', noquote(':'),  edgl[a, 4],',',
        '\'','weigth','\'', noquote(':'),  edgl[a, 3],',',
        '\'','intralayer','\'', noquote(':'),  edgl[a, 6],',',
        '\'','interlayer','\'', noquote(':'),  edgl[a, 7],
        noquote('},'), '\n'), file = tmpFile, append = TRUE)
  }
  cat(paste0(noquote(']}'), '\n'), file = tmpFile, append = TRUE)
  cat(paste0(
    noquote('return json;'), '\n',
    noquote('}'), '\n',
    noquote('</script>')
  ), file = tmpFile, append = TRUE)



  # SHow file ---------------------
  file.show(tmpFile)
}



