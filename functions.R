#Tema general para las gráficas---------------------------------------------
general_theme <- theme(legend.position = "top",
                       legend.direction = "horizontal",
                       legend.box = "horizontal",
                       legend.margin=margin(t = 0, unit='cm'),
                       legend.spacing.x = unit(0, 'cm'),
                       legend.key=element_blank(),
                       legend.text = element_text(size = 9.6, face = "bold"),
                       legend.title = element_text(size = 9.6, face = "bold"),
                       axis.title = element_text(size = 12, face = "bold.italic"),
                       # plot.background = element_rect(fill = "transparent", colour = NA, color = NA),
                       axis.text = element_text(size = 10),
                       panel.background = element_rect(fill = "gray97"),
                       text = element_text(size=12, family="Dosis"),
                       plot.title = element_text(hjust = 0.5))

white_theme <- theme(axis.ticks = element_line(linetype = "blank"), 
                     panel.grid.major = element_line(linetype = "blank"), 
                     panel.grid.minor = element_line(linetype = "blank"), 
                     axis.text.y=element_blank(), axis.text.x=element_blank(),
                     panel.background = element_rect(fill = NA),
                     text = element_text(size=12, family="Dosis"))
#Función para agregar el estilo a las tablas-------------------------------------------------
style_wK <- function(data){
  data %>% kable(escape = FALSE, booktabs = T, align=rep('c')) %>% #Mucho diseño
  kable_styling(bootstrap_options = "striped", full_width = F)
}

#Función para obtener un archivo .tex para alguna gráfica------------------------------------
layoult_latex <- function(graph, file, height = 4, width = 6.35){
  tikz(file = file, width = width, height = height)
  print(graph)
  dev.off()
}
#Función para rellenar curva----------------------------------------------------------------------------------
under_curve <- function(type = "two.sided", alpha ,x, fun, fq,...){
  if(type == "two.sided"){
    left <- fq(p = alpha/2, ...)
    right <- fq(p = 1-alpha/2, ...)
    y <- fun(x, ...)
    y[x > left & x < right] <- NA
    return(y)
  }
  if(type == "greater"){
    right <- fq(p = 1-alpha, ...)
    y <- fun(x, ...)
    y[x < right] <- NA
    return(y)
  }
  if(type == "less"){
    left <- fq(p = alpha, ...)
    y <- fun(x, ...)
    y[x > left] <- NA
    return(y)
  }
}
