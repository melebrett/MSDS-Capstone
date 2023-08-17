table_options <- list(
  dom = 'lBfrtip',
  pageLength = 25,
  buttons = list(
    list(
      extend = "collection",
      text = 'Show All',
      action = DT::JS(
        "function ( e, dt, node, config ) {
          dt.page.len(-1);
          dt.ajax.reload();}"
      )
    ),
    list(
      extend = "collection",
      text = 'Show Less',
      action = DT::JS(
        "function ( e, dt, node, config ) {
          dt.page.len(25);
          dt.ajax.reload();}"
      )
    )
  ),
  deferRender = TRUE,
  autowidth = TRUE,
  lengthMenu = list(c(25,50), c('25', '50')),
  searching = TRUE,
  editable = FALSE,
  scroller = TRUE,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
    "}"
  )
)

color_gradient <- function(low, hi, steps){ 
  colorinterval <-seq(low, hi, steps)
  gradient <- ztable::gradientColor(low = "#3B4CC0FF", mid = "#DDDDDDFF", high = "#B40426FF", n = colorinterval %>% length(), plot = FALSE)
  return(list(colorinterval, gradient))
}

