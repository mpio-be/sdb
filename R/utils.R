
#' @export
# source: https://gist.github.com/jslefche/eff85ef06b4705e6efbc

theme_black <- function(base_size = 12, base_family = "") {

      theme_grey(base_size = base_size, base_family = base_family) %+replace%

      theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9, vjust = 1),
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9, hjust = 1),
      axis.ticks = element_line(color = "white", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "white", vjust = 1),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, vjust = 0.5),

      # Specify legend options
      legend.background = element_rect(color = NA, fill = "#222d32"),
      legend.key = element_rect(color = "white",  fill = "#222d32"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = "#222d32", color  =  NA),
      panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      panel.margin = unit(0.25, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size*0.8, color = "white"),
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),
      # Specify plot options
      plot.background = element_rect(color = "#222d32", fill = "#222d32"),
      plot.title = element_text(size = base_size*1.2, color = "white"),
      plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")

      )

      }


#' string_is_mysql_date
#' @export
string_is_mysql_date <- function(x) {
    o = str_detect(x, pattern = '(\\d{2}|\\d{4})(?:\\-)?([0]{1}\\d{1}|[1]{1}[0-2]{1})(?:\\-)?([0-2]{1}\\d{1}|[3]{1}[0-1]{1})(?:\\s)?([0-1]{1}\\d{1}|[2]{1}[0-3]{1})(?::)?([0-5]{1}\\d{1})(?::)?([0-5]{1}\\d{1})')
    if ( all(is.na(o))  ) o = FALSE else o =  all(o, na.rm= TRUE)
    o  
    }



#' @export    
I_am_in_Seewiesen <- function() {

      str_trim(system('hostname', intern = TRUE)) == "pc3168"

      }