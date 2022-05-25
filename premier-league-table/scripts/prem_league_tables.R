library(tidyverse)

data.url <- "https://www.football-data.co.uk/mmz4281/2122/E0.csv"
# data.file <- "data/matchData.csv"
# data.date <- Sys.Date()
# download.file(data.url, destfile = data.file, quiet = TRUE)

data.matches <- read.csv(data.url, header = TRUE)
names(data.matches)

data.edit <- data.matches %>% select(HomeTeam, AwayTeam, FTHG, FTAG, Date)
head(data.edit)
data.edit$Date <- as.Date(data.edit$Date, "%d/%m/%Y")

calculate.win <- function(ourScore, theirScore){
  return(ourScore > theirScore)
}

calculate.lose <- function(ourScore, theirScore){
  return(ourScore < theirScore)
}

calculate.draw <- function(ourScore, theirScore){
  return(ourScore == theirScore)
}

calculate.points <- function(ourScore, theirScore){
  return(ifelse(ourScore < theirScore, 0, ifelse(ourScore == theirScore, 1, 3)))
}

# Manually change gt_plt_winloss function ---------------------------------

gt_plt_winloss_jh <- function (gt_object, column, max_wins = 17, colors = c("#D50A0A", 
                                                                            "#013369", "gray"), type = "pill") 
{
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  stopifnot(`type must be on of 'pill' or 'square'` = {
    type %in% c("pill", "square")
  })
  stopifnot(`There must be 3 colors` = length(colors) == 3L)
  test_vals <- gt_object[["_data"]] %>% dplyr::select({
    {
      column
    }
  }) %>% dplyr::pull()
  stopifnot(`The column must be a list-column` = is.list(test_vals))
  stopifnot(`All values must be 1, 0 or 0.5` = unlist(test_vals) %in% 
              c(1, 0, 0.5))
  plot_fn_pill <- function(x) {
    vals <- strsplit(x, split = ", ") %>% unlist() %>% as.double()
    input_data <- data.frame(x = 1:length(vals), xend = 1:length(vals), 
                             y = ifelse(vals == 0.5, 0.4, vals), yend = ifelse(vals == 
                                                                                 0, 0.6, ifelse(vals > 0.5, 0.4, 0.6)), color = ifelse(vals == 
                                                                                                                                         0, "#D50A0A", ifelse(vals == 1, "#013369", "grey")))
    plot_out <- ggplot(input_data) + geom_segment(aes(x = x, 
                                                      xend = xend, y = y, yend = yend, color = I(color)), 
                                                  size = 1, lineend = "round") + scale_x_continuous(limits = c(0.5, 
                                                                                                               max_wins + 0.5)) + scale_y_continuous(limits = c(-0.2, 
                                                                                                                                                                1.2)) + theme_void()
    out_name <- file.path(tempfile(pattern = "file", tmpdir = tempdir(), 
                                   fileext = ".svg"))
    ggsave(out_name, plot = plot_out, dpi = 20, height = 0.15, 
           width = 0.3)
    img_plot <- out_name %>% readLines() %>% paste0(collapse = "") %>% 
      gt::html()
    on.exit(file.remove(out_name))
    img_plot
  }
  plot_fn_square <- function(x) {
    vals <- strsplit(x, split = ", ") %>% unlist() %>% as.double()
    input_data <- data.frame(x = 1:length(vals), xend = 1:length(vals), 
                             y = ifelse(vals == 0.5, 0.4, vals), yend = ifelse(vals == 
                                                                                 0, 0.6, ifelse(vals > 0.5, 0.4, 0.6)), color = ifelse(vals == 
                                                                                                                                         0, "#D50A0A", ifelse(vals == 1, "#013369", "grey")))
    plot_out <- ggplot(input_data) + geom_point(aes(x = x, 
                                                    y = y, color = I(color)), size = 1.5, shape = 15) + 
      scale_x_continuous(limits = c(0.5, max_wins + 0.5)) + 
      scale_y_continuous(limits = c(-0.2, 1.2)) + theme_void()
    out_name <- file.path(tempfile(pattern = "file", tmpdir = tempdir(), 
                                   fileext = ".svg"))
    ggsave(out_name, plot = plot_out, dpi = 20, height = 0.15, 
           width = 0.5)
    img_plot <- out_name %>% readLines() %>% paste0(collapse = "") %>% 
      gt::html()
    on.exit(file.remove(out_name))
    img_plot
  }
  text_transform(gt_object, locations = cells_body(columns = {
    {
      column
    }
  }), fn = function(x) {
    lapply(x, if (type == "pill") {
      plot_fn_pill
    }
    else {
      plot_fn_square
    })
  })
}

