
x <- d |>
  filter(plot == "LJQ1") |>
  filter(vi == "nbr") |>
  dplyr::select(
    year, nbr = original)


plot(x$year,x$nbr)

y0 <- 2006

get_rri <- function(x, y0){

  nbr_max_y45 <- x |>
    filter(year %in% c(y0+4, y0+5)) |>
    summarise(nbry45 = max(nbr))

  nbr_y0 <- x |> filter(year == y0) |>
    dplyr::select(nbry0 = nbr)

  nbr_pre <- x |>
    filter(year %in% c(y0-1, y0-2)) |>
    summarise(nbrpre = mean(nbr))

  out <- bind_cols(nbr_y0,
                   nbr_max_y45,
                   nbr_pre)
  out$delta_nbr =
    mutate(delta_nbr = (nbr_pre - nbr_y0),
           rri = (nbr_max_y45 - nbr_y0)/delta_nbr)
  return(out)

}



