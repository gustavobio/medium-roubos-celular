require(lubridate)
require(tidyverse)
require(stringr)
require(ggplot2)
require(ggmap)
require(leaflet)
require(geosphere)
require(dbscan)
require(fields)

arquivos_bos <- list.files("data")
arquivos_bos <- paste("data/", arquivos_bos, sep = "")

binder <- function(filename) {
  x1 <- str_split(readLines(filename, skipNul = T, encoding = "latin1"), "\t")
  x2 <- do.call("rbind.data.frame", x1[-1])
  names(x2) <- x1[[1]]
  x2
}

ocorrencias <- sapply(arquivos_bos, binder, simplify = FALSE)
ocorrencias <- do.call("rbind", ocorrencias)
rownames(ocorrencias) <- NULL

p_ocorrencias <- ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  filter(year(DATAOCORRENCIA) >= 2010, month(DATAOCORRENCIA) %in% 1:9) %>%
  group_by(Ano = year(DATAOCORRENCIA)) %>%
  count() %>%
  ggplot(aes(x = Ano, y = n)) + 
  stat_summary(fun.y = sum, geom="bar", size = 4) + 
  stat_summary(geom="text", aes(label=c(n[-length(n)], paste(n[length(n)], "*", sep = ""))), vjust = -0.5, family = "IBMPlexSans", size = 6) +
  ggtitle(label = "Roubos de celular em Ribeirão Preto", subtitle = "Roubos à transeuntes entre 2010 e 2017") + 
  labs(x = NULL, y = "Número de roubos", caption = "Fonte: SSP-SP. Autor: Gustavo Carvalho. * dados até setembro de 2017") +
  theme_plex() +
  scale_x_discrete(limits = 2010:2017) +
  theme(
    axis.text = element_text(size = 14),
    axis.text.x = element_text(vjust = -1),
    axis.title = element_text(family="IBMPlexSans-Bold"),
    plot.caption = element_text(size = 10)
  ) + 
  ylim(c(0, 1600))

ggsave(p_ocorrencias, file = "~/Desktop/p_ocorrencias.jpg", width = 11, height = 6)
ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  distinct(NUMERO_BOLETIM, .keep_all = TRUE) %>%
  filter(ANO_BO %in% 2010:2017) %>%
  group_by(month = month(DATAOCORRENCIA, label = TRUE), year = year(DATAOCORRENCIA)) %>%
  count() %>%
  arrange(year, month) %>%
  ggplot(aes(x = 1:NROW(.), y = n)) + geom_line() + theme_fivethirtyeight()

prophet_rp <- ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  mutate(dia_semana = week(DATAOCORRENCIA)) %>%
  filter(year(DATAOCORRENCIA) >= 2010) %>%
  group_by(ds = DATAOCORRENCIA) %>%
  summarise(y = n()) %>%
  prophet()
future <- make_future_dataframe(prophet_rp, periods = 365)
forecast <- predict(prophet_rp, future)
plot(prophet_rp, forecast)
ribs <- ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  filter(NUMERO != 0) %>%
  mutate(lat = as.numeric(gsub(",", ".", LATITUDE))) %>%
  mutate(long = as.numeric(gsub(",", ".", LONGITUDE))) %>%
  filter(!is.na(lat)) %>%
  filter(lat != 0) %>%
  filter(round(lat) %in% -22:-19) %>%
  filter(round(long) %in% -49:-46) %>% 
  filter(NUMERO_BOLETIM_PRINCIPAL == "") %>%
  mutate(data = dmy(DATAOCORRENCIA)) %>%
  mutate(ano = year(data)) %>%
  mutate(mes = month(data)) %>%
  mutate(semana = week(data))

ribs <- ribs %>%
  mutate(month = month(DATAOCORRENCIA, label = TRUE)) %>%
  mutate(horario = factor(case_when(
    PERIDOOCORRENCIA %in% c("A NOITE", "DE MADRUGADA") ~ "Noite ou madrugada",
    PERIDOOCORRENCIA %in% c("PELA MANHÃ", "A TARDE") ~ "Dia", 
    TRUE ~ "Em hora incerta"
  ), levels = c("Dia", "Noite ou madrugada", "Em hora incerta")))

ribs <- ocorrencias %>%
  mutate(data = dmy(DATAOCORRENCIA)) %>%
  mutate(ano = year(data)) %>%
  mutate(mes = month(data)) %>%
  mutate(semana = week(data)) %>%
  filter(ano %in% 2010:2017)
qmplot(long, lat, data = filter(ribs, ano == 2017, semana == 10), source = "google", maptype = "road", size = I(1), alpha = I(0.5))
qmplot(long, lat, data = ribs, geom = "blank",maptype = "road",source="google", darken = .3, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .2, color = NA) +
  scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 200)
  pal <- colorFactor(c("#008837", "#7b3294", "grey"), ribs$horario)

  leaflet(ribs) %>% addTiles() %>% 
  addCircleMarkers(~long, ~lat, radius = 3, stroke = FALSE, fillColor = pal(ribs$horario), color = "black", fillOpacity = 0.7) %>%
    addLegend("bottomleft", pal=pal, values=ribs$horario, title="Período",
              layerId="colorLegend", opacity = 1) %>%
    addProviderTiles(providers$CartoDB, options = providerTileOptions(opacity = 1)) 
  
  p_map <-
    get_stamenmap(bbox = c(left = -47.860964, right =  -47.756268, top = -21.097576, bottom = -21.226648),
                  color = "bw", maptype = "toner", zoom = 14
    ) %>%
    ggmap(base_layer = ggplot(aes(x = long, y = lat), data = ribs)) +
    geom_point(aes(colour = horario), alpha = 0.4, size = 2) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle("Localização dos roubos de celular em Ribeirão Preto", subtitle = "Roubos à transeuntes entre 2010 e 2017") +
    labs(caption = "Fonte: SSP-SP. Autor: Gustavo Carvalho (gustavo.bio@gmail.com)") +
    theme_plex() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20, family="IBMPlexSans-Bold"),
      axis.text = element_blank(),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 18),
      plot.caption = element_text(size = 10)
    ) +
    scale_color_manual(
      values = c(
        "Noite ou madrugada" = "darkblue",
        "Dia" = "darkgreen",
        "Em hora incerta" = "red"
      ), name = "Horário do roubo"
    )
  ggsave(p_map,
         file = "~/Desktop/p_map.jpg",
         width = 12,
         height = 12)
  
  p_map2 <-
    get_stamenmap(bbox = c(left = -47.860964, right =  -47.756268, top = -21.097576, bottom = -21.226648),
      color = "bw", maptype = "toner", zoom = 15
    ) %>%
    ggmap(base_layer = ggplot(aes(x = long, y = lat), data = ribs)) +
    geom_point(alpha = 0.4, size = 2) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle("Localização dos roubos de celular em Ribeirão Preto", subtitle = "Roubos à transeuntes entre 2010 e 2017") +
    labs(caption = "Fonte: SSP-SP. Autor: Gustavo Carvalho (gustavo.bio@gmail.com).") +
    stat_density_2d(aes(fill = ..level..), geom = 'polygon', alpha = 0.3) +
    scale_fill_viridis(name = "Densidade de roubos") + 
    theme_plex() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20, family="IBMPlexSans-Bold"),
      axis.text = element_blank(),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 18),
      plot.caption = element_text(size = 10),
      legend.key.width = unit(2, "cm")
    )
  ggsave(p_map2,
         file = "~/Desktop/p_map2.jpg",
         width = 12,
         height = 12)
  
  p_map3 <-
    get_stamenmap(bbox = c(left = -47.860964, right =  -47.756268, top = -21.097576, bottom = -21.226648),
                  color = "bw", maptype = "toner-lite", zoom = 14
    ) %>%
    ggmap(base_layer = ggplot(aes(x = long, y = lat), data = filter(ribs, ano == 2017, month %in% c("Janeiro", "Fevereiro", "Março", "Abril", "Maio")))) +
    geom_point(aes(colour = horario), alpha = 0.4, size = 2) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle("Localização dos roubos de celular em Ribeirão Preto", subtitle = "Roubos à transeuntes de janeiro à maio de 2017") +
    labs(caption = "Fonte: SSP-SP. Autor: Gustavo Carvalho (gustavo.bio@gmail.com)") +
    theme_plex() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16, family="IBMPlexSans-Bold"),
      axis.text = element_blank(),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 8)
    ) +
    scale_color_manual(
      values = c(
        "Noite ou madrugada" = "darkblue",
        "Dia" = "darkgreen",
        "Em hora incerta" = "red"
      ), name = "Horário do roubo"
    ) + 
    facet_wrap(~month, ncol = 5)
  ggsave(p_map3,
         file = "~/Desktop/p_map3.jpg",
         width = 12,
         height = 5)
  
  p_map_week <-
    get_googlemap(
      "Ribeirão Preto",
      zoom = 13,
      maptype = "roadmap",
      language = "pt-BR",
      color = "bw"
    ) %>%
    ggmap(base_layer = ggplot(aes(x = long, y = lat), data = filter(ribs, mes == 4, ano == 2017, semana %in% 14:17))) +
    geom_point(aes(colour = as.factor(semana)), alpha = 0.8, size = 3) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle("Localização dos roubos de celular em Ribeirão Preto", subtitle = "Roubos à transeuntes entre 2010 e 2017") +
    labs(caption = "Fonte: Secretaria de Segurança Pública de São Paulo") +
    
    theme_plex() +
    theme(
      axis.text = element_blank(),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 18),
      legend.position = "none"
    ) 
  ggsave(p_map_week,
         file = "~/Desktop/p_map_week.jpg",
         width = 12,
         height = 12)

ts_test <- ocorrencias %>%
    filter(CIDADE == "RIBEIRAO PRETO") %>%
    filter(grepl("Roubo", RUBRICA)) %>%
    filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
    mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  filter(year(DATAOCORRENCIA) %in% 2010:2017) %>%
  group_by(ano = year(DATAOCORRENCIA), semana = week(DATAOCORRENCIA)) %>%
  count() %>%
  ungroup()

semanas <- ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  filter(year(DATAOCORRENCIA) %in% 2010:2016) %>%
  group_by(semana = week(DATAOCORRENCIA)) %>%
  count() %>%
  ungroup()

semanas %>%
  ggplot(aes(x = semana, y = n)) + geom_line() + geom_smooth(span = 0.2)
ts_test %>%
  ggplot(aes(x = ano, y = n)) + geom_path()

ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  group_by(mes = month(DATAOCORRENCIA), ano = year(DATAOCORRENCIA)) %>%
  summarise(dias = length(unique(DATAOCORRENCIA)))

first_days <- 

dias <- seq(ymd("2010/01/01"), ymd("2017/09/30"), by = "1 day")
dias <- data.frame(dia = dias, semana = week(dias), ano = as.numeric(format(dias, "%Y")))
dias <- dias[!duplicated(select(dias, semana, ano)), ]
dias$trend <- stl(ts_roubos, 31)$trend

ts_test <- ts_test %>% left_join(dias)
ts_test$trend <- stl(ts_roubos, "periodic")$time.series[, "trend"]
p_ts <- ts_test %>%
  ggplot(aes(x = dia, y = n)) +
  geom_line(alpha = 0.5) +
  geom_line(aes(x = dia, y = trend), col = "red", size = 1) +
  scale_x_date(breaks = seq(as.Date("2010/01/01"), as.Date("2017/12/31"), by = "1 year"),
               labels = 2010:2017) + theme_plex() +
  theme(
    panel.grid.major.x = element_line(),
    axis.title = element_text(family = "IBMPlexSans-Bold"),
    axis.text = element_text(size = 14),
    plot.caption = element_text(size = 10)
  ) +
  xlab(NULL) +
  ylab("Número de roubos por semana") + 
  labs(caption = "Fonte: SSP-SP. Autor: Gustavo Carvalho (gustavo.bio@gmail.com).") + 
  ggtitle("Variação semanal no número de roubos em Ribeirão Preto")
ggsave(p_ts, file = "~/Desktop/p_ts.jpg", width = 15, height = 5)


dias_1 <- unique(dmy(ribs$DATAOCORRENCIA))
p_semana <- ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  filter(year(DATAOCORRENCIA) %in% 2010:2017) %>%
  group_by(wday = wday(DATAOCORRENCIA, label = TRUE)) %>%
  count() %>%
  ggplot(aes(x = wday, y = n)) + 
  stat_summary(fun.y = sum, geom="bar", size = 4) + 
  stat_summary(geom="text", aes(label=n), vjust = -0.5, family = "IBMPlexSans") +
  # ggtitle(label = "Roubos de celular em Ribeirão Preto", subtitle = "Roubos à transeuntes entre 2010 e 2017") + 
  labs(x = "Dia da semana", y = "Número de roubos") +
  theme_plex() +
  theme(
    axis.text.x = element_text(vjust = -1),
    axis.title = element_text(family="IBMPlexSans-Bold"),
    plot.caption = element_text(size = 9, colour = "darkgrey")
  ) + ylim(c(0, 1550)) + 
  scale_x_discrete(labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab"))
ggsave(p_semana, file = "~/Desktop/p_semana.jpg", width = 10, height = 7)

plot_yearly <- function (m,
                         uncertainty = TRUE,
                         yearly_start = 0)
{
  days <-
    seq(prophet:::set_date("2017-01-01"),
        by = "d",
        length.out = 365) +
    as.difftime(yearly_start, units = "days")
  df.y <- prophet:::seasonality_plot_df(m, days)
  seas <- prophet:::predict_seasonal_components(m, df.y)
  seas$ds <- df.y$ds
  gg.yearly <-
    ggplot2::ggplot(seas, ggplot2::aes(x = as.Date(ds), y = yearly,
                                       group = 1)) + ggplot2::geom_line(color = "red", na.rm = TRUE, size = 1) +
    ggplot2::labs(x = "Dia do ano") + ggplot2::scale_x_date(breaks = dmy("01/01/2017") + months(0:11), labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
    theme_plex() + 
    theme(
      panel.grid.major.x = element_line(),
      panel.grid.minor.x = element_line(linetype = "dotted", colour = "lightgrey"), 
      axis.text.x = element_text(angle = 0),
      axis.title.y = element_text(family="IBMPlexSans-Bold"),
      axis.text = element_text(size = 14),
      plot.caption = element_text(size = 10)
    ) + 
    xlab(NULL) + 
    ylab("Variação do número de roubos") + 
    ggtitle("Variação anual do número de roubos", subtitle = "Dados combinados entre 2010 e 2017")
    labs(caption = "Fonte: SSP-SP. Autor: Gustavo Carvalho (gustavo.bio@gmail.com).") 
  if (uncertainty) {
    gg.yearly <-
      gg.yearly + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = yearly_lower,
                     ymax = yearly_upper),
        alpha = 0.2,
        fill = "#0072B2",
        na.rm = TRUE
      )
  }
  return(gg.yearly)
}
p_yearly <- plot_yearly(prophet_rp)
ggsave(p_yearly, file = "~/Desktop/p_yearly.jpg", width = 15, height = 5)

p_mes <- ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(DATAOCORRENCIA = dmy(DATAOCORRENCIA)) %>%
  filter(year(DATAOCORRENCIA) %in% 2010:2017) %>%
  group_by(month = month(DATAOCORRENCIA, label = TRUE)) %>%
  count() %>%
  ggplot(aes(x = month, y = n)) + 
  stat_summary(fun.y = sum, geom="bar", size = 4) + 
  stat_summary(geom="text", aes(label=n), vjust = -0.5, family = "IBMPlexSans") +
  # ggtitle(label = "Roubos de celular em Ribeirão Preto", subtitle = "Roubos à transeuntes entre 2010 e 2017") + 
  labs(x = "Mês", y = "Número de roubos") +
  theme_plex() +
  theme(
    axis.text.x = element_text(vjust = -1),
    axis.title = element_text(family="IBMPlexSans-Bold"),
    plot.caption = element_text(size = 9, colour = "darkgrey")
  ) + scale_x_discrete(labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
ggsave(p_mes, file = "~/Desktop/p_mes.jpg", width = 10, height = 7)

ggplot(ts_test, aes(x = rolling_week, y = rollmean(n, 52))) + 
  geom_line(col = "red")

  
ts_roubos <- ts(ts_test$n, frequency = 365.25/7, start = 2010)
decompose_roubos <- decompose(ts_roubos, "additive")
adjust_roubos <-  ts_roubos - decompose_roubos$seasonal
plot(decompose_roubos$trend)
plot(adjust_roubos)

theme_plex <- function(base_size = 14,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 18,
                       plot_title_margin = 10,
                       ...) {
  ret <- ggplot2::theme_minimal(base_family = "IBMPlexSans",
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                          margin=margin(b=strip_text_margin),
                                          family="IBMPlexSans-Medium")
  ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                             margin=margin(b=subtitle_margin),
                                             family="IBMPlexSans")
  ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                          margin=margin(b=plot_title_margin),
                                          family="IBMPlexSans-Bold")
  ret$panel.grid.minor = ggplot2::element_blank()
  ret$panel.grid.major.x = ggplot2::element_blank()
  ret$plot.margin = ggplot2::unit(c(1,1, 1, 1), "lines")
  ret
}

ribs_week <- ocorrencias %>%
  filter(CIDADE == "RIBEIRAO PRETO") %>%
  filter(grepl("Roubo", RUBRICA)) %>%
  filter(grepl("Transeunte", RUBRICA, ignore.case = TRUE)) %>%
  mutate(date = dmy(DATAOCORRENCIA)) %>%
  filter(year(date) %in% 2010:2017) %>%
  filter(NUMERO_BOLETIM_PRINCIPAL == "") %>%
  group_by(date) %>%
  count() %>% ungroup()

test2 <- filter(station_data_summary, station_id == 83669, year(date) >= 2010) %>%
  filter(prec > 5) %>%
  right_join(ribs_week)

ggplot(test2, aes(x = prec, y = n)) + geom_point() + geom_smooth(method = "lm") + theme_plex()

test3 <- ribs %>%
  filter(ano == 2016) %>%
  mutate(x = long, y = lat) %>%
  filter(horario == "Noite ou madrugada")



threshold.in.km <- 0.5
coors <- test3[,c("long", "lat")]

#distance matrix
dist.in.km.matrix <- rdist.earth(coors,miles = F,R=6371)

#clustering
fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
clusters <- cutree(fit,h = threshold.in.km)

plot(test3$long, test3$lat, col = clusters, pch = 20)

ggplot(ribs, aes(long, lat)) +
  stat_density_2d(aes(fill = ..level..), geom = 'polygon', alpha = 0.3) +
  scale_fill_viridis(name = "density") +
  geom_point(shape = '.')

  