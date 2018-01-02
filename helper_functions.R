require(rvest)
require(stringr)
require(tidyr)
require(purrr)

get_ids <- function() {
  dummy_url <- read_html("https://cidades.ibge.gov.br/xtras/perfil.php?lang=&codmun=3550308")
  cities_urls <- dummy_url %>%
    html_nodes("#lista_municipios a")
  cities_names <- cities_urls %>%
    html_text()
  cities_ids <- cities_urls %>%
    html_attr("href") %>%
    str_extract("(?<=codmun=)\\d+(?=&)")
  data.frame(cidade = cities_names, id = cities_ids, stringsAsFactors = FALSE)
}

rename_education_cols <- function(name) {
  name <- tolower(name)
  name <- gsub("é", "e", name)
  name <- gsub("ú", "u", name)
  name <- gsub("í", "i", name)
  paste(str_extract_all(name, "^(\\w{3})|(?<=_)\\w.{2}")[[1]], collapse = "_")
}

build_url <- function(tema, id) {
  paste0("https://cidades.ibge.gov.br/xtras/csv.php?lang=&idtema=", tema, "&codmun=", id)
}

get_education <- function(id = 354340) {
  ed_data <- read.csv2(build_url(156, id), header = FALSE, fileEncoding = "iso-8859-1", encoding = "iso-8859-1", skip = 3)
  ed_data <- head(ed_data, -2) %>%
    filter(!grepl("existente", V3))
  ed_data <- ed_data[, 1:3]
  colnames(ed_data) <- c("variable", "amount", "unit")
  ed_data <- ed_data %>%
    mutate(variable = gsub("\\s-\\s2015.*", "", variable)) %>%
    mutate(variable = gsub(" ", "_", variable)) %>%
    mutate(amount = as.numeric(gsub("\\.", "", amount))) %>%
    distinct() %>%
    select(-unit)
  ed_data$variable <- map_chr(ed_data$variable, rename_education_cols)
  spread(ed_data, variable, amount) %>%
    mutate(id = id) %>%
    select(id, everything())
}

get_pib_per_capita <- function(id = 354340) {
  pib_data <- read.csv2(build_url(162, id), header = FALSE, fileEncoding = "iso-8859-1", encoding = "iso-8859-1", skip = 3)
  pib <- pib_data %>%
    filter(grepl("capita", V1)) %>%
    select(V2)
  pib <- gsub("\\.", "", pib[1, 1])
  pib <- as.numeric(gsub(",", ".", pib))
  data.frame(id, pib)
}

get_population <- function(id = 354340) {
  pop_data <- read.csv2(build_url(1, id), header = FALSE, fileEncoding = "iso-8859-1", encoding = "iso-8859-1", skip = 3)
  pop_data <- head(pop_data[1:2], -1)
  names(pop_data) <- c("variable", "amount")
  pop_data %>%
    mutate(amount = gsub("\\.", "", amount)) %>%
    mutate(amount = as.numeric(gsub(",", ".", amount))) %>%
    mutate(variable = tolower(gsub(" ", "_", variable))) %>%
    mutate(id = id) %>%
    select(id, everything()) %>%
    spread(variable, amount)
}

get_jobs <- function(id = 354340) {
  job_data <- read.csv2(build_url(165, id), header = FALSE, fileEncoding = "iso-8859-1", encoding = "iso-8859-1", skip = 3)
  job_data <- head(job_data[1:2], -1)
  names(job_data) <- c("variable", "amount")
  job_data %>%
    mutate(amount = gsub("\\.", "", amount)) %>%
    mutate(amount = as.numeric(gsub(",", ".", amount))) %>%
    mutate(variable = tolower(gsub(" ", "_", variable))) %>%
    mutate(id = id) %>%
    select(id, everything()) %>%
    spread(variable, amount)
}

get_presidential_voting <- function(id = 354340) {
  votes_data <- read.csv2(build_url(140, id), header = FALSE, fileEncoding = "iso-8859-1", encoding = "iso-8859-1", skip = 3)
  votes_data <- head(votes_data, -1)
  aecio <- as.numeric(gsub(",", ".", votes_data[4, 2]))
  dilma <- 100 - aecio
  data.frame(id = id, dilma, aecio)
}

get_census_2010 <- function(id = 354340) {
  census_data <- read.csv2(build_url(105, id), header = FALSE, fileEncoding = "iso-8859-1", encoding = "iso-8859-1", skip = 3)
  census_data <- head(census_data, -1)
  census_data <- census_data[, 1:2]
  names(census_data) <- c("variable", "amount")
  census_data %>%
    mutate(variable = gsub(" ", "_", tolower(variable))) %>%
    mutate(amount = gsub("\\.", "", amount)) %>%
    mutate(amount = gsub("-", "0", amount)) %>%
    mutate(amount = as.numeric(gsub(",", ".", amount))) %>%
    spread(variable, amount) %>%
    mutate(id = id) %>%
    select(id, everything())
}

match_city <- function(x, y) {
  z <- gsub("S\\.(?=[AEIOU])", "SANTO ", x, perl = T)
  z <- gsub("S\\.(?=[^AEIOU])", "SAO ", z, perl = T)
  dist_names <- adist(z, y, ignore.case = T)
  name_match <- apply(dist_names, 1, which.min)
  res <- data.frame(x = x, y = y[name_match], z = z)
  res[res$x != "", ]
}

get_ssp_mun <- function() {
  ssp_url <-
    "http://www.ssp.sp.gov.br/Estatistica/Pesquisa.aspx"
  page <- read_html(ssp_url) %>%
    html_nodes("#conteudo_ddlMunicipios , #conteudo_lkMunicipio") %>%
    html_children() 
  ssp_id <- page %>%
    html_attr("value")
  city <- page %>%
    html_text
  data.frame(ssp_id, cidade = city)
}

get_crime_data <- function(municipio = 3, year = 2015, verbose = TRUE) {
  if (verbose) cat(municipio, sep = "\n")
  set_year <-
    post_form(target = "ctl00$conteudo$ddlAnos", year = year)
  set_mun <-
    post_form(target = "ctl00$conteudo$ddlMunicipios",
              session = set_year,
              municipio = municipio)
  set_target <-
    post_form(target = "ctl00$conteudo$btnMensal", session = set_mun)
  res <-
    set_target %>% html_node("#conteudo_repAnos_gridDados_0") %>% html_table()
  attr(res, "cidade") <-
    set_target %>% 
    html_node("#conteudo_lkMunicipio") %>% 
    html_text() %>%
    str_replace("\\s\\|\\s", "")
  res
}

remove_accents <- function(x) {
  x <- str_replace_all(x, "[áãâà]", "a")
  x <- str_replace_all(x, "[éê]", "e")
  x <- str_replace_all(x, "[í]", "i")
  x <- str_replace_all(x, "[óô]", "o")
  x <- str_replace_all(x, "ç", "c")
  x <- str_replace_all(x, ",", "")
  str_replace_all(x, "[úü]", "u")
}
get_vehicles <- function(id = 354340) {
  vehicles_data <- read.csv2(build_url(153, id), header = FALSE, fileEncoding = "iso-8859-1", encoding = "iso-8859-1", skip = 3)
  vehicles_data <- head(vehicles_data, -3) %>%
    filter(!grepl("existente", V3))
  vehicles_data <- vehicles_data[, 1:3]
  colnames(vehicles_data) <- c("variable", "amount", "unit")
  vehicles_data <- vehicles_data %>%
    mutate(variable = gsub("\\s-\\s.*", "", variable)) %>%
    mutate(variable = gsub(" ", "_", tolower(variable))) %>%
    mutate(amount = as.numeric(gsub("\\.", "", amount))) %>%
    select(-unit) 
  spread(vehicles_data, variable, amount) %>%
    mutate(id = id) %>%
    rename_all(remove_accents) %>%
    select(id, everything())
}

post_form <-
  function(target = NULL,
           response = FALSE,
           session = NULL,
           municipio = NULL,
           year = NULL,
           delegacia = 0,
           ...) {
    ssp_url <-
      "http://www.ssp.sp.gov.br/Estatistica/Pesquisa.aspx"
    if (is.null(session)) {
      page_session <- html_session(ssp_url)
    } else {
      page_session <- session
    }
    page_form <- html_form(page_session)[[1]]
    page <- rvest:::request_POST(
      page_session,
      ssp_url,
      body = list(
        `__VIEWSTATE` = page_form$fields$`__VIEWSTATE`$value,
        `__EVENTTARGET` = target,
        `__EVENTARGUMENT` = "",
        `__LASTFOCUS` = "",
        `__EVENTVALIDATION` = page_form$fields$`__EVENTVALIDATION`$value,
        `ctl00$conteudo$ddlAnos` = year,
        `ctl00$conteudo$ddlMunicipios` = municipio,
        `ctl00$conteudo$ddlDelegacias` = 0,
        `ctl00$conteudo$ddlRegioes` = page_form$fields$`ctl00$conteudo$ddlRegioes`$value
      ),
      encode = "form"
    )
    page
  }