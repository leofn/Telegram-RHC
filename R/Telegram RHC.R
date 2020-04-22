library(magrittr)

ler_html_telegram <- function(html_file) {
  # pega todas as mensagens
  divs <- xml2::read_html(html_file) %>% 
    xml2::xml_find_all("//div[@class='message default clearfix']")
  
  # nome da pessoa
  nomes <- divs %>% 
    xml2::xml_find_all("./div/div[@class='from_name']") %>% 
    xml2::xml_text() %>% 
    stringr::str_squish()
  
  # data e hora da mensagem
  data_horas <- divs %>% 
    xml2::xml_find_all("./div/div[@class='pull_right date details']") %>% 
    xml2::xml_attr("title") %>% 
    lubridate::dmy_hms()
  
  # texto da mensagem
  textos <- divs %>% 
    purrr::map(xml2::xml_find_first, "./div/div[@class='text']") %>% 
    purrr::map_chr(xml2::xml_text) %>% 
    stringr::str_squish()
  
  # retorna numa tabela
  tibble::tibble(
    data_hora = data_horas,
    nome = nomes,
    texto = textos
  )
}

## Lendo as Mensagens
path <- getwd()
todos_arquivos <- fs::dir_ls(path, regexp = "messages")

d_msg <- purrr::map_dfr(
  todos_arquivos, 
  ler_html_telegram, 
  .id = "arquivo"
)

d_msg %>% 
  dplyr::count(nome, sort = TRUE) %>% 
  dplyr::mutate(prop = scales::percent(n/sum(n))) %>% 
  head(20) %>% 
  knitr::kable()

d_msg_porc <- d_msg %>% 
  dplyr::count(nome, sort = TRUE) %>% 
  dplyr::mutate(prop = scales::percent(n/sum(n))) %>% 
  head(20) 


### total de mensagens por user
ggplot(d_msg_porc, aes(x=reorder(nome, -n), y=n)) + 
  geom_bar(stat = "identity", color='skyblue',fill='steelblue')+
  xlab("Nome") +
  ylab("Total de Mensagens")+
  ggtitle("Total de mensagens por usuário no Telegram do Raul Hacker Club (não oficial)")+
  coord_flip()

## Total de mensagens ao longo do tempo
library(ggplot2)
d_msg %>% 
  dplyr::mutate(mes = lubridate::floor_date(data_hora, "month")) %>% 
  dplyr::count(mes) %>% 
  ggplot(aes(x = mes, y = n)) +
  geom_line() +
  geom_point() +
  xlab("Anos") +
  ylab("Número de Mensagens")+
  ggtitle("Total de mensagens no Telegram do Raul Hacker Club (não oficial)")+
  theme_gray(16)

### total de mensagens por user ao longo do tempo
d_msg %>% 
  dplyr::filter(nome != "Deleted Account") %>% 
  dplyr::mutate(nome = forcats::fct_lump(nome, 12),
                nome = as.character(nome),
                mes = lubridate::floor_date(data_hora, "month")) %>% 
  dplyr::filter(nome != "Other") %>% 
  dplyr::count(mes, nome, sort = TRUE) %>% 
  tidyr::complete(mes, nome, fill = list(n = 0)) %>% 
  ggplot(aes(x = mes, y = n)) +
  geom_line() +
  facet_wrap(~nome) +
  labs(x = "Mês", y = "Quantidade de mensagens") +
  ggtitle("Frequência de mensagens por usário no Telegram do Raul Hacker Club (não oficial)")+
  theme_bw()

## hora que mais interagimos

d_msg %>% 
  dplyr::mutate(hora = factor(lubridate::hour(data_hora))) %>% 
  ggplot(aes(x = hora)) +
  geom_bar(fill = "royalblue") +
  theme_minimal(14) +
  ylab("Número de Mensagens")+
  ggtitle("Hora das mensagens no Telegram do Raul Hacker Club (não oficial)")


## o dia da semana? quinta-feira wins!
  
  d_msg %>% 
  dplyr::mutate(wd = lubridate::wday(data_hora, label = TRUE)) %>% 
  ggplot(aes(x = wd)) +
  geom_bar(fill = "blue") +
  theme_minimal(14) +
  ylab("Dia da semana")+
  ggtitle("Dia da semana das mensagens")


  # dá pra criar funções anônimas assim ;)
  # esse é um limpador bem safado que fiz em 1 min
  limpar <- . %>% 
    abjutils::rm_accent() %>% 
    stringr::str_to_title() %>% 
    stringr::str_remove_all("[^a-zA-Z0-9 ]") %>% 
    stringr::str_remove_all("Pra") %>% 
    stringr::str_squish()
library('abjutils')
  # tirar palavras que nao quero
  banned <- tidytext::get_stopwords("pt") %>% 
    dplyr::mutate(palavra = limpar(word))
  
  cores <- viridis::viridis(10, begin = 0, end = 0.8)
  
d_msg %>% 
    tidytext::unnest_tokens(palavra, texto) %>% 
    dplyr::mutate(palavra = limpar(palavra)) %>% 
    dplyr::anti_join(banned, "palavra") %>% 
    dplyr::count(palavra, sort = TRUE) %>% 
    with(wordcloud::wordcloud(
      palavra, n, scale = c(5, .1), 
      min.freq = 80, random.order = FALSE,
      colors = cores
    ))  


  
  
  
  
  
