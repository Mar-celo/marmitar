#' Buscar marmitas veganas do cardapio da Beleaf
#'
#' Faz a raspagem dos dados do site da marca Beleaf,
#' e busca uma tabela contendo informacoes sobre o
#' cardapio das marmitas veganas.
#'
#' @return Uma tibble com 5 colunas e 30 linhas
#' @export
#'
#' @examples
#' buscar_marmitas_veganas()

buscar_marmitas_veganas <- function() {
  html <- "https://www.beleaf.com.br/cardapio-marmitas-veganas" %>%
    xml2::read_html()

  url <- html %>%
    xml2::xml_find_all("//a[contains(@class, 'action more')]") %>%
    purrr::map(~ xml2::xml_attr(.x, "href")) %>%
    purrr::map_dfr(~ tibble::enframe(purrr::set_names(.x, "url")), .id = "item") %>%
    dplyr::select("url" = value)

  nm <- c("nome", "descricao", "preco")
  infos <-
    html %>% xml2::xml_find_all("//div[contains(@class, 'product-item-details')]") %>%
    purrr::map(~ stringr::str_squish(xml2::xml_text(xml2::xml_children(.x)))) %>%
    purrr::map_dfr(~ tibble::enframe(purrr::set_names(.x, nm)), .id = "item") %>%
    tidyr::pivot_wider() %>%
    dplyr::mutate(
      preco = readr::parse_number(preco, locale = readr::locale(decimal_mark = ",")),
      tipo_refeicao = dplyr::case_when(
        stringr::str_detect(nome, "BOLO|MOUSSE|BROWNIE|CHEESECAKE|TORTA") ~ "Doce",
        stringr::str_detect(nome, "CREME DE|MINESTRONE") ~ "Sopa",
        TRUE ~ "Prato"
      )
    )


  dplyr::bind_cols(infos, url)
}



#' Função para sortear uma marmita, usando a base de marmitas
#'
#' @param tipo
#' @param preco_maximo
#'
#' @return Uma tibble com uma marmita para comprar!
#' @export
#'
#' @examples
#' sortear_marmita()
#'
#'
#' sortear_marmita(preco_maximo = 22)
#'
#'
#' sortear_marmita(tipo = "Sopa")
#'
#'
#' sortear_marmita(tipo = "Prato")
#'
#'
#' sortear_marmita(tipo = "Doce")
#'
sortear_marmita <-
  function(tipo = c("Sopa", "Prato", "Doce"), preco_maximo = 50) {
    Marmitas %>%
      dplyr::filter(preco <= preco_maximo, tipo_refeicao %in% tipo) %>%
      dplyr::slice_sample() %>%
      dplyr::mutate(
        texto = glue::glue(
          "Refei\u00e7\u00e3o  sugerida: {nome} \n
      Ingredientes: {descricao} \n
      Pre\u00e7o: R$ {preco} \n
      Acesse o site: {url}"
        )
      ) %>%
      dplyr::pull(texto)
  }

