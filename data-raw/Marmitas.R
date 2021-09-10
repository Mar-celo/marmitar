## code to prepare `Marmitas` dataset goes here

Marmitas <- marmitar::buscar_marmitas_veganas()

usethis::use_data(Marmitas, overwrite = TRUE)
