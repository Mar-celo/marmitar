test_that("Buscar marmitas", {

  # Verificar se a base foi baixada
  # Uma forma é verificar se o resultado é um data frame com número de linhas
  # maior que zero
  expect_true(is.data.frame(Marmitas) )
  expect_gte(nrow(Marmitas), 0)
})
