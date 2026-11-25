coeftest_a_df <- function(ct, nombre_modelo) {
  ct_sin_clase <- unclass(ct)
  m <- matrix(
    as.numeric(ct_sin_clase),
    nrow = nrow(ct_sin_clase),
    ncol = ncol(ct_sin_clase),
    dimnames = list(rownames(ct_sin_clase), colnames(ct_sin_clase))
  )
  out <- as.data.frame(m, stringsAsFactors = FALSE)
  out$termino <- rownames(m)
  out$modelo <- nombre_modelo
  out[, c("modelo", "termino", colnames(m))]
}