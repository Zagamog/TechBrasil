# clean_prettify.R

# a function to clean prettified numbers




parse_en_number <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", x)))
}

# Example usage of parse_en_number
parse_en_number(propag_ept_financeiro$saldo_mar25)*0.20


