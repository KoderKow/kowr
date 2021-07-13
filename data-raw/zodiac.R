## code to prepare `zodiac` dataset goes here
zodiac_signs_roman_order <- c(
    "Aries", "Taurus", "Gemini",
    "Cancer", "Leo", "Virgo",
    "Libra", "Scorpio", "Sagittarius",
    "Capricorn", "Aquarius", "Pisces"
  )

zodiac_signs_modern_order <- c(
    "Capricorn", "Aquarius", "Pisces",
    "Aries", "Taurus", "Gemini",
    "Cancer", "Leo", "Virgo",
    "Libra", "Scorpio", "Sagittarius"
  )

usethis::use_data(zodiac_signs_roman_order, overwrite = TRUE)
usethis::use_data(zodiac_signs_modern_order, overwrite = TRUE)

## Zodiac colors
zodiac_sign_colors <- c(
  Aries = "#f64e4b",
  Taurus = "#87d67d",
  Gemini = "#F9DC5C",
  Cancer = "#98a6b8",
  Leo = "#fe7776",
  Virgo = "#645244",
  Libra = "#ffadcf",
  Scorpio = "#3f3c3c",
  Sagittarius = "#b493d2",
  Capricorn = "#6E7271",
  Aquarius = "#94e7ff",
  Pisces = "#a6f3d1"
)

usethis::use_data(zodiac_sign_colors, overwrite = TRUE)

sample_dates <- seq(
  from = as.Date("2000-01-01"),
  to = as.Date("2000-12-31"),
  by = "days"
  ) %>%
  tibble::tibble(
    date = .,
    x = sample(0:100, length(.), replace = TRUE),
    y = sample(0:100, length(.), replace = TRUE)
    )

usethis::use_data(sample_dates, overwrite = TRUE)
