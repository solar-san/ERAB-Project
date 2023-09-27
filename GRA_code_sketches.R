cor(iacofi_full$qd7, iacofi_full$edu)
cor(iacofi_full$qd7, iacofi_full$qd1)
cor(iacofi_full$qd7, iacofi_full$FK)
cor(iacofi_full$qd7, iacofi_data$risk_proxy)
cor(iacofi_full$qd7, iacofi_full$reddito)
cor(iacofi_full$qd7, iacofi_full$area)

datamatrix <- matrix(
  c(
    "cor(gender, edu)" = cor(iacofi_full$qd1, iacofi_full$edu),
    "cor(gender, age)" = cor(iacofi_full$qd7, iacofi_full$qd1),
    "cor(gender, FK)" = cor(iacofi_full$qd1, iacofi_full$FK),
    "cor(gender, income)" =cor(iacofi_full$qd1, iacofi_full$reddito),
    "cor(gender, area)" =cor(iacofi_full$qd1, iacofi_full$area),
    "cor(edu, age)" = cor(iacofi_full$qd7, iacofi_full$qd1),
    "cor(edu, FK)" = cor(iacofi_full$qd1, iacofi_full$FK),
    "cor(edu, income)" =cor(iacofi_full$qd1, iacofi_full$reddito),
    "cor(edu, area)" =cor(iacofi_full$qd1, iacofi_full$area),
    "cor(FK, age)" = cor(iacofi_full$qd7, iacofi_full$qd1),
    "cor(FK, income)" =cor(iacofi_full$qd1, iacofi_full$reddito),
    "cor(FK, area)" =cor(iacofi_full$qd1, iacofi_full$area),
    "cor(income, area)" =cor(iacofi_full$qd1, iacofi_full$area)
  ),
  2036,
  13
)

qr(datamatrix)$rank
