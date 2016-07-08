# creer la variable "respect de la procédure" pour l'hypertension
# (si 1 veut dire oui)
# var "resp_proc"
# var "bras"
​
​
resp_proc = "OK"
​
if((CAH_PARACL_TAPasDroit) & !is.na(CAH_PARACL_TAPadDroit)) {
  if(CAH_PARACL_TAPasDroit > CAH_PARACL_TAPadDroit) {resp_proc == 'notOK_inversion'}
}
if((CAH_PARACL_TAPasGauche) & !is.na(CAH_PARACL_TAPadGauche)) {
  if(CAH_PARACL_TAPasGauche > CAH_PARACL_TAPadGauche) {resp_proc == 'notOK_inversion'}
}
if((CAH_PARACL_TARefPas) & !is.na(CAH_PARACL_TARefPad)) {
  if(CAH_PARACL_TARefPas > CAH_PARACL_TARefPad) {resp_proc == 'notOK_inversion'}
}
​
if(resp_proc == "OK"){
  if(CAH_PARACL_TACasPartUnil == 1) {
    if(!is.na(CAH_PARACL_TAPasDroit) & !is.na(CAH_PARACL_TAPadDroit) & is.na(CAH_PARACL_TAPasGauche) & is.na(CAH_PARACL_TAPadGauche)) {
      resp_proc = "OK"
      bras = "D"
    }
    else if(!is.na(CAH_PARACL_TAPasGauche) & !is.na(CAH_PARACL_TAPadGauche) & is.na(CAH_PARACL_TAPasDroit) & is.na(CAH_PARACL_TAPadDroit)) {
      resp_proc = "OK"
      bras = "G"
    }
    else {resp_proc = "notOK_incomplet_test"}
  } else {
    if (!is.na(CAH_PARACL_TAPasDroit) & !is.na(CAH_PARACL_TAPadDroit) & !is.na(CAH_PARACL_TAPasGauche) & !is.na(CAH_PARACL_TAPadGauche)) {
      resp_proc = "OK"
      if (CAH_PARACL_TAPasDroit > CAH_PARACL_TAPasGauche) {bras = "D"}
      if (CAH_PARACL_TAPasDroit < CAH_PARACL_TAPasGauche) {bras = "G"}
      if (CAH_PARACL_TAPas Droit == CAH_PARACL_TAPasGauche) {
        if (CAH_PARACL_TAPadDroit > CAH_PARACL_TAPadGauche) {bras = "D"}
        if (CAH_PARACL_TAPadDroit < CAH_PARACL_TAPadGauche) {bras = "G"}
        if (CAH_PARACL_TAPadDroit == CAH_PARACL_TAPadGauche) {bras = "G"} ## A VERIFIER POUR D OU G QUAND TOUT EST EGAL
      }
    }
    else {resp_proc = "notOK_incomplet_test"}
  }
  
  if((bras == CAH_PARACL_TARefBras) & (resp_proc == "OK")) {resp_proc = "OK"} else {resp_proc = "notOK_mauvais_bras"}
  
  if((is.na(CAH_PARACL_TARefPas) | is.na(CAH_PARACL_TARefPad)) & (resp_proc == "OK")) {resp_proc == "notOK_incomplet_Ref"}
  
  if((age > 65) & (diab == 1) & resop_proc == "OK") {
    if((is.na(CAH_PARACL_TAPasOrtho) | is.na(CAH_PARACL_TAPadOrtho)) {resp_proc = "notOK_incomplet_ortho"}
  }
}