#lang reader "reader.rkt"

; comment
($c + = -> term wff TT 0 |(| |)|)
($v u r s P Q)
($f tu term u)
($f tr term r)
($f ts term s)
($f wp wff P)
($f wq wff Q)

($a tze () () (term 0))
($a tpl () () (term |(| u + r |)|))
($a weq () () (wff u = r))
($a wim () () (wff |(| P -> Q |)|))
($a a1 () () (TT |(| u = r -> |(| u = s -> r = s |)|))
($a a2 () () (TT |(| u + 0 |)| = u))
($a mp (min (TT P) maj (TT |(| P -> Q |)|)) () (TT Q))

($p th1 () () (TT (= u u))
    (tu tze tpl tu weq tu tu weq tu a2 tu tze tpl
     tu weq tu tze tpl tu weq tu tu weq wim tu a2
     tu tze tpl tu tu a1 mp mp))