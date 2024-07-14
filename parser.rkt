#lang brag
; grammar of a sublanguage of Metamath.
; includes and compressed proofs are absent.

; see https://us.metamath.org/downloads/metamath.pdf p.211
database : outermost-scope-stmt*
outermost-scope-stmt : constant-stmt | stmt
constant-stmt : "$c" constant+ "$."
stmt : block | variable-stmt | disjoint-stmt | hypothesis-stmt | assert-stmt
block : "${" stmt* "$}"
variable-stmt : "$v" variable+ "$."
disjoint-stmt : "$d" variable variable variable* "$."
hypothesis-stmt : floating-stmt | essential-stmt
floating-stmt : LABEL "$f" typecode variable "$."
essential-stmt : LABEL "$e" typecode MATH-SYMBOL* "$."
assert-stmt : axiom-stmt | provable-stmt
axiom-stmt : LABEL "$a" typecode MATH-SYMBOL* "$."
provable-stmt : LABEL "$p" typecode MATH-SYMBOL* "$=" proof "$."
proof : (LABEL | "?")+
typecode : constant
constant : MATH-SYMBOL
variable : MATH-SYMBOL
PRINTABLE-SEQUENCE : ( _LETTER-OR-DIGIT | _LABEL-CHARACTER | _MATH-SYMBOL-CHARACTER | _DOLLAR)+
MATH-SYMBOL : ( _LETTER-OR-DIGIT | _LABEL-CHARACTER | _MATH-SYMBOL-CHARACTER)+
LABEL : ( _LETTER-OR-DIGIT | _LABEL-CHARACTER )+
