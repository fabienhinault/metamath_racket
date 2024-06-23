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
hypothesis-stmt : floating-stmt | essential-stmt
floating-stmt : LABEL "$f" typecode variable "$."
essential-stmt : LABEL "$e" typecode MATH-SYMBOL* "$."
assert-stmt : axiom-stmt | provable-stmt
axiom-stmt : LABEL "$a" typecode MATH-SYMBOL* "$."
provable-stmt : LABEL "$p" typecode MATH-SYMBOL* "$=" proof "$."
proof : (LABEL | "?")+
constant : MATH-SYMBOL
variable : MATH-SYMBOL
PRINTABLE-SEQUENCE : _PRINTABLE-CHARACTER+
MATH-SYMBOL : (_PRINTABLE-CHARACTER - "$")+
;_PRINTABLE-CHARACTER : [#x21-#x7e]
LABEL : ( _LETTER-OR-DIGIT | "." | "-" | "_" )+
_LETTER-OR-DIGIT : [A-Za-z0-9]
WHITESPACE : (_WHITECHAR+ | _COMMENT) -> SKIP
_COMMENT : "$(" (_WHITECHAR+ (PRINTABLE-SEQUENCE - "$)")* _WHITECHAR+ "$)" _WHITECHAR
;_WHITECHAR : [#x20#x09#x0d#x0a#x0c]
