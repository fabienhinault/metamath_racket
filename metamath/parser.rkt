#lang brag
; grammar of a sublanguage of Metamath.
; includes and compressed proofs are absent.
; disjoints are present, although they will not be checked.

; see https://us.metamath.org/downloads/metamath.pdf p.211
database : outermost-scope-stmt*
@outermost-scope-stmt : constant-stmt | stmt
constant-stmt : /"$c" constant+ /"$."
@stmt : block | variable-stmt | disjoint-stmt | hypothesis-stmt | assert-stmt
block : /"${" stmt* /"$}"
variable-stmt : /"$v" variable+ /"$."
disjoint-stmt : "$d" variable variable variable* "$."
@hypothesis-stmt : floating-stmt | essential-stmt
floating-stmt : LABEL /"$f" typecode variable /"$."
essential-stmt : LABEL /"$e" typecode MATH-SYMBOL* /"$."
@assert-stmt : axiom-stmt | provable-stmt
axiom-stmt : LABEL /"$a" typecode MATH-SYMBOL* /"$."
provable-stmt : LABEL /"$p" typecode MATH-SYMBOL* /"$=" proof /"$."
proof : (LABEL | "?")+
@typecode : constant
@constant : MATH-SYMBOL
@variable : MATH-SYMBOL
@LABEL : _LABEL
@MATH-SYMBOL : _LABEL | _MATH-SYMBOL

