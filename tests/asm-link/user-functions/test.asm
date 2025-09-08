; Parameters are captured by reference.
def x = 0
def f() = x * 2
println f()
def x += 1
println f() ; This should produce an (opt-in) warning, since `x` has changed.
redef f() = {x} * 2 ; Use interpolation for by-value capture.
def x += 1
println f()

println g() ; Undefined function.
println x() ; Not a function.

  def g(x)    = x
redef g(x, y) = x + y
redef g(x)    = 0 ; Warn on unused parameter.
redef g(x, y) = 0 ; ...different warning when there are multiple unused ones.
println g(42, 69)
redef g(_x)   = 0 ; Prefixing with an underscore silences the warning.

; Arguments are atomic, so operator precedence doesn't apply across them.
def mult(x, y) = x * y
println mult(1 + 1, 2) ; Should print (1 + 1) * 2 = 4, not 1 + 1 * 2 = 3.

def logic_and(x, y) = x && y
println logic_and(0, 1 / 0) ; TODO: decide whether this should emit the div-0 error or not
println logic_and!(0, 1 / 0) ; TODO: decide whether this should emit the div-0 error or not
