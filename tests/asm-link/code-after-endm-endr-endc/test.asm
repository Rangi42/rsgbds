println "start"

MACRO mac
	println \1
ENDM println "<_<"
	mac "argument"

REPT 3
	println "hey!"
ENDR println "<_<"

IF 0
	println "skipped"
	bad syntax!?
ELSE println "<_<"
	println "else clause"
ENDC println "<_<"

IF 1
	println "if clause"
ELSE println "<_<"
	println "still skipped"
ENDC println "<_<"

println "done"
