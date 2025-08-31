if 1
	println "taken if"
elif 2 / 0 ; avoided "Division by zero" error
	println "untaken elif"
elif 3 / 0 ; avoided "Division by zero" error
	println "untaken after untaken"
endc

if 0
	println "untaken if"
elif 1
	println "taken elif"
elif !@#$ ; avoided syntax error
	println "untaken elif"
elif %^&* ; avoided syntax error
	println "untaken after untaken"
endc

if 0
	println "untaken if"
elif 1 / 0
	println "taken elif"
endc
