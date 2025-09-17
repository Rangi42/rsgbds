MACRO mac
	for i, -1, -_NARG - 1, -1
		println "{d:i}: \<i>"
	endr
	; error cases
	def i = 0
	println "{d:i}: \<i> == \<0>"
	def i = -_NARG - 1
	println "{d:i}: \<i> == \<-8>"
	def i = $7fff_ffff
	println "{d:i}: \<i> == \<2147483647>"
	; signed/unsigned difference error cases
	def i = $8000_0000
	println "{d:i}: \<i> == \<-2147483648>"
	println "{u:i}: \<i> == \<2147483648>"
	def i = $ffff_ffff
	println "{d:i}: \<i> == \<-1>"
	println "{u:i}: \<i> == \<4294967295>"
ENDM

mac A, B, C, D, E, F, G
