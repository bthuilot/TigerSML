all: factorial.s


factorial.s:
	sml run.sml factorial.tig factorial.s
