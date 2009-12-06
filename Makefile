ERL          ?= erl
ERLC          = erlc
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := erlsockserver

all: mochiweb  erl ebin/$(APP).app 

erl:
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

mochiweb:
	(cd deps/mochiweb; make)

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	(cd deps/mochiweb; make clean)
	@rm -fv ebin/*.beam ebin/*.app
	@rm -fv erl_crash.dump $(PARSER).erl
	@rm -fv priv/log/*

ebin/$(APP).app:
	@cp -v src/$(APP).app $@

