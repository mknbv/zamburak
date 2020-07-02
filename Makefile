.PHONY: all zamburak clean

all: zamburak exp3 ucb trade

zamburak: zamburak/
	dune build zamburak

install: zamburak/
	dune build @install

exp3: demos/exp3.ml zamburak/
	dune build demos/exp3.exe

exp3.exe: demos/exp3.ml zamburak/
	dune exec demos/exp3.exe

ucb: demos/ucb.ml zamburak/
	dune build demos/ucb.exe

ucb.exe: demos/ucb.ml zamburak/
	dune exec demos/ucb.exe

trade: demos/trade.ml zamburak/ stock/
	dune build demos/trade.exe

trade.exe: trade
	dune exec demos/trade.exe

clean: demos/ zamburak/
	dune clean
