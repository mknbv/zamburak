all: zamburak install exp3 ucb

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

clean: demos/ zamburak/
	dune clean
