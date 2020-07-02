run:
	cabal run discord-terminal-exe -- fake-token

ghcid:
	ghcid -c "cabal repl discord-terminal"

ghcid-test:
	ghcid -c "cabal repl discord-terminal-test"\
		-T main -s ":set -isrc"
