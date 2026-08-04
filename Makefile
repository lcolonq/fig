.PHONY: web

web:
	cabal build all
	cabal exec fig-web -- --bus-host=shiro --bus-port=32051 public
