.PHONY: all deploy extension

all: dist/test/index.html dist/test/assets dist/test/main.js dist/test/main.css

deploy: dist/deploy/index.html dist/deploy/assets dist/deploy/main.js dist/deploy/main.css
	rsync -av dist/deploy/ "pub.colonq.computer:~/public_html/"

dist:
	mkdir -p dist/test
	mkdir -p dist/deploy

main.js: $(shell find src)
	purs-nix bundle

dist/%/assets: $(shell find assets) dist
	rm -rf $@
	mkdir -p $@
	cp -r assets/* $@

dist/%/main.js: main.js dist
	cp $< $@

dist/%/main.css: main.css dist
	cp $< $@

dist/%/index.html: config-%.m4 dist index-template.html
	sh -c "m4 $< >$@"

extension: dist/extension/assets dist/extension/manifest.json dist/extension/background.js dist/extension/main.js dist/extension/main.css

dist/extension/manifest.json: extension/manifest.dhall
	dhall-to-json <$< >$@

dist/extension/%: extension/%
	cp $< $@
