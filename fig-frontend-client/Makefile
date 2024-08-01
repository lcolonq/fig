.PHONY: all dist deploy extension clean

TEMPLATES=$(shell ls templates)

all: dist $(addprefix dist/test/,$(TEMPLATES)) dist/test/assets dist/test/main.js dist/test/main.css

deploy: dist $(addprefix dist/deploy/,$(TEMPLATES)) dist/deploy/assets dist/deploy/main.js dist/deploy/main.css
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

dist/extension/main.css: extension/main.css dist
	cp $< $@

dist/%/main.css: main.css dist
	cp $< $@

dist/%/index.html: config-%.m4 dist index-template.html
	sh -c "m4 $< >$@"


define GEN_RULE
dist/%/$(template): config-%.m4 templates/$(template)
	sh -c "m4 $$^ >$$@"
endef
$(foreach template,$(TEMPLATES), $(eval $(GEN_RULE)))

extension: dist dist/extension/assets dist/extension/manifest.json dist/extension/background.js dist/extension/main.js dist/extension/main.css dist/extension/config.js

dist/extension/manifest.json: extension/manifest.dhall
	dhall-to-json <$< >$@

dist/extension/config.js: config-extension.js
	cp $< $@

dist/extension/%: extension/%
	cp $< $@

clean:
	rm main.js
	rm -r dist/
