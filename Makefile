
default: dev

# figwheel:
# 	lein trampoline run -m figwheel.main -b dev -r

build:
	lein cljsbuild once

deploy: build
	scp -r resources/public/* felipe@felipereigosa.com:/home/felipe/website/resources/public/world/

tree:
	tree -C src resources/ | less -r

clean:
	lein clean
	rm -fr resources/public/cljs-out/
