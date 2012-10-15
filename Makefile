all: build

post:
	mvim posts/`date +%Y-%m-%d`-`echo '$(TITLE)' | sed 's/[ :,]/-/g'`.markdown

preview:
	runhaskell site.hs preview

build:
	runhaskell site.hs rebuild

clean:
	runhaskell site.hs clean

deploy: build
	rm -rf deploy/*
	cp -r _site/* deploy
	git --git-dir=./deploy/.git --work-tree=./deploy add .
	git --git-dir=./deploy/.git --work-tree=./deploy commit -a -m "Deployment `date`"
	git --git-dir=./deploy/.git --work-tree=./deploy push
