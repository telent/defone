all: index.html readme.html

readme.org: ../README.org
	cp $< $@

readme.html: readme.org defone-logo.png
	emacs --batch --visit=readme.org --funcall org-export-as-html-batch

defone-logo.png: defone-logo.svg
	inkscape -b white -y 1 -d 315 -D -e $@ $<

upload: 
	rsync -av . dan@sehll.telent.net:/var/www/www.defone.org/public_html/
