ALL=index.html manual.html defone-logo.png arch.html unsemantic-grid-responsive.css
all: $(ALL)

manual.html: manual.org defone-logo.png
	emacs --batch --visit=$< --funcall org-export-as-html-batch

defone-logo.png: defone-logo.svg
	inkscape -b white -y 1 -d 315 -D -e $@ $<

upload: $(ALL)
	rsync -v $(ALL) dan@sehll.telent.net:/var/www/www.defone.org/public_html/
