TOP=$(shell pwd)
R=$(TOP)/build

root: $(R)/etc/shells $(R)/etc/passwd $(R)/etc/shadow $(R)/etc/resolv.conf \
 $(R)/etc/dropbear/dropbear_rsa_host_key $(R)/start.rc $(R)/lib/android \
 $(TOP)/dpkg

$(R)/lib/android $(TOP)/dpkg:
	mkdir $@

$(R)/etc/shells: multistrap-config
	multistrap -f multistrap-config

$(R)/start.rc: $(TOP)/start.rc
	cp $< $@

$(R)/etc/passwd: /etc/passwd
	cp $< $@

$(R)/etc/shadow: /etc/shadow
	cp $< $@

$(R)/etc/resolv.conf: /etc/resolv.conf
	cp $< $@

$(R)/etc/dropbear/dropbear_rsa_host_key:
	mkdir -p $(shell dirname $@)
	$(TOP)/bin/dropbearkey -t rsa -f $@

sync: setup
	rsync -u --exclude /lib/android -av build/ rsync://root@localhost:8873/root 
	adb shell "sh /data/debian/start.rc"

setup:
	adb shell "mkdir -p /data/debian"
	adb push rsync.conf /data/debian/rsync.conf
	adb push start.rc /data/debian/start.rc
	adb shell "rsync --daemon --port=8873 --config=/data/debian/rsync.conf"
	adb forward tcp:8873 tcp:8873


# shell "echo -e '[root]\n path=/data/debian' > /data/debian/rsync.conf"

