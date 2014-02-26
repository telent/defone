PATH:=/sbin:/usr/sbin:/usr/local/lib/android-sdk-linux/tools/:/usr/local/lib/android-sdk-linux/platform-tools/:$(PATH)
TOP=$(shell pwd)
R=$(TOP)/build
http_proxy=http://loaclhost:3142/

root: $(R)/etc/shells $(R)/etc/passwd $(R)/etc/shadow $(R)/etc/resolv.conf \
 $(R)/etc/dropbear/dropbear_rsa_host_key $(R)/lib/android \
 $(TOP)/dpkg $(R)/root/.ssh/authorized_keys \
 $(R)/root/src


clean:
	-rm -r $(R)

$(R)/lib/android $(TOP)/dpkg:
	mkdir $@

$(R)/etc/shells: multistrap-config
	multistrap -f multistrap-config

$(R)/etc/passwd: /etc/passwd
	cp $< $@

$(R)/etc/shadow: /etc/shadow
	cp $< $@

$(R)/etc/resolv.conf: /etc/resolv.conf
	cp $< $@

$(R)/etc/dropbear/dropbear_rsa_host_key:
	mkdir -p $(shell dirname $@)
	$(TOP)/bin/dropbearkey -t rsa -f $@

$(R)/root/.ssh/authorized_keys: /home/dan/.ssh/id_dsa.pub
	mkdir -p $(shell dirname $@)
	cp $< $@

$(R)/root/src: $(TOP)/../src
	mkdir -p $(shell dirname $@)
	cp -r $< $@

sync: root setup
	rsync -u --exclude /lib/android -av build/ rsync://root@localhost:8873/root 
	adb shell "sh /data/debian/start.rc"

setup:
	adb root
	adb shell "mkdir -p /data/debian"
	adb push rsync.conf /data/debian/rsync.conf
	adb push start.rc /data/debian/start.rc
	adb push stop.rc /data/debian/stop.rc
	adb shell "rsync --daemon --port=8873 --config=/data/debian/rsync.conf"
	adb forward tcp:8873 tcp:8873
	adb forward tcp:2222 tcp:22
	sleep 1 # time for rsync to start

stop:
	adb shell "sh /data/debian/stop.rc"

clean-device:
	adb shell "rm -rf /data/debian"



# shell "echo -e '[root]\n path=/data/debian' > /data/debian/rsync.conf"

