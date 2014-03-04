PATH:=/sbin:/usr/sbin:/usr/local/lib/android-sdk-linux/tools/:/usr/local/lib/android-sdk-linux/platform-tools/:$(PATH)
TOP=$(shell pwd)
PARENT=$(shell cd .. && /bin/pwd)
R=$(TOP)/build
http_proxy=http://loaclhost.lan:3142/

default: all

root: $(R)/etc/shells $(R)/etc/passwd $(R)/etc/shadow $(R)/etc/resolv.conf \
 $(R)/etc/dropbear/dropbear_rsa_host_key $(R)/lib/android \
 $(TOP)/dpkg $(R)/root/.ssh/authorized_keys 


clean:
	-rm -r $(R)

$(R)/lib/android $(TOP)/dpkg:
	mkdir $@

$(R)/etc/shells: multistrap-config
	-mv $(R)/var/lib/dpkg/status $(R)/var/lib/dpkg/status.old
	multistrap -f multistrap-config

$(R)/etc/passwd: /etc/passwd
	cp $< $@

$(R)/etc/shadow: /etc/shadow
	cp $< $@

$(R)/etc/resolv.conf: /etc/resolv.conf
	cp $< $@

$(R)/root/.ssh/authorized_keys:
	mkdir -p $(shell dirname $@)
	( ( cat $@ ; ssh-add -L ) | uniq > $$$$) && mv $$$$ $@

$(R)/root/src: $(TOP)/../src
	mkdir -p $(shell dirname $@)
	cp -r $< $@

sync: root setup
	rsync -u --exclude /lib/android -av build/ rsync://root@localhost:8873/root 
	adb shell "sh /data/debian/start.rc"

setup: busybox.android rsync.android
	adb root
	adb shell "mkdir -p /data/debian"
	adb push rsync.conf /data/debian/rsync.conf
	adb push start.rc /data/debian/start.rc
	adb push stop.rc /data/debian/stop.rc
	adb  remount
	adb  push rsync.android /data/debian/rsync.android
	adb  push busybox.android /data/debian/busybox.android
	adb  shell chmod 755 /data/debian/*.android
	adb forward tcp:8873 tcp:8873
	adb forward tcp:2222 tcp:22
	adb shell "/data/debian/rsync.android --daemon --port=8873 --config=/data/debian/rsync.conf &"
	sleep 1 # time for rsync to start

all: sync stage2

copy-stage2: setup
	ssh -A -p2222 root@localhost mkdir -p /defone
	cd $(PARENT) && ( git ls-files && echo .git ) |rsync -u -r -v --files-from=- . rsync://root@localhost:8873/root/defone 


stage2: copy-stage2
	ssh -p 2222 root@localhost make -C /defone/stage2


stop:
	adb shell "sh /data/debian/stop.rc"

clean-device:
	adb shell "rm -rf /data/debian"


busybox.android:
	curl http://busybox.net/downloads/binaries/latest/busybox-armv7l -o $@

# XXX rsync.android was extracted from a device after installing and
# running rsync4android.apk.  I don't know its provenance any better
# than that

emulator: busybox.android
	# this might ask if you want a custom profile.  seems to work
	# fine if you say no
	-android create avd -n defone  -t 3 
	adb -e shell id || (HOME=/root emulator -partition-size 2048 -avd defone ) &
	adb -e wait-for-device
