# http_proxy=http://213.138.103.234:8888/

ACLOCAL=aclocal -I/usr/share/aclocal -I/usr/local/share/aclocal
export ACLOCAL
PATH:=/usr/local/bin:$(PATH)

APT=PATH=/sbin:/usr/sbin:$$PATH apt-get install

all: /usr/local/lib/libGLESv2.la /usr/bin/java /usr/local/bin/lein

download = curl $(1) -o $@

/usr/bin/curl:
	$(APT) curl python \
	python-libxml2 x11proto-gl-dev \
	pkg-config libdrm2 libdrm-dev libdrm-omap1 x11proto-dri2-dev autoconf \
	automake libtool autotools-dev vim-tiny strace bison bison \
	flex gettext gettext libxml2-dev libexpat1-dev llvm-3.1-dev \
	git openssh-client libfreetype6-dev avahi-utils

/usr/bin/java:
	-groupadd messagebus 
	$(APT) openjdk-7-jdk

/usr/local/bin/lein: /usr/bin/java
	$(call download, https://raw.github.com/technomancy/leiningen/stable/bin/lein)
	chmod 755 $@

Mesa-10.0.3.tar.bz2: /usr/bin/curl
	$(call download, ftp://ftp.freedesktop.org/pub/mesa/10.0.3/MesaLib-10.0.3.tar.bz2)

automake-1.14.tar.xz: /usr/bin/curl
	$(call download, http://ftp.gnu.org/gnu/automake/automake-1.14.tar.xz)

/usr/local/bin/automake: automake-1.14.tar.xz
	tar xf $< && cd automake-1.14 && ./configure && make install

Mesa-10.0.3/unpack-stamp: Mesa-10.0.3.tar.bz2
	tar jxf $<
	touch $@

# XXX if this fails with an error about aclocal 1.14 not found, run it
# again.  This is probably because it doesn't detect an automake
# installed in the same run of make, or something like that

Mesa-10.0.3/config.status: Mesa-10.0.3/unpack-stamp /usr/local/bin/automake
	cd `dirname $<` &&  ./configure --disable-xlib-glx \
	--enable-gles1 --enable-gles2 \
	 --disable-glx \
	 --with-egl-platforms=fbdev --disable-xvmc \
	 --with-gallium-drivers=swrast --enable-gallium-llvm \
	 --enable-gallium-egl --enable-shared-glapi --enable-dri  \
	 --with-dri-drivers=swrast 

/usr/local/lib/libGLESv2.la: Mesa-10.0.3/config.status
	make -C `dirname $<` install

