# http_proxy=http://213.138.103.234:8888/

ACLOCAL=aclocal -I/usr/share/aclocal -I/usr/local/share/aclocal
export ACLOCAL

APT=PATH=/sbin:/usr/sbin:$$PATH apt-get install

all: mesa

download = curl $(1) -o $@

/usr/bin/curl:
	$(APT) curl python \
	python-libxml2 x11proto-gl-dev \
	pkg-config libdrm2 libdrm-dev libdrm-omap1 x11proto-dri2-dev autoconf \
	automake libtool autotools-dev vim-tiny strace bison bison \
	flex gettext gettext libxml2-dev libexpat1-dev llvm-3.1-dev 

/usr/bin/java:
	-groupadd messagebus 
	$(APT) openjdk-7-jdk

lein: /usr/bin/java
	$(call download, https://raw.github.com/technomancy/leiningen/stable/bin/lein)

Mesa-10.0.3.tar.bz2: /usr/bin/curl
	$(call download, ftp://ftp.freedesktop.org/pub/mesa/10.0.3/MesaLib-10.0.3.tar.bz2)

automake-1.14.tar.xz: /usr/bin/curl
	$(call download, http://ftp.gnu.org/gnu/automake/automake-1.14.tar.xz)

/usr/local/bin/automake: automake-1.14.tar.xz
	tar xf $< && cd automake-1.14 && ./configure && make install

Mesa-10.0.3: Mesa-10.0.3.tar.bz2
	tar jxf $<

mesa: Mesa-10.0.3 /usr/local/bin/automake
	cd $< && ./autogen.sh && ./configure --disable-xlib-glx \
	--enable-gles1 --enable-gles2 \
	 --disable-glx \
	 --with-egl-platforms=fbdev --disable-xvmc \
	 --with-gallium-drivers=swrast --enable-gallium-llvm \
	 --enable-gallium-egl --enable-shared-glapi --enable-dri  \
	 --with-dri-drivers=swrast \
	&& make

