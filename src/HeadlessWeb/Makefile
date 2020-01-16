MODULE=HeadlessWeb
MAIN=HeadlessWeb.js
SRCS=HeadlessWeb.js \
	WebProcedure.js
# NOTE: We specifically require jsdom@11.6.0 because later versions apparently
# don't work correctly; in particular 12.1.0+ uses saxes instead of sax, which
# doesn't allow us to hack its XML entities list to include common things like
# &nbsp;. Also, later versions of jsdom fail to pull in the required peer
# dependency called request, so if ever we do upgrade jsdom we need to add
# request here too.
REQUIRES=jquery jsdom@11.6.0

CONFIGDIR=../config
include $(CONFIGDIR)/javascript/prog.mk

