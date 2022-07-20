# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT = ebus

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so

# System type and C compiler/flags.
# STAGING_DIR is defined by Buildroot for correct cross-compilation

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += $(shell pkg-config --cflags dbus-1)
	CXXFLAGS ?= -O3 -finline-functions -Wall
	LDFLAGS ?= -flat_namespace -undefined suppress
	LDFLAGS += $(shell pkg-config --libs dbus-1)
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += $(shell pkg-config --cflags dbus-1)
	CXXFLAGS ?= -O3 -finline-functions -Wall
	LDFLAGS ?= -L /usr/local/lib
	LDFLAGS += $(shell pkg-config --libs dbus-1)
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	MACHINE ?= $(shell $(CC) -dumpmachine)
	CFLAGS ?= -g3 -ggdb -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -I $(STAGING_DIR)/usr/include/dbus-1.0 -I $(STAGING_DIR)/usr/lib/dbus-1.0/include -I $(STAGING_DIR)/usr/lib/$(MACHINE)/dbus-1.0/include -I $(STAGING_DIR)/usr/lib64/dbus-1.0/include
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR)

LDLIBS += -ldbus-1
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -maxdepth 1 -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

%.o: %.c %.h ebus_shared.h
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
