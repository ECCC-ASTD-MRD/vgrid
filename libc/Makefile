
LIB_NAME		:= cvgrid
LIB_VERSION := 5.4.0

CC := s.compile
FC := s.compile
LD := ld -shared -x
AR := s.ar

SRC_H := $(subst .c,.h,$(wildcard *.h))
OBJ_C := $(subst .c,.o,$(wildcard *.c))

LIB_SHARED 	:= lib$(LIB_NAME).so
LIB_SHAREDV	:= lib$(LIB_NAME).so.$(LIB_VERSION)
LIB_STATIC 	:= lib$(LIB_NAME).a
LIB_STATICV	:= lib$(LIB_NAME)-$(LIB_VERSION).a
LIB_ALL		:= $(LIB_SHARED) $(LIB_STATIC)

COPTIONS		:= -O 2
LDOPTIONS	+= -librmn

$(SRC_H): $@.h


%.o: %.c
	$(CC) -src $< $(COPTIONS)


obj: $(OBJ_C)


$(LIB_SHAREDV): obj
	$(CC) -shared -o $@ -obj $(OBJ_C) $(LDOPTIONS)


$(LIB_SHARED): $(LIB_SHAREDV)
	ln -fs $< $@


$(LIB_STATICV): obj
	$(AR) rv $@ $(OBJ_C)


$(LIB_STATIC): $(LIB_STATICV)
	ln -fs $< $@


lib: $(LIB_ALL)


all: lib


clean:
	-rm -f $(OBJ_C)


clear: clean
	-rm -f $(LIB_ALL) $(LIB_STATICV) $(LIB_SHAREDV)


.PHONY: obj lib clean clear all

