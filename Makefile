CC ?= cc
CFLAGS ?= -Wall -Wextra -O2
LD ?= $(CC)
LDFLAGS ?= $(CFLAGS)
LEX ?= lex
YACC ?= yacc

SRC = λ.l.c λ.y.c
OBJ = $(SRC:.c=.o)

all: λ

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

λ.l.c: λ.l
	$(LEX) --header-file=λ.l.h -o λ.l.c λ.l

λ.y.c: λ.y
	$(YACC) -d -o λ.y.c λ.y

λ: $(OBJ)
	$(LD) $(LDFLAGS) $(OBJ) -ly -o λ

test: all
	$(foreach f,$(wildcard test/*),./λ < $(f);)

clean:
	rm -f λ.l.c λ.l.h λ.y.c λ.y.h

.PHONY: all test clean
