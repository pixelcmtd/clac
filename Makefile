CC ?= cc
CFLAGS ?= -Wall -Wextra -O2
LD ?= $(CC)
LDFLAGS ?= $(CFLAGS)
LEX ?= lex
YACC ?= yacc

SRC = obj/λ.l.c obj/λ.y.c
OBJ = $(SRC:.c=.o)

$(shell mkdir -p obj)

all: λ

obj/%.o: obj/%.c
	$(CC) $(CFLAGS) -Iobj -c $< -o $@

obj/λ.l.c: λ.l
	$(LEX) --header-file=obj/λ.l.h -o obj/λ.l.c λ.l

obj/λ.y.c: λ.y
	$(YACC) -d -o obj/λ.y.c λ.y

λ: $(OBJ)
	$(LD) $(LDFLAGS) $(OBJ) -ly -o λ

test: all
	$(foreach f,$(wildcard test/*),./λ < $(f);)

clean:
	rm -rf obj/ λ

.PHONY: all test clean
