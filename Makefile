SRC = λ.l.c λ.y.c
OBJ = $(SRC:.c=.o)

all: λ

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

λ.l.c: λ.l
	lex --header-file=λ.l.h -o λ.l.c λ.l

λ.y.c: λ.y
	yacc -d -o λ.y.c λ.y

λ: $(OBJ)
	$(LD) $(LDFLAGS) $(OBJ) -ly -o λ

test: all
	./λ < I.λ

clean:
	rm -f λ.l.c λ.l.h λ.y.c λ.y.h

.PHONY: all test clean
