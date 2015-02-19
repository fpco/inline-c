CC_FLAGS = -I/opt/NAG/cll6i24dcl/include -Wall
LD_FLAGS = -lm -lpthread -lnagc_nag -L/opt/NAG/cll6i24dcl/lib

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %, $(SOURCES))

.PHONY: all
all: $(EXECS)

%: %.c
	gcc -o $@ $? $(CC_FLAGS) $(LD_FLAGS)

.PHONY: clean
clean:
	rm -f $(EXECS)
