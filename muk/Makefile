# Dependancies: Lua, libredblack, pthreads...

SOURCES = db.ml net.ml core.ml main.ml
OBJS = db.cmx net.cmx core.cmx main.cmx

OCAMLOPT = ocamlopt

FLAGS = 

SSLPATH=`ocamlfind query ssl`

LDFLAGS = -I $(SSLPATH) ssl.cmxa unix.cmxa

PROGRAM = muk

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(OCAMLOPT) $(LDFLAGS) $(OBJS) -o $(PROGRAM)
	mv $(PROGRAM) ./dist

$(OBJS): $(SOURCES)
	$(OCAMLOPT) $(FLAGS) -c $(SOURCES)

clean:
	rm -rf *.cmx *.cmi *.o *~ *.cmo

dbtest:
	$(OCAMLOPT) $(LDFLAGS) db.ml dbtest.ml
