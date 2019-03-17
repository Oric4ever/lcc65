SHELL=/bin/sh
OBJ=build
SRC=src

COBJS=$(OBJ)/dag.o \
	$(OBJ)/decl.o \
	$(OBJ)/enode.o \
	$(OBJ)/error.o \
	$(OBJ)/expr.o \
	$(OBJ)/init.o \
	$(OBJ)/input.o \
	$(OBJ)/lex.o \
	$(OBJ)/main.o \
	$(OBJ)/output.o \
	$(OBJ)/profio.o \
	$(OBJ)/simp.o \
	$(OBJ)/stmt.o \
	$(OBJ)/string.o \
	$(OBJ)/sym.o \
	$(OBJ)/tree.o \
	$(OBJ)/types.o \
	$(OBJ)/gen.o

$(OBJ)/rcc:	$(COBJS)
		$(CC) $(LDFLAGS) -o $@ $(COBJS)

$(COBJS):	$(SRC)/c.h $(SRC)/token.h $(SRC)/ops.h $(SRC)/config.h

#CCOPT=-D$(TARGET)_$(OS) $(CFLAGS) $(INCLUDES)
CCOPT= $(CFLAGS) $(INCLUDES)

$(OBJ)/lex.o:	$(SRC)/keywords.h

$(OBJ)/dag.o:	$(SRC)/dag.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/dag.c
$(OBJ)/decl.o:	$(SRC)/decl.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/decl.c
$(OBJ)/enode.o:	$(SRC)/enode.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/enode.c
$(OBJ)/error.o:	$(SRC)/error.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/error.c
$(OBJ)/expr.o:	$(SRC)/expr.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/expr.c
$(OBJ)/gen.o:	$(SRC)/gen.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/gen.c
$(OBJ)/init.o:	$(SRC)/init.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/init.c
$(OBJ)/input.o:	$(SRC)/input.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/input.c
$(OBJ)/lex.o:	$(SRC)/lex.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/lex.c
$(OBJ)/main.o:	$(SRC)/main.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/main.c
$(OBJ)/output.o:$(SRC)/output.c;cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/output.c
$(OBJ)/profio.o:$(SRC)/profio.c;cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/profio.c
$(OBJ)/simp.o:	$(SRC)/simp.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/simp.c
$(OBJ)/stmt.o:	$(SRC)/stmt.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/stmt.c
$(OBJ)/string.o:$(SRC)/string.c;cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/string.c
$(OBJ)/sym.o:	$(SRC)/sym.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/sym.c
$(OBJ)/tree.o:	$(SRC)/tree.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/tree.c
$(OBJ)/types.o:	$(SRC)/types.c;	cd $(OBJ); $(CC) -c $(CCOPT) ../$(SRC)/types.c
