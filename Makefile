OPA=opa
OPA_OPTIONS=

GIT=git

LOGS=error.log access.log
BUILD_DIRS=_build _tracks *.opx
SRC=src/tetris.opa src/main.opa

EXEC=main.exe

all:
	$(OPA) $(OPA_OPTIONS) $(SRC) -o $(EXEC)

clean:
	rm -f $(EXEC)
	rm -fr $(BUILD_DIRS)
	rm -fr $(LOGS)
	$(GIT) clean -f
