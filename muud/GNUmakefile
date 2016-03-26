FS=fsharpc
OUT=muud.exe
FILES=main.fs

.phony: clean

$(OUT): $(FILES)
	$(FS) -r System.Net $(FILES) -o $(OUT)

all: $(OUT)

clean:
	/bin/rm -f *.exe
