MKDIR_P := mkdir -p

FEZ := ./src/fez/fez.fsproj
FEZ_CORE := ./src/fez.core/fez.core.fsproj
TST := ./test/test.fsproj

VERSION := 0.1.0
SUFFIX := alpha.1

.PHONY: restore build all clean test ebin

all: restore build

ebin:
	mkdir -p ebin && \
	erlc +clint -o ./ebin ./src/*.erl

restore:
	dotnet restore $(FEZ_CORE)
	dotnet restore $(FEZ)
	dotnet restore $(TST)

build: ebin
	dotnet build $(FEZ_CORE)
	dotnet build $(FEZ)



test:
	./run-tests.sh

clean:
	dotnet clean $(FEZ)
	dotnet clean $(FEZ_CORE)
	dotnet clean $(TST)
	rm -rf ./releases
	rm -rf ./ebin

prep-releases:
	rm -rf ./releases
	mkdir ./releases

rids := win-x64 osx-x64 linux-x64

$(rids): prep-releases
	rm -rf tmp && \
	dotnet publish $(FEZ) --self-contained -c Release -r $@ -o ../../tmp/$@ && \
	cd tmp && \
	zip -r ../releases/$@_$(VERSION)-$(SUFFIX).zip $@ && \
	cd .. && \
	rm -rf tmp

release: $(rids)
