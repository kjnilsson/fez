echo $0
./build.sh
rm -rf ./releases/*
mkdir ./releases
VERSION=v0.1.0
SUFFIX=alpha.1

dotnet publish ./src/fez/fez.fsproj --version-suffix $SUFFIX -c Release -r osx.10.10-x64
dotnet publish ./src/fez/fez.fsproj --version-suffix $SUFFIX  -c Release -r win7-x64
dotnet publish ./src/fez/fez.fsproj --version-suffix $SUFFIX -c Release -r debian.8-x64

cp -r ./src/fez/bin/Release/netcoreapp1.1/osx.10.10-x64/publish ./releases/fez-osx.$VERSION-$SUFFIX
cp -r ./src/fez/bin/Release/netcoreapp1.1/debian.8-x64/publish/ ./releases/fez-debian.$VERSION-$SUFFIX
cp -r ./src/fez/bin/Release/netcoreapp1.1/win7-x64/publish/ ./releases/fez-win.$VERSION-$SUFFIX

cd ./releases
zip fez-osx.$VERSION-$SUFFIX.zip ./fez-osx.$VERSION-$SUFFIX/
zip fez-debian.$VERSION-$SUFFIX.zip ./fez-debian.$VERSION-$SUFFIX/
zip fez-win.$VERSION-$SUFFIX.zip ./fez-win.$VERSION-$SUFFIX/
