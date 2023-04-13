set -e 

rm -rf dist
rm -rf target/public

npm install
npx shadow-cljs release app

rsync -av --exclude='js' public/* dist/
rsync -av --exclude '*.edn' target/public/* dist/

for file in $(find dist -type f)
do
	gzip $file
	touch $file
	echo "yo $file"
done

tar -czvf dist.tar.gz dist
rm -rf dist
