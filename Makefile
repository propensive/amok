tool:
	mill clean
	rm -f amok
	mill amok.tool.assembly
	echo $$(( $$(<res/build.id) + 1 )) > res/build.id
	jps | grep amok | cut -d' ' -f1 | xargs kill || echo Not running
	#cat classpath | xargs java -cp out/amok/tool/assembly.dest/out.jar burdock.Bootstrapper
	java -Dbuild.executable=tool -jar out/amok/tool/assembly.dest/out.jar
	cp tool ~/.local/bin/amok

build:
	mill clean
	mill amok.runner.assembly
	echo $$(( $$(<res/build.id) + 1 )) > res/build.id
	rm runner
	#java -Dbuild.executable=runner -jar out/amok/runner/assembly.dest/out.jar
	#cp runner ~/.local/bin/amok

dev:
	mill -w amok.tool.compile

.PHONY: tool build dev
