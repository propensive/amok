tool:
	rm -f tool
	mill amok.tool.assembly
	echo $$(( $$(<res/build.id) + 1 )) > res/build.id
	jps | grep amok | cut -d' ' -f1 | xargs kill || echo Not running
	find /Users/propensive/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ -name '*.jar' | grep -v '%' > .classpath
	#cat .classpath | xargs java -cp out/amok/tool/assembly.dest/out.jar burdock.Bootstrapper
	java -Dbuild.executable=tool -Dbuild.java.minimum=23 -Dbuild.java.preferred=25 -Dbuild.java.bundle=jdk -jar out/amok/tool/assembly.dest/out.jar
	cp tool ~/.local/bin/amok

build:
	mill clean
	mill amok.runner.assembly
	echo $$(( $$(<res/build.id) + 1 )) > res/build.id
	rm runner
	java -Dbuild.executable=runner -jar out/amok/runner/assembly.dest/out.jar
	cp runner ~/.local/bin/amok

dev:
	mill -w amok.tool.compile

.PHONY: tool build dev
