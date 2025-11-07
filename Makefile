tool:
	rm -f tool
	mill amok.tool.assembly
	echo $$(( $$(<res/build.id) + 1 )) > res/build.id
	jps | grep amok | cut -d' ' -f1 | xargs kill || echo Not running
	java -Dbuild.executable=tool -Dbuild.java.minimum=23 -Dbuild.java.preferred=25 -Dbuild.java.bundle=jdk -jar out/amok/tool/assembly.dest/out.jar
	cp tool ~/.local/bin/amok

release:
	rm -rf /Users/propensive/Library/Caches/Coursier
	make tool
	find /Users/propensive/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ -name '*.jar' | grep -v '%' > .classpath
	cat .classpath | xargs java -cp out/amok/tool/assembly.dest/out.jar burdock.Bootstrapper
	mkdir -p release
	java -Dbuild.executable=release/amok -Dbuild.java.minimum=23 -Dbuild.java.preferred=25 -Dbuild.java.bundle=jdk -jar out/amok/tool/assembly.dest/out.jar

build:
	mill clean
	mill amok.runner.assembly
	echo $$(( $$(<res/build.id) + 1 )) > res/build.id
	rm runner
	java -Dbuild.executable=runner -jar out/amok/runner/assembly.dest/out.jar
	cp runner ~/.local/bin/amok

test:
	mill amok.test.assembly
	java -cp out/amok/test/assembly.dest/out.jar amok.Tests

dev:
	mill -w amok.tool.compile

.PHONY: tool build dev
