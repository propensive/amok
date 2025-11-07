<img src="/doc/images/github.png" valign="middle">

## Amok

Amok is a tool for working with documentation, including presentations. It is incomplete, and
available as a prerelease version which works on Mac OS X and Linux.

### Installation

Download the prerelease version of [amok](https://github.com/propensive/amok), and put it onto your
PATH, for example in `/usr/local/bin/amok`. Make sure that the file is executable.

Check that it runs with:
```sh
> amok about
```

You should see a friendly welcome message.

## Getting Started

The prerelease version only supports presentations.

### Presentations

First, write a source file containing your presentation. For example, a simple file
`presentation.amok` might look like the following:

```amok
version  1
format   presentation
title    Sample presentation
##

# Your first slide

This is some Markdown content for the first slide.

---

## A second slide

- items can be placed in a list
- items will appear one-by-one
- until they are all done

```

That file can be displayed with,
```sh
> amok load presentation.amok
```
and it will be loaded into memory.

It can then be served from port 8080 with:
```sh
> amok serve
```

If everything worked, you should be able to view the presentation at `http://localhost:8080/`.

Note that the file format is quite strict, and whitespace is significant. It will be fully
documented later.

#### Code samples

Amok allows Scala code samples to be included and animated. These should be added in `amok`
fenced-code blocks in the markdown, like so:

`````markdown
```amok
frame
    def run(): Unit = ???
frame
    def run(): Unit = println("Hello world")
frame
    @main
    def run(): Unit = ???
```
`````

Each step in the animation should be introduced with the `frame` keyword, and the content should
appear on the following lines, indented with four spaces.

Amok will parse each frame as Scala, and interpolate between the different frames.

#### Custom stylesheets

The CSS for the content can be customized by adding a `styles` keyword in the header with a `css`
child. This should be formatted like so:

```
version  1
format   presentation
title    Your title
styles
  css
      /* Custom CSS styles can be added here */
      body { font-family: "Times New Roman"; }
##
```

Note that whitespace is significant, and each line of CSS must be indented by six spaces.

#### Limitations

- external resources like images cannot be included in a presentation, unless they are stored
  elsewhere
- errors in markdown or
