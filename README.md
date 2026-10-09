# atbp

Assorted tooling bits and pieces.

## GitHub release

Download `atbp-<version>.zip` from [GitHub Releases](https://github.com/esamson/atbp/releases) for the tag you want.

Unpack the zip. The distribution requires Java 25 on the machine where you run it.

From the unpacked directory:

```bash
bin/atbp --help
```

## Local installation

Requires Java 25 and sbt. From the repository root, run:

```bash
sbt installLocal
```

This builds the tools and installs them in `~/.local/share/atbp`.
It symlinks `~/.local/bin/atbp` to `~/.local/share/atbp/bin/atbp`.

This assumes `~/.local/bin` is on your `PATH`.

```bash
export PATH="$HOME/.local/bin:$PATH"
atbp --help
```

Run `installLocal` again to update the distribution and repair the symlink.
