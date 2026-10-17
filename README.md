# atbp

Assorted tooling bits and pieces.

## Packaged releases

Downloads are on [GitHub Releases](https://github.com/esamson/atbp/releases).

| File | Machine |
|---|---|
| `atbp-<version>.zip` | Any machine with Java 25 |
| `atbp-linux-amd64` | Linux x86_64, glibc 2.39 or newer (Ubuntu 24.04 and newer) |
| `atbp-linux-arm64` | Linux aarch64, glibc 2.39 or newer (Ubuntu 24.04 and newer) |
| `atbp-darwin-arm64` | macOS on Apple Silicon |

The zip still requires Java 25. Unpack it and run:

```bash
bin/atbp --help
```

The Linux binaries are dynamically linked against glibc. They do not run on Alpine. The macOS binary is an unsigned Apple Silicon executable.

A browser download does not keep the executable bit. On Linux:

```bash
chmod +x atbp-linux-amd64
./atbp-linux-amd64 --help
```

Use `atbp-linux-arm64` the same way on aarch64. On macOS, clear the quarantine flag before the first run:

```bash
chmod +x atbp-darwin-arm64
xattr -d com.apple.quarantine atbp-darwin-arm64
./atbp-darwin-arm64 --help
```

`d2` and Graphviz are not inside the zip or the native binaries. Install them on the machine when you render those diagrams.

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
