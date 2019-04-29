#!/usr/bin/env bash
RUSTDOCFLAGS="--html-in-header rustdoc/katex-header.html --document-private-items" cargo doc --no-deps
pwd=`pwd`
firefox $pwd/target/doc/voile/index.html
explorer file:///"${pwd/\/c\//C\:\/}"/target/doc/voile/index.html
