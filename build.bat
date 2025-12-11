#! /bin/bash

cargo build --target x86_64-pc-windows-gnu
cargo build --target x86_64-pc-windows-gnu --release
cargo build --target x86_64-unknown-linux-gnu
cargo build --target x86_64-unknown-linux-gnu --release
cargo zigbuild --target x86_64-apple-darwin
cargo zigbuild --target x86_64-apple-darwin --release
cargo zigbuild --target aarch64-apple-darwin
cargo zigbuild --target aarch64-apple-darwin --release
