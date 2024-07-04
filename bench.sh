cargo build --release

cmd='echo "'"$1\" "'| ./target/release/x9'
hyperfine "$cmd"