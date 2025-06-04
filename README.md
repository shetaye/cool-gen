# COOL Generator

This program generates a subset of COOL programs.

## Usage

First, build:

```rust
cargo build --release
```

This generates two binaries: `cool-gen` and `cool-gen-harness`. To invoke them, you can either use `cargo run --bin <binary>` or look in `targets/release/`.

`cool-gen` *just* generates a (possibly valid, see below) COOL program on STDOUT.

`cool-gen-harness` is more involved. It is a test harness for a basic fuzzer. You will need to write a part of the harness yourself.

### `cool-gen-harness` Harnessing

The harness executables you write (e.g. a shell script, like ours) must take a COOL file path as the only input and print to STDOUT the output that you want equality checked. In particular, this is where you would filter out debug messages.

Given the nonzero chance that the COOL program is *not* valid, you need to signal this to the harness by exciting with a nonzero exit code (e.g. 1).

You need two harness executables: one that invokes the gold compiler and one that invokes yours.

To invoke the harness, run `cool-gen-harness <your harness> <gold harness> <output_folder>`. Note that `output_folder` should exist. See `cool-gen-harness --help` for more.

The COOL generator can also be configured with the harness. For example, the generated programs tend to be on the long side, which for stack machines in the SPIM simulator can mean stack overflows. This can be resolved by tweaking the generation parameters. We found that reducing the number of rounds (which corresponds to the maximum expression depth) helps. The configuration is provided as a TOML passed via `--config`. We recommend:

```toml
max_block=2
max_case=2
max_attributes=3
n_rounds=2
```

## Limitations (gotchas)

The produced programs have some limitations:
- About 30% of the time, the programs are not in fact correct COOL programs! This is unfortunate but not catastrophic, since we can always generate more COOL programs.
- Complicated subtype usage. It does its best (cases & branches, for example), but I have not been able to concretely verify full coverage. In many cases, the program generates expressions with *exactly* the correct type.
- Direct voids. A very useful feature would be the ability to introduce void bindings intentionally. We incidentally test voids when shadowing.

On the generation side, it also has a some shortcomings:
- No RNG seeding, so you must save the entire program output to reproduce it
- Inefficient, multi-round tree generation. I reckon it can be done in one pass

That being said, I find that the program covers lots of common cases. As completely optional assignment code, written in lieu of PA4 this weekend, it is imperfect but workable ;)
