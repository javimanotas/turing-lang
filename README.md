# Turing Lang

A Haskell-based tool for emulating Turing Machines.

## Features

* Define Turing Machines using a custom, human-readable syntax
* Support for composition and parameterized machines
* Interactive execution and tape visualization (in progress)

## Usage

This project uses the [`cabal`](https://www.haskell.org/cabal/) build system.

To run a Turing Machine, use the following command:

```bash
cabal run turing-lang -- <path-to-machine-file> <initial-tape>
```

* `<path-to-machine-file>`: Path to the `.tm` file containing your Turing Machine definition.
* `<initial-tape>`: A string of valid tape symbols to initialize the machine with.

### Example

```bash
cabal run turing-lang -- examples/AddOne.tm B1011
```

## Learn More

Read the [Format Guide](/docs/format-guide.md) to learn how to write and structure your own Turing Machines.

## License

This project is licensed under the [MIT License](LICENSE).
Feel free to do whatever you want with it according to the terms of the license.

## Contributing

Contributions are welcome! Be sure to do so following the [Contributing](CONTRIBUTING) guidelines.
Please make sure to read and follow our [Code of Conduct](CODE_OF_CONDUCT.md) when contributing to this project.
