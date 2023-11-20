# Chinese DAG

This project aims to use text mining (with the help of the open-source [CC-CEDICT](https://cc-cedict.org/wiki/)) on Mandarin texts to create a Directed Acyclic Graph (DAG) of Chinese characters. The DAG will represent the dependencies between characters, allowing learners to follow a logical and efficient path when learning new characters.

## Usage

First, build the program:

```
dune build @install
```

### Test

You can run the basic tests for this using the following command:

```
dune runtest
```

#### Expect tests

The in-line tests in this repo are expect tests, and are run as above. However, if the program evolves in its output, you can re-run the test and see if the changed output looks good. If so, then you can change the in-line test output to be new output using the following command:

```
dune promote
```

