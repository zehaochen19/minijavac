# minijavac

MiniJava compiler course project

## Contributors

### 陈泽昊

- UID: 15307130273
- Contributions
  - Parsing
    - Lexical
    - Grammer
    - Error recovery
    - Error report
  - Static semantic checking
    - Type safety checking
    - Error report

### 王郑阳

## Dependency

**IMPORTANT**

Most parts of this project is developed using Haskell and managed with the Stack build tool.

The latest version of [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) should be installed in \$PATH if one would like to run demos.

## Demo

### Commind line minijavac frontend

```shell
# cd into mj-front
cd src/mj-front
# build the project
stack build
# run the frontend part of our compiler
# please replace your MiniJava source file and output name
# the `fix` flag is optional
stack exec mj-front-exe src.java output.json [fix]
# the result should be in `output.json`
```

### Test samples

We provide a total of 51 test examples to ensure the quality of our development process.

```shell
cd src/mj-front
stack test
```

### Web demo

Firstly, the backend server should be started

```shell
cd src/visualizer
stack build
stack exec visualizer-exe
```

Open your favorite browser and mount at <localhost:3000>

Drop your favorite MiniJava program to the box and view its graphic AST output
